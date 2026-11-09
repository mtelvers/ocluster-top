(* Application state, key handling and the event loop. *)

open Lwt.Infix

type pool = {
  name : string;
  mutable show : Show.t option;
  mutable error : string option;
  mutable updated : float;  (** time of the last successful show, 0 if never *)
}

(* One line of the worker list for a pool: a registered worker, or a disconnected one. *)
type wrow = { wname : string; detail : [ `Registered of Show.worker | `Disconnected of bool (* paused *) ] }

type focus = Pools | Workers

type sort = Busy | Queue | Name

type mode =
  | Normal
  | Confirm of string * (unit -> unit)
  | Input of { prompt : string; mutable buf : string; submit : string -> unit }
  | Help

type event = Term of [ Notty.Unescape.event | `Resize of int * int ] | Tick | Redraw

type t = {
  cluster : Cluster.t;
  cap_path : string;
  mutable interval : float;
  mutable pools : pool array;
  mutable show_empty : bool;  (** list pools that have no workers at all *)
  mutable sort : sort;
  mutable sel_pool : int;  (** index into [visible] *)
  mutable sel_worker : int;
  mutable pool_top : int;  (** scroll offsets, kept between renders *)
  mutable worker_top : int;
  mutable focus : focus;
  mutable mode : mode;
  mutable log : (float * string) list;  (** newest first *)
  mutable refreshing : bool;
  mutable last_refresh : float;
  mutable busy : int;  (** actions in flight *)
  mutable quit : bool;
  push : event option -> unit;
}

let now = Unix.gettimeofday

let log t msg =
  let rec take n = function [] -> [] | x :: xs -> if n = 0 then [] else x :: take (n - 1) xs in
  t.log <- (now (), msg) :: take 200 t.log;
  t.push (Some Redraw)

let worker_rows p =
  match p.show with
  | None -> []
  | Some s ->
      List.map (fun (w : Show.worker) -> { wname = w.name; detail = `Registered w }) s.registered
      @ List.map (fun (n, paused) -> { wname = n; detail = `Disconnected paused }) s.disconnected

let clamp lo hi x = max lo (min hi x)

(* A pool is empty once we know it has neither registered nor disconnected workers. *)
let is_empty p = match p.show with Some { Show.registered = []; disconnected = []; _ } -> true | _ -> false

let sort_name = function Busy -> "busy" | Queue -> "queue" | Name -> "name"

(* Like top: the busiest pools first.  Unknown pools sort last. *)
let compare_pools sort a b =
  let key p = match p.show with Some s -> (Show.running s, Show.queued s) | None -> (-1, -1) in
  let (ra, qa), (rb, qb) = (key a, key b) in
  match sort with
  | Name -> compare a.name b.name
  | Busy -> (
      match compare rb ra with 0 -> ( match compare qb qa with 0 -> compare a.name b.name | c -> c) | c -> c)
  | Queue -> (
      match compare qb qa with 0 -> ( match compare rb ra with 0 -> compare a.name b.name | c -> c) | c -> c)

(* The pools listed on screen. *)
let visible t =
  let l = Array.to_list t.pools in
  let l = if t.show_empty then l else List.filter (fun p -> not (is_empty p)) l in
  Array.of_list (List.stable_sort (compare_pools t.sort) l)

let select_pool_by_name t name =
  Array.iteri (fun i p -> if p.name = name then t.sel_pool <- i) (visible t);
  t.sel_pool <- clamp 0 (max 0 (Array.length (visible t) - 1)) t.sel_pool

let selected_pool t =
  let v = visible t in
  if Array.length v = 0 then None else Some v.(min t.sel_pool (Array.length v - 1))
let selected_worker t = match selected_pool t with None -> None | Some p -> List.nth_opt (worker_rows p) t.sel_worker

let is_active (r : wrow) = match r.detail with `Registered { state = Running _; _ } -> true | `Registered _ -> false | `Disconnected paused -> not paused

(* ---- refreshing ---- *)

let refresh_pool t p =
  Cluster.show t.cluster p.name >|= function
  | Ok text ->
      p.show <- Some (Show.parse text);
      p.error <- None;
      p.updated <- now ()
  | Error e -> p.error <- Some e

let refresh t =
  if t.refreshing then Lwt.return_unit
  else begin
    t.refreshing <- true;
    t.push (Some Redraw);
    let sel_name = Option.map (fun p -> p.name) (selected_pool t) in
    let sel_worker = Option.map (fun r -> r.wname) (selected_worker t) in
    (Cluster.pools t.cluster >>= function
     | Error e ->
         log t ("refresh failed: " ^ e);
         Lwt.return_unit
     | Ok names ->
         let old = t.pools in
         let find n = Array.find_opt (fun p -> p.name = n) old in
         t.pools <- Array.of_list (List.map (fun n -> match find n with Some p -> p | None -> { name = n; show = None; error = None; updated = 0. }) names);
         Lwt_list.iter_p (refresh_pool t) (Array.to_list t.pools))
    >|= fun () ->
    (* The order and visibility of pools may have changed: keep the same pool and worker selected. *)
    Option.iter (select_pool_by_name t) sel_name;
    (match (selected_pool t, sel_worker) with
     | Some p, Some n -> List.iteri (fun i r -> if r.wname = n then t.sel_worker <- i) (worker_rows p)
     | _ -> ());
    t.refreshing <- false;
    t.last_refresh <- now ();
    t.push (Some Redraw)
  end

(* ---- actions ---- *)

(* Run an RPC in the background, logging its outcome and refreshing the pool it touched. *)
let action t ~pool label f =
  t.busy <- t.busy + 1;
  log t (label ^ " ...");
  Lwt.async (fun () ->
      f () >>= fun r ->
      t.busy <- t.busy - 1;
      (match r with Ok () -> log t (label ^ ": done") | Error e -> log t (label ^ ": " ^ e));
      refresh_pool t pool >|= fun () -> t.push (Some Redraw))

let set_active t p (r : wrow) active =
  let verb = if active then "unpause" else "pause" in
  if is_active r = active then log t (Printf.sprintf "%s is already %s" r.wname (if active then "active" else "paused"))
  else action t ~pool:p (Printf.sprintf "%s %s/%s" verb p.name r.wname) (fun () -> Cluster.set_active t.cluster p.name r.wname active)

let set_active_all t p active =
  let verb = if active then "unpause" else "pause" in
  let targets = List.filter (fun r -> is_active r <> active) (worker_rows p) in
  match targets with
  | [] -> log t (Printf.sprintf "%s: nothing to %s" p.name verb)
  | _ ->
      let names = String.concat ", " (List.map (fun r -> r.wname) targets) in
      t.mode <-
        Confirm
          ( Printf.sprintf "%s %d worker(s) in %s (%s)?" verb (List.length targets) p.name names,
            fun () -> List.iter (fun r -> set_active t p r active) targets )

let progress t p (r : wrow) msg = log t (Printf.sprintf "%s/%s: %s" p.name r.wname msg)

let drain t p (r : wrow) =
  action t ~pool:p (Printf.sprintf "drain %s/%s" p.name r.wname) (fun () -> Cluster.drain t.cluster p.name r.wname ~report:(progress t p r))

let update t p (r : wrow) =
  t.mode <-
    Confirm
      ( Printf.sprintf "Drain and restart %s/%s with the latest version?" p.name r.wname,
        fun () ->
          action t ~pool:p (Printf.sprintf "update %s/%s" p.name r.wname) (fun () ->
              Cluster.update t.cluster p.name r.wname ~report:(progress t p r)) )

let forget t p (r : wrow) =
  t.mode <-
    Confirm
      ( Printf.sprintf "Forget worker %s/%s?" p.name r.wname,
        fun () -> action t ~pool:p (Printf.sprintf "forget %s/%s" p.name r.wname) (fun () -> Cluster.forget t.cluster p.name r.wname) )

let set_rate t p =
  let clients = match p.show with Some s -> List.map (fun (c : Show.client) -> c.id) s.clients | None -> [] in
  let buf = match clients with [ c ] -> c ^ " " | _ -> "" in
  t.mode <-
    Input
      {
        prompt = Printf.sprintf "set-rate %s <client> <rate>: " p.name;
        buf;
        submit =
          (fun line ->
            match Show.words line with
            | [ client_id; rate ] -> (
                match float_of_string_opt rate with
                | Some rate ->
                    action t ~pool:p (Printf.sprintf "set-rate %s/%s %g" p.name client_id rate) (fun () ->
                        Cluster.set_rate t.cluster p.name ~client_id rate)
                | None -> log t ("set-rate: bad rate " ^ rate))
            | _ -> log t "set-rate: expected <client> <rate>");
      }

let list_clients t =
  Lwt.async (fun () ->
      Cluster.list_clients t.cluster >|= function
      | Ok [] -> log t "clients: none registered"
      | Ok cs -> log t ("clients: " ^ String.concat " " cs)
      | Error e -> log t ("list-clients: " ^ e))

(* ---- keys ---- *)

let move t d =
  match t.focus with
  | Pools ->
      t.sel_pool <- clamp 0 (max 0 (Array.length (visible t) - 1)) (t.sel_pool + d);
      t.sel_worker <- 0
  | Workers ->
      let n = match selected_pool t with Some p -> List.length (worker_rows p) | None -> 0 in
      t.sel_worker <- clamp 0 (max 0 (n - 1)) (t.sel_worker + d)

let with_worker t f =
  match (selected_pool t, selected_worker t) with
  | Some p, Some r -> f p r
  | Some _, None -> log t "no worker selected (Tab or Enter moves to the worker list)"
  | None, _ -> log t "no pool selected"

let normal_key t = function
  | `ASCII 'q' -> t.quit <- true
  | `ASCII 'r' -> Lwt.async (fun () -> refresh t)
  | `ASCII 'j' | `Arrow `Down -> move t 1
  | `ASCII 'k' | `Arrow `Up -> move t (-1)
  | `Page `Down -> move t 10
  | `Page `Up -> move t (-10)
  | `ASCII 'g' | `Home -> move t (-max_int / 2)
  | `ASCII 'G' | `End -> move t (max_int / 2)
  | `Tab -> t.focus <- (match t.focus with Pools -> Workers | Workers -> Pools)
  | `Enter | `Arrow `Right -> t.focus <- Workers
  | `Escape | `Arrow `Left -> t.focus <- Pools
  | `ASCII ('p' | 'u' as c) -> (
      let active = c = 'u' in
      match t.focus with
      | Workers -> with_worker t (fun p r -> set_active t p r active)
      | Pools -> ( match selected_pool t with Some p -> set_active_all t p active | None -> ()))
  | `ASCII 'd' -> with_worker t (drain t)
  | `ASCII 'U' -> with_worker t (update t)
  | `ASCII 'f' -> with_worker t (forget t)
  | `ASCII 'R' -> ( match selected_pool t with Some p -> set_rate t p | None -> ())
  | `ASCII 'c' -> list_clients t
  | `ASCII ('+' | '=') -> t.interval <- t.interval +. 1.
  | `ASCII '-' -> t.interval <- max 1. (t.interval -. 1.)
  | `ASCII '?' -> t.mode <- Help
  | `ASCII 'a' ->
      let sel = Option.map (fun p -> p.name) (selected_pool t) in
      t.show_empty <- not t.show_empty;
      Option.iter (select_pool_by_name t) sel
  | `ASCII 's' ->
      let sel = Option.map (fun p -> p.name) (selected_pool t) in
      t.sort <- (match t.sort with Busy -> Queue | Queue -> Name | Name -> Busy);
      Option.iter (select_pool_by_name t) sel
  | _ -> ()

let key t k =
  match t.mode with
  | Normal -> normal_key t k
  | Help -> t.mode <- Normal
  | Confirm (_, yes) -> (
      t.mode <- Normal;
      match k with `ASCII ('y' | 'Y') -> yes () | _ -> log t "cancelled")
  | Input i -> (
      match k with
      | `Escape -> t.mode <- Normal
      | `Enter ->
          t.mode <- Normal;
          i.submit i.buf
      | `Backspace -> if i.buf <> "" then i.buf <- String.sub i.buf 0 (String.length i.buf - 1)
      | `ASCII c when c >= ' ' -> i.buf <- i.buf ^ String.make 1 c
      | _ -> ())

(* ---- main loop ---- *)

let create cluster ~cap_path ~interval push =
  {
    cluster;
    cap_path;
    interval;
    pools = [||];
    show_empty = false;
    sort = Busy;
    sel_pool = 0;
    sel_worker = 0;
    pool_top = 0;
    worker_top = 0;
    focus = Pools;
    mode = Normal;
    log = [];
    refreshing = false;
    last_refresh = 0.;
    busy = 0;
    quit = false;
    push;
  }

(* Capnp-rpc logs over Logs; route warnings into the message log rather than the screen. *)
let install_logs t =
  let report src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Format.kasprintf
          (fun m ->
            log t (Printf.sprintf "%s [%s]: %s" (Logs.level_to_string (Some level)) (Logs.Src.name src) m);
            over ();
            k ())
          fmt)
  in
  Logs.set_reporter { Logs.report };
  Logs.set_level (Some Logs.Warning)

let run cluster ~cap_path ~interval render =
  let stream, push = Lwt_stream.create () in
  let t = create cluster ~cap_path ~interval push in
  install_logs t;
  Lwt.async_exception_hook := (fun ex -> log t ("internal error: " ^ Printexc.to_string ex));
  let term = Notty_lwt.Term.create ~mouse:false () in
  Lwt.async (fun () -> Lwt_stream.iter (fun e -> push (Some (Term e))) (Notty_lwt.Term.events term));
  let rec tick () = Lwt_unix.sleep 1. >>= fun () -> push (Some Tick); tick () in
  Lwt.async tick;
  Lwt.async (fun () -> refresh t);
  let rec loop () =
    if t.quit then Lwt.return_unit
    else
      Notty_lwt.Term.image term (render t (Notty_lwt.Term.size term)) >>= fun () ->
      Lwt_stream.next stream >>= fun ev ->
      (match ev with
       | Term (`Key (k, _)) -> key t k
       | Term _ -> ()
       | Tick -> if (not t.refreshing) && now () -. t.last_refresh >= t.interval then Lwt.async (fun () -> refresh t)
       | Redraw -> ());
      loop ()
  in
  Lwt.finalize loop (fun () -> Notty_lwt.Term.release term)
