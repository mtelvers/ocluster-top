(* Rendering: a pool table at the top, the selected pool's workers and queue below, then the message log. *)

open Notty
open Notty.Infix
open App

let bold = A.st A.bold
let dim = A.fg A.(gray 12)
let sel = A.st A.reverse
let red = A.fg A.lightred
let green = A.fg A.lightgreen
let yellow = A.fg A.lightyellow
let cyan = A.fg A.lightcyan

let clean s = String.map (fun c -> if Char.code c < 32 || c = '\127' then ' ' else c) s

(* exactly [w] columns *)
let text ?(attr = A.empty) w s = I.hsnap ~align:`Left (max 0 w) (I.string attr (clean s))
let rtext ?(attr = A.empty) w s = I.hsnap ~align:`Right (max 0 w) (I.string attr (clean s))
let hline w = I.uchar dim (Uchar.of_int 0x2500) (max 0 w) 1
let titled w title = I.string bold (" " ^ title ^ " ") </> hline w

let ago secs =
  let s = int_of_float secs in
  if s < 60 then Printf.sprintf "%ds" s else if s < 3600 then Printf.sprintf "%dm%02ds" (s / 60) (s mod 60) else Printf.sprintf "%dh%02dm" (s / 3600) (s mod 3600 / 60)

let clock f =
  let tm = Unix.localtime f in
  Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* columns: title, width, right-aligned *)
let row ?(attr = A.empty) cols cells =
  List.map2 (fun (_, w, right) c -> if right then rtext ~attr w c else text ~attr w c) cols cells
  |> List.mapi (fun k i -> if k = 0 then i else I.string attr " " <|> i)
  |> I.hcat

let header cols = row ~attr:dim cols (List.map (fun (t, _, _) -> t) cols)

(* Move [top] so that [sel] is among the [h] visible rows of [n]. *)
let scroll top sel h n =
  let top = if sel < top then sel else if sel >= top + h then sel - h + 1 else top in
  clamp 0 (max 0 (n - h)) top

let slice top h l = List.filteri (fun i _ -> i >= top && i < top + h) l

let state_of (r : wrow) =
  match r.detail with
  | `Registered { Show.state = Running _; _ } -> (green, "active")
  | `Registered { state = Inactive reasons; _ } -> (yellow, "inactive: " ^ reasons)
  | `Registered { state = Finished; _ } -> (dim, "finished")
  | `Disconnected true -> (red, "disconnected, paused")
  | `Disconnected false -> (red, "disconnected")

let keys_help =
  "q quit  r refresh  Tab pools\226\134\148workers  s sort  a all pools  p pause  u unpause  d drain  U update  f forget  R rate  c clients  ? help"

let help_lines =
  [
    "ocluster-top keys";
    "";
    "j/k, arrows      move the selection";
    "PgUp/PgDn, g/G   page, first, last";
    "Tab, Enter/Esc   switch between the pool list and the worker list";
    "r                refresh now        +/-  change the refresh interval";
    "s                sort pools: busy (running, then queued), queue, or name";
    "a                show every pool, or only those with workers";
    "p / u            pause / unpause the selected worker";
    "                 (from the pool list: every worker in the pool, after confirming)";
    "d                drain: pause and wait until no jobs are running";
    "U                update: drain, restart with the latest version, wait for it";
    "f                forget a (disconnected) worker";
    "R                set a client's expected rate for this pool";
    "c                list the registered clients";
    "q                quit";
    "";
    "press any key to close";
  ]

let overlay (w, h) base =
  let box = I.vcat (List.map (fun l -> I.string A.empty ("  " ^ l ^ "  ")) help_lines) in
  let box = I.hsnap ~align:`Left (I.width box) box in
  let bw = I.width box + 2 and bh = I.height box + 2 in
  let frame =
    I.tabulate bw bh (fun x y ->
        let c =
          if x = 0 && y = 0 then 0x250C
          else if x = bw - 1 && y = 0 then 0x2510
          else if x = 0 && y = bh - 1 then 0x2514
          else if x = bw - 1 && y = bh - 1 then 0x2518
          else if x = 0 || x = bw - 1 then 0x2502
          else if y = 0 || y = bh - 1 then 0x2500
          else 0x20
        in
        I.uchar cyan (Uchar.of_int c) 1 1)
  in
  let box = I.pad ~l:1 ~t:1 box </> frame in
  I.pad ~l:(max 0 ((w - bw) / 2)) ~t:(max 0 ((h - bh) / 2)) box </> base

let render (t : App.t) (w, h) =
  let pools_shown = visible t in
  let n = Array.length pools_shown in
  (* header *)
  let status =
    if t.refreshing then "refreshing..."
    else if t.last_refresh = 0. then "connecting..."
    else Printf.sprintf "refreshed %s ago, every %.0fs" (ago (now () -. t.last_refresh)) t.interval
  in
  let busy = if t.busy > 0 then Printf.sprintf "  %d action(s) running" t.busy else "" in
  let head =
    text ~attr:bold w
      (Printf.sprintf " ocluster-top  %s  %d of %d pools%s, by %s  %s%s" t.cap_path n (Array.length t.pools)
         (if t.show_empty then "" else " with workers") (sort_name t.sort) status busy)
  in
  (* pool table *)
  let name_w = Array.fold_left (fun m p -> max m (String.length p.name)) 10 pools_shown in
  let pcols =
    [ ("pool", name_w, false); ("cap", 5, true); ("queue", 6, true); ("workers", 7, true); ("running", 7, true); ("paused", 6, true); ("disc", 4, true); ("clients", 7, true) ]
  in
  let pool_h = min n (max 3 (h * 2 / 5)) in
  t.pool_top <- scroll t.pool_top t.sel_pool pool_h n;
  let prow i p =
    let attr = if i = t.sel_pool then if t.focus = Pools then sel else bold else A.empty in
    let cells, extra =
      match p.show with
      | Some s ->
          let s = (s : Show.t) in
          ( [ p.name; Printf.sprintf "%.0f" s.capacity; string_of_int (Show.queued s); string_of_int (List.length s.registered);
              string_of_int (Show.running s); string_of_int (Show.paused s); string_of_int (List.length s.disconnected);
              string_of_int (List.length s.clients) ],
            match p.error with Some e -> Some (red, "stale: " ^ e) | None -> None )
      | None -> ([ p.name; "?"; "?"; "?"; "?"; "?"; "?"; "?" ], match p.error with Some e -> Some (red, e) | None -> None)
    in
    let img = row ~attr pcols cells in
    let img = match extra with Some (a, e) -> img <|> I.string A.empty "  " <|> I.string a (clean e) | None -> img in
    I.hsnap ~align:`Left w img
  in
  let pools = I.vcat (header pcols :: List.init pool_h (fun k -> prow (t.pool_top + k) pools_shown.(t.pool_top + k))) in
  (* detail of the selected pool *)
  let log_h = 3 in
  let remaining = max 0 (h - 1 - I.height pools - 1 - 2 - log_h - 1) in
  let detail =
    match selected_pool t with
    | None -> I.vcat [ titled w "no pools"; text ~attr:dim w " (waiting for the scheduler)" ]
    | Some p ->
        let title = titled w p.name in
        let s = Option.value ~default:Show.empty p.show in
        let queue_summary =
          match s.queue with
          | Show.Backlog l -> Printf.sprintf "backlog: %d job(s) waiting" (List.length l)
          | Show.Ready ws -> Printf.sprintf "ready: %d worker(s) waiting for jobs" (List.length ws)
        in
        let summary =
          I.vcat
            [
              text w (Printf.sprintf " capacity %.0f   %s   %d job(s) assigned to workers" s.capacity queue_summary (Show.assigned s));
              text w (" clients: " ^ match s.clients with [] -> "none" | cs -> String.concat "  " (List.map Show.client_to_string cs));
            ]
        in
        let rows = worker_rows p in
        let nw = List.length rows in
        (* what the queue pane shows: the selected worker's own queue, or the pool backlog *)
        let qtitle, qitems =
          match (t.focus, selected_worker t) with
          | Workers, Some { wname; detail = `Registered { Show.state = Running (_ :: _ as q); _ } } ->
              (Printf.sprintf "queued on %s (%d)" wname (List.length q), q)
          | _ -> (
              match s.queue with
              | Show.Backlog l -> (Printf.sprintf "queue (%d)" (List.length l), l)
              | Show.Ready ws -> ("queue: ready, workers waiting: " ^ String.concat " " ws, []))
        in
        let nq = List.length qitems in
        let body = max 0 (remaining - 2) in
        let workers_h = if nq = 0 then min nw body else min nw (max 1 (body - min (nq + 1) (body / 2))) in
        let queue_h = max 0 (body - workers_h) in
        t.worker_top <- scroll t.worker_top t.sel_worker workers_h nw;
        let wcols = [ ("worker", max 10 (List.fold_left (fun m r -> max m (String.length r.wname)) 0 rows), false); ("workload", 8, true); ("running", 7, true); ("queued", 6, true); ("state", 30, false) ] in
        let wrow i (r : wrow) =
          let attr = if i = t.sel_worker && t.focus = Workers then sel else A.empty in
          let color, state = state_of r in
          let wl, run, q =
            match r.detail with
            | `Registered (x : Show.worker) -> (string_of_int x.workload, string_of_int x.running, match x.state with Running q -> string_of_int (List.length q) | _ -> "-")
            | `Disconnected _ -> ("-", "-", "-")
          in
          let img = row ~attr (List.filteri (fun i _ -> i < 4) wcols) [ r.wname; wl; run; q ] in
          I.hsnap ~align:`Left w (img <|> I.string attr " " <|> I.string (if attr = sel then sel else color) (clean state))
        in
        let workers =
          if nw = 0 then I.vcat [ header wcols; text ~attr:dim w " (no workers)" ]
          else I.vcat (header wcols :: List.mapi (fun k r -> wrow (t.worker_top + k) r) (slice t.worker_top workers_h rows))
        in
        let qcols = [ ("#", 3, true); ("client", 16, false); ("start", 6, true); ("cost", 4, true); ("hint", max 10 (w - 35), false) ] in
        let qrow i (tk : Show.ticket) =
          let attr = if tk.urgent then yellow else A.empty in
          row ~attr qcols [ string_of_int (i + 1); tk.client; tk.start; (match tk.cost with Some c -> string_of_int c | None -> ""); tk.hint ^ (if tk.urgent then " (urgent)" else "") ]
        in
        let queue =
          if nq = 0 then text ~attr:dim w (" " ^ qtitle)
          else
            let shown = slice 0 (max 0 (queue_h - 1)) qitems in
            let more = nq - List.length shown in
            I.vcat
              ((I.string dim (" " ^ qtitle) :: header qcols :: List.mapi qrow shown)
              @ if more > 0 then [ text ~attr:dim w (Printf.sprintf " ... and %d more" more) ] else [])
        in
        I.vcat [ title; summary; workers; queue ]
  in
  (* log and key line *)
  let log =
    let rec take n = function [] -> [] | x :: xs -> if n = 0 then [] else x :: take (n - 1) xs in
    let lines = List.rev (take log_h t.log) in
    let lines = List.map (fun (ts, m) -> I.string dim (clock ts ^ " ") <|> I.string A.empty (clean m)) lines in
    I.vsnap ~align:`Bottom log_h (I.vcat lines)
  in
  let bottom =
    match t.mode with
    | Normal | Help -> text ~attr:dim w (" " ^ keys_help)
    | Confirm (q, _) -> text ~attr:yellow w (" " ^ q ^ " [y/N]")
    | Input i -> text ~attr:cyan w (" " ^ i.prompt ^ i.buf ^ "\226\150\143")
  in
  let detail = I.vsnap ~align:`Top (max 0 (h - 1 - I.height pools - log_h - 1)) detail in
  let base = I.vcat [ head; pools; detail; log; bottom ] in
  let base = I.vsnap ~align:`Top h (I.hsnap ~align:`Left w base) in
  match t.mode with Help -> overlay (w, h) base | _ -> base
