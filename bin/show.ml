(* Parse the text returned by PoolAdmin.show.  The scheduler prints it with
   Fmt boxes, so long lists wrap onto indented continuation lines.  The shape is:

     capacity: 364
     queue: (backlog) [client:hint@12s(2+urgent) ...]     or   (ready) [worker ...]
     registered:
       name (workload): [tickets] (N running)
       name (workload): (inactive: admin pause) (N running)
     disconnected:
       name (paused)
     clients: id(rate)+delay id:FINISHED ...
*)

type ticket = {
  client : string;
  hint : string;  (** cache hint, or "(no cache hint)" *)
  start : string;  (** rough time until the fair start time, e.g. "12s" or "3m" *)
  cost : int option;
  urgent : bool;
}

type worker_state =
  | Running of ticket list  (** connected and active; the tickets queued on it *)
  | Inactive of string  (** the reasons, e.g. "admin pause" *)
  | Finished

type worker = { name : string; workload : int; state : worker_state; running : int }

type client = {
  id : string;
  rate : float option;
  delay : string option;  (** time until this client's next fair start, if it is in the future *)
  finished : bool;
}

type queue =
  | Backlog of ticket list  (** jobs waiting for a worker *)
  | Ready of string list  (** workers waiting for a job *)

type t = {
  capacity : float;
  queue : queue;
  registered : worker list;
  disconnected : (string * bool) list;  (** name, paused *)
  clients : client list;
}

let empty = { capacity = 0.; queue = Backlog []; registered = []; disconnected = []; clients = [] }

(* ---- small string helpers ---- *)

let starts_with ~prefix s = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let ls = String.length s and lx = String.length suffix in
  ls >= lx && String.sub s (ls - lx) lx = suffix

let chop_suffix ~suffix s = if ends_with ~suffix s then Some (String.sub s 0 (String.length s - String.length suffix)) else None

let after s i = String.sub s i (String.length s - i)

(* Split [s] at the last occurrence of [c]. *)
let cut_last c s = match String.rindex_opt s c with Some i -> Some (String.sub s 0 i, after s (i + 1)) | None -> None

let cut_first c s = match String.index_opt s c with Some i -> Some (String.sub s 0 i, after s (i + 1)) | None -> None

let words s = String.split_on_char ' ' s |> List.filter (fun w -> w <> "")

let indent s =
  let n = String.length s in
  let rec go i = if i < n && s.[i] = ' ' then go (i + 1) else i in
  go 0

(* ---- sections ---- *)

type section = { header : string; inline : string; body : string list (* raw, still indented *) }

(* Headers sit in column 0; every indented line belongs to the section above it. *)
let sections text =
  let lines = String.split_on_char '\n' text |> List.filter (fun l -> String.trim l <> "") in
  let finish = function None -> [] | Some s -> [ { s with body = List.rev s.body } ] in
  let rec go acc cur = function
    | [] -> List.rev (finish cur @ acc)
    | l :: rest when indent l > 0 -> (
        match cur with Some s -> go acc (Some { s with body = l :: s.body }) rest | None -> go acc None rest)
    | l :: rest ->
        let header, inline = match cut_first ':' l with Some (h, r) -> (h, String.trim r) | None -> (l, "") in
        go (finish cur @ acc) (Some { header; inline; body = [] }) rest
  in
  go [] None lines

let section name secs = List.find_opt (fun s -> s.header = name) secs

(* The whole text of a section on one line. *)
let flat s = String.concat " " (s.inline :: List.map String.trim s.body)

(* Entries of a list section: a line indented by exactly two spaces starts an entry; deeper lines continue it. *)
let entries s =
  let rec go acc = function
    | [] -> List.rev acc
    | l :: rest when indent l <= 2 -> go (String.trim l :: acc) rest
    | l :: rest -> ( match acc with e :: acc -> go ((e ^ " " ^ String.trim l) :: acc) rest | [] -> go [ String.trim l ] rest)
  in
  go [] s.body

(* ---- tickets ---- *)

let no_hint = "(no cache hint)"
let no_hint_token = "\001NOHINT\001"

let bracketed s =
  match (String.index_opt s '[', String.rindex_opt s ']') with
  | Some i, Some j when j > i -> String.sub s (i + 1) (j - i - 1)
  | _ -> ""

(* Replace every occurrence of [sub] in [s] with [by]. *)
let replace ~sub ~by s =
  let ls = String.length s and lx = String.length sub in
  let b = Buffer.create ls in
  let rec go i =
    if i > ls - lx then Buffer.add_string b (after s i)
    else if String.sub s i lx = sub then (
      Buffer.add_string b by;
      go (i + lx))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* client:hint@start, then optionally (cost), (cost+urgent) or +urgent *)
let ticket raw =
  let raw = replace ~sub:no_hint_token ~by:no_hint raw in
  let body, cost, urgent =
    if ends_with ~suffix:")" raw then
      match cut_last '(' raw with
      | Some (body, inside) ->
          let inside = String.sub inside 0 (String.length inside - 1) in
          let inside, urgent = match chop_suffix ~suffix:"+urgent" inside with Some c -> (c, true) | None -> (inside, false) in
          (body, int_of_string_opt inside, urgent)
      | None -> (raw, None, false)
    else match chop_suffix ~suffix:"+urgent" raw with Some body -> (body, None, true) | None -> (raw, None, false)
  in
  let body, start = match cut_last '@' body with Some (b, s) -> (b, s) | None -> (body, "") in
  let client, hint = match cut_first ':' body with Some (c, h) -> (c, h) | None -> ("", body) in
  { client; hint; start; cost; urgent }

let tickets s = bracketed (replace ~sub:no_hint ~by:no_hint_token s) |> words |> List.map ticket

(* ---- workers ---- *)

(* "name (workload): STATE (N running)" *)
let worker line =
  match cut_first '(' line with
  | None -> None
  | Some (name, rest) -> (
      match cut_first ')' rest with
      | None -> None
      | Some (workload, rest) ->
          let rest = String.trim rest in
          let rest = if starts_with ~prefix:":" rest then String.trim (after rest 1) else rest in
          let state_text, running =
            match cut_last '(' rest with
            | Some (st, r) when ends_with ~suffix:"running)" r -> (String.trim st, Option.value ~default:0 (int_of_string_opt (List.hd (words r))))
            | _ -> (rest, 0)
          in
          let state =
            if starts_with ~prefix:"[" state_text then Running (tickets state_text)
            else if starts_with ~prefix:"(inactive:" state_text then
              Inactive (String.trim (String.sub state_text 10 (String.length state_text - 11)))
            else if state_text = "(finished)" then Finished
            else Inactive state_text
          in
          Some { name = String.trim name; workload = Option.value ~default:0 (int_of_string_opt (String.trim workload)); state; running })

(* "name" or "name (paused)" *)
let disconnected line =
  match chop_suffix ~suffix:"(paused)" line with Some n -> (String.trim n, true) | None -> (String.trim line, false)

(* ---- clients ---- *)

(* "id(rate)", "id(rate)+61m" or "id:FINISHED" *)
let client tok =
  match chop_suffix ~suffix:":FINISHED" tok with
  | Some id -> { id; rate = None; delay = None; finished = true }
  | None -> (
      match cut_first '(' tok with
      | None -> { id = tok; rate = None; delay = None; finished = false }
      | Some (id, rest) -> (
          match cut_first ')' rest with
          | None -> { id; rate = None; delay = None; finished = false }
          | Some (rate, tail) ->
              let delay = if tail = "" then None else Some tail in
              let delay = Option.map (fun d -> if starts_with ~prefix:"+" d then after d 1 else d) delay in
              { id; rate = float_of_string_opt rate; delay; finished = false }))

(* ---- whole dump ---- *)

let parse text =
  let secs = sections text in
  let capacity = match section "capacity" secs with Some s -> Option.value ~default:0. (float_of_string_opt (String.trim s.inline)) | None -> 0. in
  let queue =
    match section "queue" secs with
    | None -> Backlog []
    | Some s ->
        let f = flat s in
        if starts_with ~prefix:"(ready)" f then Ready (words (bracketed f)) else Backlog (tickets f)
  in
  let registered = match section "registered" secs with Some s -> List.filter_map worker (entries s) | None -> [] in
  let disconnected = match section "disconnected" secs with Some s -> List.map disconnected (entries s) | None -> [] in
  let clients = match section "clients" secs with Some s -> List.map client (words (flat s)) | None -> [] in
  { capacity; queue; registered; disconnected; clients }

(* ---- derived figures ---- *)

let backlog t = match t.queue with Backlog l -> l | Ready _ -> []
let queued t = List.length (backlog t)
let running t = List.fold_left (fun n (w : worker) -> n + w.running) 0 t.registered
let paused t =
  List.length (List.filter (fun (w : worker) -> match w.state with Running _ -> false | _ -> true) t.registered)
  + List.length (List.filter snd t.disconnected)

(* Jobs sitting in the queues of individual workers (assigned but not started). *)
let assigned t =
  List.fold_left (fun n (w : worker) -> match w.state with Running q -> n + List.length q | _ -> n) 0 t.registered

let client_to_string (c : client) =
  if c.finished then c.id ^ ":FINISHED"
  else
    c.id
    ^ (match c.rate with Some r -> Printf.sprintf "(%.0f)" r | None -> "")
    ^ match c.delay with Some d -> "+" ^ d | None -> ""
