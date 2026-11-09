(* The admin connection: a sturdy ref to the scheduler's Admin service, the
   live capability it resolves to, and one PoolAdmin capability per pool.
   Every call goes through [guard], which turns a failure into an error
   message and drops a broken connection so the next call reconnects. *)

open Lwt.Infix
open Capnp_rpc_lwt

type t = {
  sr : Cluster_api.Raw.Client.Admin.t Sturdy_ref.t;
  mutable admin : Cluster_api.Admin.t option;
  pools : (string, Cluster_api.Pool_admin.t) Hashtbl.t;
}

let load path =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match Capnp_rpc_unix.Cap_file.load vat path with
  | Ok sr -> Ok { sr; admin = None; pools = Hashtbl.create 32 }
  | Error (`Msg m) -> Error m

let drop_pools t =
  Hashtbl.iter (fun _ p -> Capability.dec_ref p) t.pools;
  Hashtbl.reset t.pools

let disconnect t =
  drop_pools t;
  Option.iter Capability.dec_ref t.admin;
  t.admin <- None

let admin t =
  match t.admin with
  | Some a when Capability.problem a = None -> Lwt.return a
  | _ ->
      disconnect t;
      Sturdy_ref.connect_exn t.sr >|= fun a ->
      t.admin <- Some a;
      a

let pool t name =
  admin t >|= fun admin ->
  let fresh () =
    let p = Cluster_api.Admin.pool admin name in
    Hashtbl.replace t.pools name p;
    p
  in
  match Hashtbl.find_opt t.pools name with
  | Some p when Capability.problem p = None -> p
  | Some p ->
      Capability.dec_ref p;
      fresh ()
  | None -> fresh ()

let error_message = function Failure m -> m | ex -> Printexc.to_string ex

let guard t f =
  Lwt.catch
    (fun () -> f () >|= Result.ok)
    (fun ex ->
      (match t.admin with Some a when Capability.problem a <> None -> disconnect t | _ -> ());
      Lwt.return_error (error_message ex))

let capnp_result = function
  | Ok () -> Lwt.return_unit
  | Error (`Capnp e) -> Lwt.fail (Failure (Fmt.str "%a" Capnp_rpc.Error.pp e))

let with_progress report f = Capability.with_ref (Cluster_api.Progress.local report) f

let pools t = guard t (fun () -> admin t >>= Cluster_api.Admin.pools)
let list_clients t = guard t (fun () -> admin t >>= Cluster_api.Admin.list_clients)
let show t name = guard t (fun () -> pool t name >>= Cluster_api.Pool_admin.show)

let set_active t name worker active =
  guard t (fun () -> pool t name >>= fun p -> Cluster_api.Pool_admin.set_active p worker active)

let drain t name worker ~report =
  guard t (fun () -> pool t name >>= fun p -> with_progress report (fun progress -> Cluster_api.Pool_admin.drain ~progress p worker))

let update t name worker ~report =
  guard t (fun () ->
      pool t name >>= fun p ->
      with_progress report (fun progress -> Cluster_api.Pool_admin.update ~progress p worker >>= capnp_result))

let forget t name worker = guard t (fun () -> pool t name >>= fun p -> Cluster_api.Pool_admin.forget p worker >>= capnp_result)

let set_rate t name ~client_id rate =
  guard t (fun () -> pool t name >>= fun p -> Cluster_api.Pool_admin.set_rate p ~client_id rate)
