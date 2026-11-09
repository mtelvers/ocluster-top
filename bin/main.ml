open Cmdliner

let main cap_path interval =
  match Cluster.load cap_path with
  | Error m -> `Error (false, Printf.sprintf "%s: %s" cap_path m)
  | Ok cluster ->
      Lwt_main.run (App.run cluster ~cap_path ~interval View.render);
      `Ok ()

let default_cap = Filename.concat (Option.value ~default:"." (Sys.getenv_opt "HOME")) "admin.cap"

let cap_path =
  Arg.value
  @@ Arg.opt Arg.string default_cap
  @@ Arg.info ~doc:"Path of the admin.cap file from ocluster-scheduler." ~docv:"ADDR" [ "c"; "connect" ]

let interval =
  Arg.value @@ Arg.opt Arg.float 5. @@ Arg.info ~doc:"Seconds between automatic refreshes." ~docv:"SECONDS" [ "i"; "interval" ]

let cmd =
  let doc = "a terminal UI for administering an ocluster-scheduler" in
  let man =
    [
      `S Manpage.s_description;
      `P "Connects to the scheduler's admin service over Cap'n Proto and shows every pool's capacity, queue, workers and clients, refreshing automatically.";
      `S "KEYS";
      `P "j/k or arrows move; Tab or Enter switches between the pool list and the worker list; r refreshes now.";
      `P "p pauses and u unpauses the selected worker (or, from the pool list, every worker in the pool, after confirmation); d drains; U updates; f forgets; R sets a client's rate; c lists registered clients; +/- change the refresh interval; ? shows help; q quits.";
    ]
  in
  Cmd.v (Cmd.info "ocluster-top" ~doc ~man) Term.(ret (const main $ cap_path $ interval))

let () = exit (Cmd.eval cmd)
