let pool_sample =
  {|capacity: 364
queue: (ready) [clete.caelum.ci.dev odawa.caelum.ci.dev asteria.caelum.ci.dev
       laodoke.caelum.ci.dev phoebe.caelum.ci.dev toxis.caelum.ci.dev
       bremusa.ocamllabs.io]
registered:
  asteria.caelum.ci.dev (0): [] (1 running)
  bremusa.ocamllabs.io (0): [] (0 running)
  eumache.caelum.ci.dev (0): (inactive: worker pause) (0 running)
  marpe.cl.cam.ac.uk (0): (inactive: admin pause) (0 running)
  toxis.caelum.ci.dev (0): [] (4 running)
disconnected:
  doris.caelum.ci.dev
  iphito.caelum.ci.dev (paused)
clients: avsm(1) base-image-builder(1) mirage-ci(1) mirage-deployer(1)
         ocaml-ci(900) ocurrent-deployer(1) opam-health-check(1)+364145m
         opam-repo-ci(1000)
|}

let backlog_sample =
  {|capacity: 10
queue: (backlog) [u1:job1@0s u1:job3@12s+urgent opam-repo-ci:(no cache hint)@3m(2)
       u2:some-hint@-5s(2+urgent)]
registered:
  worker-1 (6): [u1:job2@10s(2) u1:job6@24s(2+urgent) u1:job4@22s(2+urgent)] (1 running)
  worker-2 (10): [u1:job2@5s(10)] (0 running)
  worker-3 (0): (finished) (0 running)
disconnected:
clients: u1(1) u2:FINISHED
|}

let empty_sample = {|capacity: 0
queue: (backlog) []
registered:
disconnected:
clients:
|}

let check name b = if not b then (Printf.printf "FAIL: %s\n" name; exit 1) else Printf.printf "ok: %s\n" name

let () =
  let s = Show.parse pool_sample in
  check "capacity" (s.capacity = 364.);
  check "ready list" (s.queue = Show.Ready [ "clete.caelum.ci.dev"; "odawa.caelum.ci.dev"; "asteria.caelum.ci.dev"; "laodoke.caelum.ci.dev"; "phoebe.caelum.ci.dev"; "toxis.caelum.ci.dev"; "bremusa.ocamllabs.io" ]);
  check "registered count" (List.length s.registered = 5);
  check "running sum" (Show.running s = 5);
  check "inactive reasons" (List.exists (fun (w : Show.worker) -> w.name = "marpe.cl.cam.ac.uk" && w.state = Show.Inactive "admin pause") s.registered);
  check "disconnected" (s.disconnected = [ ("doris.caelum.ci.dev", false); ("iphito.caelum.ci.dev", true) ]);
  check "paused" (Show.paused s = 3);
  check "clients wrapped" (List.length s.clients = 8);
  check "client delay" (List.exists (fun (c : Show.client) -> c.id = "opam-health-check" && c.rate = Some 1. && c.delay = Some "364145m") s.clients);
  check "client rate" (List.exists (fun (c : Show.client) -> c.id = "opam-repo-ci" && c.rate = Some 1000. && c.delay = None) s.clients);
  let b = Show.parse backlog_sample in
  check "backlog count" (Show.queued b = 4);
  let q = Show.backlog b in
  check "ticket plain" (List.nth q 0 = { Show.client = "u1"; hint = "job1"; start = "0s"; cost = None; urgent = false });
  check "ticket urgent" (List.nth q 1 = { Show.client = "u1"; hint = "job3"; start = "12s"; cost = None; urgent = true });
  check "ticket no hint" (List.nth q 2 = { Show.client = "opam-repo-ci"; hint = "(no cache hint)"; start = "3m"; cost = Some 2; urgent = false });
  check "ticket cost urgent" (List.nth q 3 = { Show.client = "u2"; hint = "some-hint"; start = "-5s"; cost = Some 2; urgent = true });
  check "worker queue" (match List.nth b.registered 0 with { Show.name = "worker-1"; workload = 6; running = 1; state = Running l } -> List.length l = 3 | _ -> false);
  check "assigned" (Show.assigned b = 4);
  check "finished" (match List.nth b.registered 2 with { Show.state = Finished; _ } -> true | _ -> false);
  check "finished client" (List.exists (fun (c : Show.client) -> c.id = "u2" && c.finished) b.clients);
  let e = Show.parse empty_sample in
  check "empty" (e = { Show.empty with capacity = 0. });
  print_endline "all show tests passed"
