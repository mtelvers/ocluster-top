(lang dune 3.19)

(name ocluster-top)

(generate_opam_files true)

(authors "Mark Elvers <mark.elvers@tunbury.org>")

(maintainers "Mark Elvers <mark.elvers@tunbury.org>")

(license MIT)

(package
 (name ocluster-top)
 (synopsis "A terminal UI for administering an OCluster scheduler")
 (description "ocluster-top talks to an ocluster-scheduler over its Cap'n Proto admin API. It shows every pool's capacity, queue length, workers and clients on one screen, refreshing automatically, and lets you pause, unpause, drain, update and forget workers and set client rates from the keyboard.")
 (depends
  (ocaml (>= 5.0))
  notty
  lwt
  (capnp-rpc-lwt (and (>= 1.2.3) (< 2.0)))
  (capnp-rpc-unix (and (>= 1.2.3) (< 2.0)))
  (ocluster-api (>= 0.4.0))
  cmdliner
  logs
  fmt))
