# ocluster-top

A terminal UI for administering an
[OCluster](https://github.com/ocurrent/ocluster) scheduler.  It talks to
the scheduler over the same Cap'n Proto admin API that `ocluster-admin`
uses, so it needs nothing but an `admin.cap` file.

```
ocluster-top                      # uses ~/admin.cap
ocluster-top -c ./admin.cap -i 10 # another cap file, refresh every 10 seconds
```

The top of the screen is a table of pools with capacity, backlog length,
worker count, running jobs, paused workers, disconnected workers and
clients. Like `top`, the busiest pools (most running jobs, then longest
queue) come first, and pools with no workers at all are hidden unless
you press `a`. It refreshes itself every few seconds. Below it is the
selected pool: its workers (with workload, running jobs, jobs queued on
the worker and state) and either the pool's backlog or, when a worker
with its own queue is selected, that worker's queue.

## Keys

| key | action |
|-----|--------|
| `j`/`k`, arrows, PgUp/PgDn, `g`/`G` | move |
| Tab, Enter, Esc | switch between the pool list and the worker list |
| `r` | refresh now |
| `s` | sort pools by busy (running, then queued), by queue length, or by name |
| `a` | show every pool, or only those with workers |
| `p` / `u` | pause / unpause the selected worker; from the pool list, every worker in the pool (asks first) |
| `d` | drain: pause and wait until no jobs are running |
| `U` | update: drain, restart with the latest version and wait for it to return (asks first) |
| `f` | forget a worker (asks first) |
| `R` | set a client's rate for the pool |
| `c` | list the registered clients |
| `+` / `-` | change the refresh interval |
| `?` | help |
| `q` | quit |

Long actions (drain, update) run in the background and report progress in the log at the bottom.

## Building

```
day10 build .
day10 build --with-test . @runtest
```

Notty requires OCaml < 5.4, so `.day10` pins the compiler to 5.3.0.
