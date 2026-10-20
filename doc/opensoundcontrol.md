# Open Sound Control (`src/osc.lisp`)

`src/osc.lisp` adds OSC 1.0 support alongside the raw-UDP layer in `src/udp.lisp`: same switch/server
shape (`*osc*` mirrors `*udp*`; `osc-input-server`/`osc-control-server`/`osc-output-server` mirror
`input-server`/`control-server`/`output-server`), but messages are OSC packets instead of raw text, so
a network talks directly to Pd ([oscparse]/[oscformat]), Max, or SuperCollider. No external library —
encoding and decoding are implemented directly in `src/osc.lisp`, the same "self-contained, portable"
spirit as the rest of the codebase.

## Contents

1. [Message format and the wire codec](#1-message-format-and-the-wire-codec)
2. [`osc-control-server`: remote control of any network](#2-osc-control-server-remote-control-of-any-network)
3. [`osc-input-server`: driving a network's input](#3-osc-input-server-driving-a-networks-input)
4. [Sending a network's output](#4-sending-a-networks-output)
5. [Running a server: threads, and stopping them](#5-running-a-server-threads-and-stopping-them)
6. [Toward Pd](#6-toward-pd)

---

## 1. Message format and the wire codec

An OSC message is represented as a plain list, address first: `("/neuromuse/note" 60 100)`. On
**send**, each argument's Lisp type picks its OSC type tag — `integer` → `i` (int32), any other `real`
→ `f` (float32), `string` → `s`. On **receive**, the same three plus the boolean/nil tags `T`/`F`/`N`
(read back as `t`/`nil`/`nil`). No bundles.

```lisp
(osc-encode '("/neuromuse/note" 60 100))   ;; -> a (vector (unsigned-byte 8)), the raw packet
(osc-decode *)                              ;; -> ("/neuromuse/note" 60 100), round-trips
```

`osc-encode`/`osc-decode` are pure functions — useful on their own to check a packet without opening a
socket. `osc-numbers` extracts just the numeric arguments of a message (ignores the address and any
string arguments), the same convention `udp.lisp` uses for `input-server`.

Two thin wrappers over `sb-bsd-sockets`, the OSC equivalents of `udp.lisp`'s `send-udp`/`receive-udp`:

```lisp
(send-osc '("/neuromuse/note" 60 100) "127.0.0.1" 9001)
(receive-osc 9002)       ;; prints every message received on 9002 while *osc* is true; (osc) stops it
```

`receive-osc` is a quick way to check what's actually arriving on a port before wiring a real server —
run it at the REPL, send something from Pd/Max, see it printed.

## 2. `osc-control-server`: remote control — and triggering — of any network

```lisp
#+sbcl (defmethod osc-control-server ((ann ann) (port number) (lenght number)) ...)
```

One method on `ann` itself, so it works for *every* architecture — including one that lives outside
`:neuromuse` entirely, like `fred/transformer.lisp`'s `transformer` class, since all it needs is a
`setf`-able accessor or an ordinary function. The name — a parameter to set, or an action to trigger —
is the **last segment of the OSC address** — the usual OSC convention, and what most
controllers/patches naturally produce:

```lisp
("/neuromuse/control/learn-fact" 0.1)   ;; -> (setf (learn-fact ann) 0.1)
```

Falls back to an older convention, name as the first argument instead, when the address's last
segment doesn't resolve (e.g. a generic address shared by several parameters/actions):

```lisp
("/neuromuse/control" "learn-fact" 0.1)   ;; -> (setf (learn-fact ann) 0.1), same result
```

`osc-control` resolves the name to a symbol via `find-symbol` in `:neuromuse`, then:

- if it names a `setf`-able accessor, sets that slot (`learn-fact`, `temp`, `net-temp`, `threshold`,
  `radius`... any slot, not a fixed list) — a continuous *parameter*;
- otherwise, if it names an ordinary function, calls it directly with `ann` and whatever arguments
  follow — a one-shot *action*, e.g. a generation step defined on the network's own class:

```lisp
("/neuromuse/control/generate" 32)       ;; -> (generate ann 32)
("/neuromuse/control" "generate" 32)     ;; -> (generate ann 32), same result
```

— so the exact same server/thread that adjusts `learn-fact` live can also kick off a trained network's
generation (whatever that network calls it — `generate`, `play-events-osc`, its own free-generation
function...), from a plain OSC message, with no extra server to run. `osc-control` `warn`s only when
the resolved name is neither a `setf`-able accessor nor a callable function (unknown name, in either
form).

```lisp
(setf (superdaemon acdll-transformer) t
      (verbose acdll-transformer) t)        ;; optional: prints each message as it arrives
(start-daemon acdll-transformer "acdll-transformer-osc-control"
              (lambda () (osc-control-server acdll-transformer 9002 1024)))
```

## 3. `osc-input-server`: driving a network's input

```lisp
#+sbcl (defmethod osc-input-server ((ann som) (port number) (lenght number)) ...)
#+sbcl (defmethod osc-input-server ((ann mlp) (port number) (lenght number)) ...)
```

Only `som` and `mlp` have a method today — the same scope as `udp.lisp`'s `input-server`. The numeric
arguments of each received message become `(input ann)`; for a `som` this also computes and stores the
winner and its activation (`(setf (winner ann) (find-winner ann))`, then `activation`); for an `mlp` it
triggers `run-mlp`. Other architectures (`transformer`, `perceptron`, `hopfield`, `auto-assoc`,
`rosom`) don't have a method yet — add one the same way if needed, following `udp.lisp`'s
`input-server` for the pattern.

## 4. Sending a network's output

Two levels, matching the rest of this library's I/O: a generic periodic server for the common case,
and the lower-level primitive (`send-osc`, §1) for anything that needs its own timing.

```lisp
#+sbcl (defmethod osc-output-server ((ann som) (ip string) (port number) &optional address) ...)
#+sbcl (defmethod osc-output-server ((ann mlp) (ip string) (port number) &optional address) ...)
```

Mirrors `udp.lisp`'s `output-server`: while `(superdaemon ann)` is true, sends `(output ann)` as the
arguments of an OSC message at `address` (default `"/neuromuse/output"`), paced by `(latence ann)`.
Same scope as `osc-input-server` — `som`/`mlp` only.

```lisp
(setf (superdaemon xor) t (latence xor) 0.05)
(start-daemon xor "xor-osc-output" (lambda () (osc-output-server xor "127.0.0.1" 9001)))
```

For anything that doesn't fit "push the current output every `latence` seconds" — scheduling discrete
note-on/note-off events from a trained network's predictions, for instance — call `send-osc` directly
instead of going through `osc-output-server`. That's how `fred/acdll/examples/acdll-transformer.lisp`'s
`play-events-osc` drives Pd in real time: it decodes each predicted frame into notes and calls
`send-osc` itself for each note-on/note-off, timed against a tempo, rather than streaming the raw
`(output ann)` vector on a fixed clock.

## 5. Running a server: threads, and stopping them

None of the three servers return on their own — each loops until `(superdaemon ann)` goes false, so
each needs its own thread. `mk-process` (`src/neuromuse-main.lisp`) wraps `sb-thread:make-thread`:

```lisp
(setf (superdaemon ann) t)   ;; required -- see the warning below if forgotten
(mk-process "a-name" (lambda () (osc-control-server ann port lenght)))
```

**Silent trap**: each server's loop is `(loop while (superdaemon ann) do ...)` — tested *before*
listening/sending anything. Launch one while `(superdaemon ann)` is still `NIL` (the default on a
fresh network, and easy to forget after restarting a Lisp image) and the thread exits immediately,
with no error at all: `mk-process` returns a perfectly normal-looking thread object, and a moment
later it's just gone (`(sb-thread:list-all-threads)` won't show it). All five server methods call
`warn-unless-superdaemon` first, so this now prints a `WARN` instead of failing silently — but the fix
is still to set `(superdaemon ann) t` *before* starting the thread, not just reading the warning.

Stopping: `(setf (superdaemon ann) nil)` is checked only *between* iterations. An output server
notices at its next `sleep`-paced wakeup — quick. A control or input server is blocked on
`socket-receive`, so it won't notice until the next packet actually arrives, which may be never. For
an immediate stop, either send one more (even a throwaway) packet to unblock it, or keep the thread
object `mk-process` returns and kill it directly with `sb-thread:terminate-thread` — the same thing
`fred/acdll/examples/acdll-transformer.lisp`'s `stop-osc` does for a scheduled `play-events-osc` job
(`*osc-player*`, joined rather than killed there, since that loop isn't blocked on a socket).

## 6. Toward Pd

`src/osc.lisp`'s own header comment names the two Pd objects this is meant to pair with:
**[oscparse]** (packet → OSC message, on the receiving end) and **[oscformat]** (OSC message → packet,
on the sending end) — both built into recent Pd releases, no extra library needed on the Pd side
either. The exact patch wiring depends on the Pd version/externals in use; conceptually:

**Pd receiving what neuromuse sends** (`send-osc`/`osc-output-server`/`play-events-osc`, §4) —
`udpreceive` on the port neuromuse sends to, `oscparse` to turn the packet back into a typed OSC
message, then route on the address:

```
[udpreceive 9001]
      |
 [oscparse]
      |
 [route /neuromuse/note /neuromuse/section]
```

**Pd sending control to neuromuse** (`osc-control-server`, §2) — put the slot name (or the name of an
action to trigger, e.g. a generation function) at the end of the address and the value/arguments as
the message's contents, format it as a packet, and send it to the port the control server is
listening on:

```
[... 0.1(                           (a message box, or anything producing that value)
      |
 [oscformat /neuromuse/control/learn-fact]
      |
 [udpsend 127.0.0.1 9002]
```

Same shape to trigger an action instead of setting a parameter — e.g. a bang driving a 32-step
generation:

```
[bang(  ->  [32(
             |
       [oscformat /neuromuse/control/generate]
             |
       [udpsend 127.0.0.1 9002]
```

Same shape for `osc-input-server` (§3), sending numeric arguments at whatever address `osc-numbers`
should extract from — the address itself is never inspected, so any address works.
