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

## 2. `osc-control-server`: remote control of any network

```lisp
#+sbcl (defmethod osc-control-server ((ann ann) (port number) (lenght number)) ...)
```

One method on `ann` itself, so it works for *every* architecture — including one that lives outside
`:neuromuse` entirely, like `fred/transformer.lisp`'s `transformer` class, since all it needs is a
`setf`-able accessor. A control message is `(address "slot" value)` — the address is never read:

```lisp
("/neuromuse/control" "learn-fact" 0.1)   ;; -> (setf (learn-fact ann) 0.1)
```

`osc-control` resolves `"learn-fact"` to the symbol `NEUROMUSE::LEARN-FACT` via `find-symbol` and calls
its `setf` function if one exists, or `warn`s ("OSC control : pas d'accesseur ...") if it doesn't —
works for any slot with a `setf`-able accessor (`learn-fact`, `temp`, `net-temp`, `threshold`,
`radius`...), not a fixed list.

```lisp
(setf (superdaemon acdll-transformer) t
      (verbose acdll-transformer) t)        ;; optional: prints each message as it arrives
(mk-process "acdll-transformer-osc-control"
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
(mk-process "xor-osc-output" (lambda () (osc-output-server xor "127.0.0.1" 9001)))
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
(mk-process "a-name" (lambda () (osc-control-server ann port lenght)))
```

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

**Pd sending control to neuromuse** (`osc-control-server`, §2) — build an OSC message whose second
and third arguments are the slot name and the new value, format it as a packet, and send it to the
port the control server is listening on:

```
[... learn-fact 0.1(   (a message box, or anything producing that list)
      |
 [oscformat /neuromuse/control]
      |
 [udpsend 127.0.0.1 9002]
```

Same shape for `osc-input-server` (§3), sending numeric arguments at whatever address `osc-numbers`
should extract from — the address itself is never inspected, so any address works.
