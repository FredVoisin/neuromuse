# Auto-Assoc Examples: Auto-Associative Memory with neuromuse

An auto-associative memory learns to reproduce/associate each training pattern to itself — the same
idea as a modern auto-encoder, just a single linear layer here, no bottleneck. See
[EXAMPLES.md](EXAMPLES.md) for the general, architecture-agnostic workflow and Tips & Tricks, and
[SOM-exemples.md](SOM-exemples.md)/[MLP-exemples.md](MLP-exemples.md) for the other architectures.

## 1. Building an auto-assoc

```lisp
(make-auto-assoc my-net 9)   ;; 9 cells
```

Unlike `mlp`/`som`, `net` here is a plain CL 2D array (`in-size x in-size`), symmetric: `(aref net i j)`
always equals `(aref net j i)`. `make-auto-assoc` initializes it to small random weights, matching the
class's own `make-X` convention (`make-perceptron`, `make-mlp`, `make-hopfield`).

```lisp
(setf (learn-fact my-net) .1)   ;; defaults to 0.0, like every other architecture
```

## 2. Training

One learning step is `(learn my-net)`: present a pattern via `(setf (input my-net) ...)`, then call
`learn` — Widrow-Hoff, same rule as `perceptron`. Returns the instance itself, like every other `learn`.

```lisp
(dotimes (epoch 100)
  (setf (input my-net) #(1 0 0 0 0 0 0 0 1)) (learn my-net)
  (setf (input my-net) #(0 1 0 0 0 0 0 1 0)) (learn my-net))
```

## 3. Recall

`run-auto-assoc` (not `run-aa` — that was the name of the 2001 code this was ported from; renamed to
match `run-mlp`/`run-perceptron`'s convention) runs a pattern through the learned weights and writes
the reconstruction to `(output my-net)`:

```lisp
(run-auto-assoc my-net :in #(1 0 0 0 0 0 0 0 1))
;; => (0.998 -0.997 -0.995 -0.996 -0.996 -0.998 -0.993 -0.999 0.997)
```

Values are bipolar (`tanh`, close to ±1), not 0/1 — a trained pattern comes back close to its own
bipolar encoding. The useful property is *completion*: present a partial/noisy cue (some bits missing
or flipped) and the network tends to settle back onto the closest trained pattern:

```lisp
(run-auto-assoc my-net :in #(1 0 0 0 0 0 0 0 0))  ;; missing the second 1
;; => still close to (+1 -1 -1 -1 -1 -1 -1 -1 +1), the full trained pattern
```

## 4. A worked example: choreographic pattern analysis, *L'Écarlate* (2001)

`auto-assoc` isn't a new addition — it's a port of code first used live, on stage, in 2001. This is
worth knowing before using it: it's not a toy architecture here, it has a working history.

*L'Écarlate*, a piece by Kasper T. Toeplitz and Myriam Gourfink, premiered 7 June 2001 at Ircam (Agora
festival). The dancer's movement was analyzed in real time by a separate computer running
[LabanOnLisp](https://github.com/FredVoisin/LabanOnLisp) — Fred's own implementation of Laban movement
notation — and streamed over MIDI to the neuromuse machine: angle, speed, duration, amplitude, *écart*
(deviation), *support*, one dimension per MIDI controller on a dedicated channel.

Each "moment" of dance was assembled into a 180-element vector (10 time-slices × 18 dimensions — hence
the network's name, `ecarl180`, a 180-cell `auto-assoc`) and run through it:

```lisp
(run-aa moment-vector ecarl180-1)   ;; 2001 name; today, (run-auto-assoc ecarl180-1 :in moment-vector)
```

The network had been trained beforehand on a set of reference **situations** — Gourfink's own term for
the specific bodily configurations/states her notation works with, as distinct from a fixed pose — so
at performance time it was only *run*, never retrained live. What was actually watched, continuously,
was the *distance* between each live moment and the network's reconstruction of it: how far the
dancer's current movement sat from the learned situations, turned into a musical parameter. The
original code (`get-moment`, `legacy/neuromuse-2001-concert/getmidi.lisp`) is explicit about this:

```lisp
(setf activation (run-aa (car (last *moment-vectors*)) ecarl180-1))
(setf *distances* (append *distances* (list (distance activation moment-vector))))
(format t "~%> ecarl180 : distance = ~S." (car (last *distances*)))
```

— the same reconstruction-error idea `(euclidian input output)` computes today, and what
[section 5](#5-watching-it-live-the-auto-assoc-viewer) below displays as a live curve.

## 5. Watching it live: the auto-assoc viewer

`neuromuse-gui` has an `auto-assoc`-specific window (`src/gui.lisp`), parallel to the SOM one but
simpler: the whole grid *is* `(output my-net)` — the full reconstruction, cell by cell — so there's no
separate input row or output row to show alongside it, and no "winner" (auto-assoc isn't competitive).
Below the grid, a curve traces the reconstruction distance (`euclidian` between `(input my-net)` and
`(output my-net)`) over time — the same signal `*distances*` was, in 2001.

```lisp
(ql:quickload :ltk)                  ; once per session; needs Tk installed (Debian: apt install tk)
(asdf:load-system "neuromuse/gui")   ; note the string, not a keyword

(neuromuse-gui:gui 'my-net)          ; dispatches to the auto-assoc viewer automatically
```

The window only *reads* `(output my-net)` — it never calls `run-auto-assoc` itself, to avoid racing a
training/running loop in another thread. That means your own loop has to call `run-auto-assoc` from
time to time for the window to have anything fresh to show, exactly how `get-moment` did in 2001:

```lisp
(dolist (moment movements)
  (setf (input my-net) moment)
  (run-auto-assoc my-net))   ;; writes (output my-net); the window picks it up on its next tick
```

## Further Reading

- See `src/auto-assoc.lisp` for the full implementation and docstrings.
- See `legacy/neuromuse-2001-concert/` for the original 2001 code (`"ANN 1.4.lisp"`, `getmidi.lisp`,
  `ecarl180-1.lisp`) and its own README.
- See [LabanOnLisp](https://github.com/FredVoisin/LabanOnLisp) for the movement-analysis side of the
  2001 setup.
- See [EXAMPLES.md](EXAMPLES.md) for the general, architecture-agnostic workflow and Tips & Tricks.
- See [SOM-exemples.md](SOM-exemples.md) and [MLP-exemples.md](MLP-exemples.md) for the other
  architectures.
