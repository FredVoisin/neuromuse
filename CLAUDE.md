# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

neuromuse is a Common Lisp library of artificial neural network architectures (perceptron, MLP,
recurrent MLP, self-organizing maps, recurrent oscillatory SOM) built since 1999-2000 for real-time,
network-controlled computer music and generative art. It targets SBCL today; historically it also ran
on Macintosh Common Lisp (MCL/OpenMCL), Lispworks, and CMUCL, and reader conditionals (`#+sbcl`,
`#+mcl`, `#+cmu`, ...) throughout the code still branch on this. There is no CI or automated test
harness — this is a "load the system into a Lisp image and interact at the REPL" project.

All library code lives in the `:neuromuse` package (`(in-package :neuromuse)` at the top of every file
under `src/`) and is loaded as an ASDF system, not via ad hoc `load` calls. The one exception is
`src/gui.lisp`, the optional Ltk window, which defines its own `:neuromuse-gui` package (`:use`ing
`:cl` and `:neuromuse`) because it needs `ltk:` symbols and ships as a separate system — see
"Visualisation (Ltk)" below.

## Running the code

Load the system via ASDF from an SBCL REPL started in this directory (or with this directory on
`asdf:*central-registry*`):

```lisp
(push #P"./" asdf:*central-registry*)   ; only if this directory isn't already findable by ASDF
(asdf:load-system :neuromuse)
(in-package :neuromuse)
```

`neuromuse.asd` defines the `neuromuse` system, depends on `:sb-bsd-sockets`, and loads `src/` files in
this order: `neuromuse` (package definition), `neuromuse-main` (core classes/generics), `misc` (generic
Lisp/wire-format utilities), `maths` (transfer functions, matrix/vector algebra, SOM topology math —
depends on `make-listarray` from `misc`, hence loading after it), `mlp`, `perceptron`, `hopfield`,
`auto-assoc`, `som`, `rosom`, `read-write`, `udp`. Load order matters — later files depend on
classes/functions (e.g. `ANN`, `make-new-symbol`, matrix helpers) defined earlier. `maths.lisp` and
`misc.lisp` used to be one file, `maths&misc.lisp`; split along the line the old name already implied
(numerical/matrix code vs. generic utilities), with the duplicate `make-new-symbol` definition (also in
`neuromuse-main.lisp`) dropped rather than carried into either half. `src/perceptron.lisp` is part of
the `:components` list, loaded right after `mlp`; the upstream "unfinished ?, see mlp" comment is about
the class being superseded, not about the build. `src/hopfield.lisp`/`src/auto-assoc.lisp` only need
`ann`/`learn`/`binary`/`widrow-hoff`/`logistic` (all defined by `maths`), so their exact position
between `perceptron` and `som` is cosmetic, not load-bearing. `src/read-write.lisp` loads after
`mlp`/`som`/`rosom` (its `save` and `activation-state` methods specialize on those classes, so they must
already exist) and before `udp.lisp` (see the
`do-symbols` export-sweep note further down — it has to precede `udp.lisp` in `:components` for the same
reason any
new top-level file does).

Run the test suite with:

```lisp
(push #P"./" asdf:*central-registry*)
(asdf:test-system :neuromuse)
```

`tests/neuromuse.lisp` is a `prove`-based suite (package `neuromuse-test`, `:use`s `:neuromuse` and
`:prove`) covering the math/transfer-function layer, matrix/vector algebra, SOM topology helpers, MLP
construction/shape, one deterministic MLP-training-reduces-error check (fixed `*random-state*` seed, so
it's not flaky), rmlp construction, and som/rosom construction+activation+winner-finding. It's wired up
via a second system, `neuromuse-test`, defined at the bottom of `neuromuse.asd`
(`:depends-on (:neuromuse :prove)`, loads `tests/neuromuse.lisp`) and referenced from `neuromuse`'s
`:in-order-to ((test-op (test-op "neuromuse-test")))`. Requires Quicklisp for `:prove`.

Beyond the test suite, "testing" in practice also means running the scripts in `examples/` at the REPL
(e.g. `(load "examples/mlp-test.lisp")` after loading the system trains an MLP on XOR and prints
predictions) or evaluating the commented-out example forms left at the bottom of `src/som.lisp` and
`src/rosom.lisp`, and checking output/behavior by hand.

## Architecture

### Class hierarchy

All network types inherit from the base `ANN` class (`src/neuromuse-main.lisp`), which holds shared
state: `net` (the actual weights/topology), `input`/`output`, `epoch`, `learn-fact`, `history-error`,
`temp` / `net-temp` (stochastic noise temperature for output vs. for synaptic weights — see `noise` in
`maths.lisp`), UDP-related fields (`udplist`, `iplist`, `daemons`, `superdaemon`, `attention`,
`latence`), and bookkeeping (`creation-date`, `history`, `verbose`).

- `neuron` (`src/neuromuse-main.lisp`) — a single unit with its own `net` (list of `(neuron weight
  last-activation)` triples), `fun` (activation function + args), `slope`, `bias`, `temp`. Used as the
  node type inside SOM/ROSOM nets.
- `perceptron` (`src/perceptron.lisp`, subclass of `ANN`) — simple single-layer net; marked "unfinished"
  in a comment, superseded by MLP. Loaded by the `.asd` after `mlp`. Its one-step training method is
  `learn`, same name and "returns only itself" convention as every other architecture's — renamed from
  `learn-perceptron`, which took the stimulus/goal as `:in`/`:goal` keywords rather than reading
  `(input self)`/`(goal self)` and returned an error count rather than `self`; `train-perceptron`'s
  own multi-epoch loop (unrenamed — it's the driver, not the one-step primitive, same role as
  `train-pinson-som`) now does the `setf` itself before each call, and reads that former return value
  off `(current-error self)` instead.
- `hopfield` (`src/hopfield.lisp`, subclass of `ANN`) and `auto-assoc` (`src/auto-assoc.lisp`, subclass
  of `ANN`) — both ported from `legacy/neuromuse-2001-concert/"ANN 1.4.lisp"` (MCL, 2001), class names
  unchanged from there. `net` for either is a plain CL `in-size x in-size` 2D array (not a list like
  mlp/perceptron, nor `neuron` instances like som/rosom) of symmetric synaptic weights,
  `(aref net i j)` always equal to `(aref net j i)`. `hopfield` is a classic associative memory: `learn`
  reinforces each synapse by Hebbian coincidence between cells `i`/`j` of `(input self)`; `run-hopfield`
  recalls by thresholding (`binary`, `maths.lisp`) the weighted sum for each cell, so presenting a
  noisy/partial pattern tends to settle on the closest trained one — within capacity (~0.138 x in-size
  patterns for random ones, the standard Hopfield limit, less for correlated ones). `auto-assoc` learns
  by Widrow-Hoff instead (same rule as `perceptron`, reusing `widrow-hoff`/`logistic` from
  `maths.lisp`) to reproduce/associate `(input self)` to itself — the file is named `auto-assoc.lisp`
  for what it is now usually called (a single-layer linear auto-encoder, no bottleneck), but the class
  keeps its original, more exact 2001 name. `hopfield`'s `learn` originally divided the *entire*
  existing weight by `in-size` on every call, not just the new contribution — a bug carried over from
  2001, since fixed: only the new contribution is normalized by `in-size` now, same as the standard
  Hebbian rule, before being *added* to the existing weight. The old scaling compounded across repeated
  training and measurably hurt recall well before the standard capacity limit should bite (5 random
  patterns on 100 cells: only 1 recalled exactly before the fix, 5/5 after — right at the ~0.138 x
  in-size capacity limit itself, recall still degrades sharply, which is expected Hopfield behaviour,
  not a bug). `auto-assoc`'s `learn`/`run-auto-assoc` originally indexed with `(elt input i)` inside
  their loop over `j` instead of `(elt input j)` — another 2001 bug, since fixed: each cell's
  activation is now the proper weighted sum over the whole input, `sum_j net[i][j] * input[j]`, not
  `input[i]` times a sum that didn't depend on the rest of the input. Confirmed by testing: trained on
  a few bipolar patterns, `run-auto-assoc` now recalls each one almost exactly (`tanh` output close to
  ±1 matching the pattern), and completes a partial cue (one bit zeroed out) back to the full trained
  pattern — proper auto-associative pattern completion, which the pre-fix version could not do.
  `run-auto-assoc`/`run-hopfield` are both renamed from 2001 (`run-aa`/a `run-hopfield` `defun`, not a
  method) to match `run-mlp`/`run-perceptron`'s convention; `run-aa` itself no longer exists anywhere
  in the codebase. `auto-assoc` now has a `gui-snapshot` method and its own `neuromuse-gui` window (see
  "Visualisation (Ltk)" below) — real performance history, not a toy: see
  [doc/auto-assoc-exemples.md](../doc/auto-assoc-exemples.md) for the 2001 use of a 180-cell
  `auto-assoc` (`ecarl180`) to analyze a dancer's movement live on stage, fed over MIDI from
  [LabanOnLisp](https://github.com/FredVoisin/LabanOnLisp), the reconstruction-distance signal that
  produced being exactly what the new viewer's curve plots today. `hopfield` has no `gui-snapshot` yet.
  Neither class has a `save` or `activation-state` method (no file saving, no `trace-activation`
  support) — not ported, since nothing asked for it and both would need real design decisions (a 2D
  array doesn't serialize or trace the way `som`'s neuron list or `mlp`'s weight-matrix list do). The
  2001 file's own `vector-difference`/`vector-addition`/`vector-*` helpers were duplicated between its
  Hopfield and auto-associative sections and never actually called by either's `learn`/`run` — not
  ported; `substract-2-vectors`/`add-two-vectors`/`multiply-2-vectors` (`maths.lisp`) already cover the
  same role if ever needed. Its MCL-specific `view-aa-activation` window and hardcoded-53x11-grid `see`
  printer weren't ported either, for the same reason `src/gui.lisp` doesn't yet have a view for these
  two classes — an actual port would mean designing one, not transcribing QuickDraw calls.
- `mlp` (`src/mlp.lisp`, subclass of `ANN`) — multi-layer perceptron. `net` is a
  list of weight matrices (one per layer transition), each matrix a list-of-lists. `hidden-fun`/
  `out-fun` are activation functions (default `logistic`). Has `learn` (one step; renamed from
  `backpropagate` — dropped its `&optional in` override, so it always trains on `(input mlp)`, the
  same way every other architecture's `learn` reads straight off the instance; a commented-out `learn`
  method shelved years earlier for unrelated reasons, in the "oldies" block near the end of
  `mlp.lisp`, happens to share the name now — see the note right above that block) and `clear` methods
  in `mlp.lisp` itself; its `save` method (replacing the old `nnsave`) moved to `src/read-write.lisp`
  alongside the `neuron`/`list`/`t` `save` methods it used to sit next to in `neuromuse-main.lisp` — see
  "Reading and writing a network to a file (read-write.lisp)" below.
- `rmlp` (`src/mlp.lisp`, subclass of `mlp`) — recurrent MLP (Elman style): one hidden layer's
  activation feeds back into the input on the next step via `recurrent-layer-activation`.
- `som` (`src/som.lisp`, subclass of `ANN`) — self-organizing (Kohonen) map. `net` is a flat list of
  `neuron` instances; `topology` describes the map's spatial layout for neighborhood computation.
- `rosom` (`src/rosom.lisp`, subclass of `som`) — "recurrent oscillatory SOM": pairs a content SOM with
  a context SOM (`net` is `(content-neurons context-neurons)`) plus phase/frequency-like state
  (`input-context`) so winners synchronize over time. Its training step is a `learn` method, like
  `som`'s — this used to be a separate plain function, `rosom-learn`, taking `radius`, the learning
  rate, `entrainement-rate`, `temp-som` and `temp-rosom` as explicit arguments on every call (`input`
  too, never read from `(input rosom)`), because none of those last three had a slot to read from
  above `rosom`. Renamed into `learn` by adding `entrainement-rate`/`temp-som`/`temp-rosom`/
  `content-on`/`context-on` slots to the `rosom` class (defaults `0.0`/`0.0`/`0.0`/`1`/`1`, same
  "off until you set it" convention as `learn-fact`/`net-temp`) and wrapping the function's unchanged
  body in a `let` that reads all of it — `radius`/`learn-fact`/`verbose` included — off `self` instead:
  set them once with `setf`, then call `(learn a-rosom)` repeatedly, exactly like `som`. (Before this,
  `(learn a-rosom)` fell through to `som`'s own method and failed deep inside `find-winner` with a
  confusing "no applicable method for `ID`" error, since `(net rosom)` isn't a flat neuron list — same
  problem `rosom`'s `gui-snapshot`/`activation-state`/`save` methods guard against explicitly, below.)

Instances are not just returned values — `initialize-instance :after` methods intern a fresh symbol
for each instance via `make-new-symbol` and bind the instance as that symbol's value (e.g. creating an
`ann` binds a global like `ANN-3` to the instance). Code elsewhere treats network names as symbols and
uses `eval`/`symbol-value` to dereference them; keep this in mind when reading or writing methods that
take a "network" argument, since it may be a symbol, a `neuron`/`ANN` instance, or a list depending on
the generic function (see the `net`/`id` generic methods in `src/neuromuse-main.lisp` for the
symbol/list dispatch pattern used throughout). This naming is `ANN`'s own `:after` method's job; `som`
and `rosom` each only add their own `:after` for what `ANN`'s doesn't already do (triggering `init` for
`som`; nothing extra for `rosom`, beyond a fallback name for anonymous construction — see below). They
used to *also* redo the naming unconditionally, which is actively wrong, not just redundant: CLOS runs
every applicable `:after` method, least-specific first, so `ANN`'s ran first and correctly bound e.g.
`PINSON-SOM`, then `som`'s ran again and, seeing that name already taken, gensymed `PINSON-SOM-129`
instead and overwrote `(name self)` with it — the original global stayed correctly bound (so ordinary
`(learn-fact pinson-som)`-style code never noticed), but `(name pinson-som)` itself, and anything reading
it (the GUI status line, `save` — see "Reading and writing a network to a file" below), was wrong for
*every* `som`/`rosom` ever given an explicit `:name`. Fixed by only redoing the naming when no `:name`
was given (anonymous construction still needs it, or an anonymous `som`/`rosom` would end up called
`ANN-n`/`SOM-n` instead of `SOM-n`/`ROSOM-n`).

Networks are constructed via macros, not `make-instance` directly: `make-perceptron`, `make-MLP`,
`make-rMLP`. Each expands to code that builds the instance at load/run time (a `defvar`/`setf` wrapping
a `make-instance` form) and guards against clobbering an existing network bound to the same name (via
`ann-p` + `warning-msg`). These macros used to splice a *live instance*, built at macroexpansion time,
directly into their expansion — which only worked when the call was interpreted at the REPL, not when
compiled (SBCL has no `make-load-form` for these classes, so `compile-file` errored with "don't know how
to dump ..."). This broke as soon as anything (the ASDF-compiled test suite) actually called them from a
file. Fixed by having the macros return *code* that constructs the instance, not the instance itself. If
you add a similar constructor macro, follow the same pattern: return a form, don't embed a runtime
object literal in the expansion. `copy-MLP`/`duplicate` (also in `mlp.lisp`) are commented out — they
called an undefined `copy-net` and referenced a nonexistent `:parent` initarg; shelved rather than
half-fixed, since net-copying was never actually designed.

`structure-slot-names` (`src/read-write.lisp`) — used by `mlp`'s and `neuron`'s `save` methods to
list a class's slots — is implemented via `sb-mop:class-slots`/`sb-mop:slot-definition-name` (SBCL-only;
the upstream version was commented out and had `+sbcl` instead of `#+sbcl`, so it never actually ran).
`save`'s output isn't a perfect round-trip yet: `mlp`'s `save` method only wraps list-valued slots in a
quote when printing, so a non-list slot holding a symbol (e.g. `:name`) prints unquoted and would be read
back as a variable reference, not a literal — a pre-existing quirk, not something this pass redesigned.

The `:neuromuse` package exports nothing from its own `defpackage` form (`src/neuromuse.lisp`). Instead,
`src/udp.lisp` (last file in the `.asd` load order) ends with a `do-symbols` sweep that exports every
symbol interned in `:neuromuse` so far — restoring the same unqualified,
load-and-use-everything-at-the-REPL visibility the code had before the 2021 package split (when
everything lived directly in `:cl-user`). This is why a separate consumer package like
`neuromuse-test` (which `:use`s `:neuromuse`) can see `logistic`, `make-mlp`, `in-size`, etc. unqualified.
If you add a new top-level `src/` file to the `.asd` `:components` list, put it *before* `udp.lisp`, or
the export sweep won't see its symbols. (`src/gui.lisp` isn't in that system at all, so the sweep never
reaches it; it exports its own entry points from its `defpackage`.)

### Math / utility layer

`src/maths.lisp` and `src/misc.lisp` (split from a single `maths&misc.lisp`) have no dependencies on
`mlp`/`som`/`rosom`/`udp` and provide everything the network code is built on.

`src/maths.lisp` — the numerical/matrix core:
- Activation/transfer functions as generics dispatching on `number`/`list`/`vector`: `binary`, `sign`,
  `logistic`, `sigmoide`, `linear`, `boltzmann` (all take `:thresh :temp :learn :slope`).
- Matrix/vector algebra on plain lists (not CL arrays), e.g. `multiply-matrix-and-vector`,
  `multiply-two-matrices`, `add-2-matrices`, `transpose`, `hadamar-product`, `substract-2-vectors`.
- Distance/error: `euclidian`, `euclidian-fast`, `check-error` (returns a single summed error value, not
  a list), `compare-vectors`.
- `noise` (replaces the old `noiser`) — random perturbation of a number/list/vector/`neuron`/`ann`;
  `mlp`'s `learn`/`run-mlp` call it on the net via `net-temp` to add weight jitter.
- Gotcha: `ANN`'s `learn-fact` slot defaults to `0.0`, and `make-MLP`/`make-rMLP` don't override it — a
  freshly-constructed net won't learn anything from `learn` until you `(setf (learn-fact net)
  ...)` yourself (see `examples/mlp-test.lisp` or the `tests/neuromuse.lisp` MLP subtest).
- SOM topology/neighborhood helpers: `2d`/`d2`, `3d`/`d3` (index <-> spatial coordinate conversion),
  `voisins` (neighborhood lookup), `gaussian-hat` (Mexican-hat-style learning rate falloff).
- `inventaire` / `inventaire-h` — generic tally: distinct elements of a list and their occurrence counts,
  in first-seen order (`inventaire-h` is the same thing via a hash table, O(n) instead of `inventaire`'s
  O(n^2) `assoc` scan on large lists). Not neural-net-specific; moved here from `examples/pinson_som.lisp`,
  where they were first written to summarize which grid cell a SOM's winner lands on across many inputs.
  Neither is called from any `learn`/`train-*` method yet — they're post-hoc analysis tools invoked from
  the REPL/examples; wire one in directly if a training method ever needs this kind of counting internally.
- `transition-matrix` (sequence, size) and `cos-similarity` (two same-size 2D arrays, treated as
  flattened vectors) — same story as `inventaire`: moved here from `examples/pinson_som.lisp`
  (`cos-similarity` as `compare-transitions`), where they built and compared a SOM's per-song
  transition matrices (`compare-pinson-chants-transitions`, still there, now just calling both) —
  nothing about either computation is SOM- or neural-net-specific. Moving `transition-matrix` dropped
  its `size` argument's pinson-specific default (`*pinson-som-size*`); callers now pass it explicitly.

`src/misc.lisp` — generic Lisp utilities and UDP wire-format conversion, loaded *before* `maths.lisp`
since `maths.lisp`'s matrix functions (`add-2-matrices`, `hadamar-product`) default-call `make-listarray`
from here:
- `make-listarray`, `ldlp`/`ldvp`, `round1`, `test-t`, `get-time`.
- String/wire-format conversion for the UDP layer: `st2v`, `st2list`, `vector2string`/`v2st`,
  `list2string`, `buf2string`, `split`.

### Reading and writing a network to a file (read-write.lisp)

`src/read-write.lisp` holds the library's file I/O for a network: `save` (serializing a `neuron`/`mlp`
instance to a reloadable Lisp form, together with its one helper `structure-slot-names` — moved here
from `neuromuse-main.lisp`/`mlp.lisp`, where its methods used to be split across the two files for no
reason tied to what they do) and `trace-activation` (below), added together with this file as a single
place for "write a network's state to disk" rather than splitting that concern by which file a class
happens to be defined in.

`save` for a `som` can't just dump every slot via `structure-slot-names` the way `mlp`'s does: `net` is a
list of `neuron` instances, not numbers, and `neighbourhood` a function (`#'voisins` by default) —
neither reads back with `~S`. Instead it writes one `(let ((it (make-instance 'som ...))) ...)` form:
`make-instance` (re-triggering `init`, so freshly and correctly wired `neuron`s) for the topology, then
`setf`s for the actually-useful config (`topology`, `radius`, `learn-fact`, `temp`, `net-temp`,
`distance`, `epoch`) and each neuron's `net` (its learned weights — the only part of a neuron's own state
that matters for reuse; `age`/`output`/`distance` are left at their defaults). Everything operates on the
`it` binding rather than on a form referencing `(name self)` by symbol a second time, deliberately: if
that name is already taken when the file is reloaded, `initialize-instance` picks a different one (see
above), and separate top-level forms built around the original name would silently miss the instance
actually created. `rosom` gets a guard method that errors clearly instead of misreading its
`(content-neurons context-neurons)` net the same way `som`'s does, matching `gui-snapshot`'s and
`activation-state`'s `rosom` methods.

`trace-activation` lets any training/running loop log a network's activation over time without
changing the loop itself: `(trace-activation ann &optional path)` appends one Lisp form (one call, one
line) to `path` — a pure read of `ann`'s current state, like `gui-snapshot` in `src/gui.lisp` — and
returns `ann` (same "returns only itself" convention as `learn` itself, across every architecture that
has one, so it drops straight into a chain, e.g. `(trace-activation (learn som))`).
`read-activation-trace path` reads the whole sequence back as a list, oldest first. What actually gets
written is `activation-state ann`, one generic with one method per class rather than a type-dispatch
inside `trace-activation` itself:
- The default method, on `ann` itself, is just `(output ann)` — works unchanged for `mlp`, `rmlp`, and
  `perceptron` (all three write their current output to that same ANN-level slot), so none of them needs
  its own method.
- `som` gets its own method: `(mapcar #'output (net ann))`, one list per grid cell (`output` here is
  each *neuron's* own slot, set by `activation`/`find-winner` as a per-input-dimension list — not the
  SOM's own `output` slot, which `init` sets once to zeroes and nothing afterwards ever updates). Like
  the SOM viewer in `src/gui.lisp`, this reads values already computed as a side effect of `find-winner`
  (or a bare `activation` call) rather than recomputing anything itself — call one of those first in the
  loop, or the trace just repeats stale state.
- `rosom` inherits `som`, but `(net rosom)` is `(content-neurons context-neurons)`, not a flat neuron
  list, so it has its own method that signals a clear error instead of silently misreading that
  structure — same pattern as `gui-snapshot`'s `rosom` method in `src/gui.lisp`.

### Real-time control (UDP)

`src/udp.lisp` implements the network's real-time I/O, gated by `#+sbcl`/`#+mcl` reader conditionals
(`send-udp`/`receive-udp` under SBCL use `sb-bsd-sockets`). A network can run as a set of independent
threaded daemons driven by its `superdaemon` flag, dispatched via generic functions specialized on the
`ann` subclass (`som` vs `mlp`) rather than the older per-class `som-input-server`-style functions:
- `input-server` — receives data over UDP into `(input ann)`; for a `som` this also computes the winner
  (`(setf (winner ann) (find-winner ann))`, mirroring the `#+mcl` variant a few lines below it — the
  `#+sbcl` version used to call undefined `winner-neuron`/`(setf winner-neuron)` accessors instead, a
  drift bug fixed by bringing it back in line with the working MCL code path) and activation; for an
  `mlp` it triggers `run-mlp`.
- `control-server` — receives `(slot value)` pairs over UDP and `eval`s a `setf` on that slot of the
  network (i.e. remote parameter control, e.g. adjusting `learn-fact` or `temp` live).
- `output-server` — periodically pushes `(output ann)` out over UDP, paced by `(latence ann)`.

This is the mechanism referenced in the README for driving Max/PureData or other real-time environments
externally over the network. Threads are started with `mk-process` (`src/neuromuse-main.lisp`), which
wraps `sb-thread:make-thread` under SBCL or MCL's process API under `#+mcl`.

### Visualisation (Ltk)

`src/gui.lisp` is an optional Tk window that watches a live `mlp`/`rmlp`, drawing the synaptic weights
either of two ways — a heatmap (one grid per layer transition, one row per neuron, red = positive /
blue = negative, normalised by the net's largest |weight|) or a layered graph (a circle per neuron,
input layer at the top and output at the bottom, each weight the link from its source neuron down to
the one it feeds, coloured the same red/blue as the heatmap; the input-layer circles are filled grey
in proportion to the value presented and the output-layer ones to the predicted value, white = 0 and
black = 1, hidden circles stay white) — plus the curve of `history-error` and
two status lines (topology + settings; epoch, current error, `input`/`output`/`goal`). A button in the
window toggles between the two views live; `(neuromuse-gui:gui xor :view :graph)` opens straight into
the graph one. It is deliberately *read-only* —
nothing in a training loop has to change for the window to follow it, since `examples/mlp-test.lisp`
already pushes onto `(history-error mlp)`, which is what the curve plots (that list is pushed
newest-first, so the GUI reverses it).

It is its own ASDF system, `neuromuse/gui` (bottom of `neuromuse.asd`), so `:ltk` stays out of the
`neuromuse` system's `:depends-on` and the library keeps loading on a headless machine. Named
`neuromuse/gui` and not `neuromuse-gui` so that ASDF resolves it to `neuromuse.asd` even in a fresh
image — the `neuromuse-test` naming only resolves once something else has caused `neuromuse.asd` to be
read, which is why `asdf:test-system :neuromuse` works but a cold `(asdf:load-system :neuromuse-test)`
would not:

```lisp
(ql:quickload :ltk)                  ; once; needs Tk itself (Debian: apt install tk)
(asdf:load-system "neuromuse/gui")
(neuromuse-gui:gui xor)              ; non-blocking: opens in its own thread via mk-process
(neuromuse-gui:demo)                 ; same window on fake weights, to check Tk/Ltk alone
```

Design points worth preserving if you extend it:
- One generic, `gui-snapshot`, is the only tie between the window and a network class: it returns a
  `snapshot` struct (weights, error series, the two status lines, error threshold) for `mlp`/`rmlp`, or a
  separate `som-snapshot` struct (grid side, per-neuron activity, winner index, winner's output, the two
  status lines — see below) for `som`. Add one more method rather than spreading accessor calls through
  the drawing code. `(net mlp)` is already exactly the structure both the heatmap and the graph view
  want, so the `mlp` method is essentially `(copy-list (net source))` — the `copy-list` freezes the
  list's spine, because `learn` replaces the layer matrices in place while the window is reading
  them.
- The heatmap and the graph view are two independent renderers of that same `weights` structure — see
  `layer-sizes` for how the graph derives each layer's neuron count from the matrices alone (no mlp
  slots involved), so it works unchanged on `demo`'s fake weights too. `viewer-view` (`:heatmap` or
  `:graph`) plus a `(view . topology)` cache key in `viewer-built` decide whether `refresh` has to
  rebuild the canvas items or can just recolour the ones already there.
- What `(output mlp)` holds, since the graph view reads it instead of recomputing a forward pass:
  `learn` (and `run-mlp`) write it with the *current* `(input mlp)`, so it is always the
  prediction for the state on screen. But after `learn` it is the prediction made *before*
  that step's weight update, computed with `(threshold mlp)` subtracted from every neuron's input
  (`logistic`'s `:thresh` — `run-mlp` passes 0, so the two disagree whenever `threshold` is non-zero,
  e.g. .51135 vs .53775 measured for threshold .1) and on weights jittered by `net-temp`. Hidden
  activations are not kept anywhere (local variable in `learn`), hence not drawn. For an
  `rmlp` the extra input circles show `recurrent-layer-activation`, which by then is the context that
  will feed the *next* step, not the one the displayed output was computed from.
- Canvas items (weight rectangles or neuron circles/links, error curve, graduations) are created once
  and afterwards only reconfigured or given new coordinates; nothing is destroyed and recreated per
  frame. They're rebuilt only when the topology or the view changes — note that switching view resizes
  the window, since each canvas is sized to its content (heatmap: cell grid; graph: `graph-geometry`,
  at most `*graph-row-gap*` px between layers and `*graph-height-max*` px in total).
- All Tk traffic stays in the window's own thread, which re-arms itself with `ltk:after`, because Ltk
  is not thread-safe: the REPL's training loop only mutates the network, never the widgets. Each
  refresh runs inside a `handler-case` that writes the condition into the window instead of opening a
  debugger in a background thread.

`(net som)` is a *flat* list of `neuron`, one per grid cell — structurally nothing like an mlp's list of
weight matrices — so the som viewer (section 7 of `gui.lisp`) is a parallel implementation (own
`som-viewer`/`som-snapshot` structs, `som-refresh`/`launch-som-gui`/`som-gui`), not a second
`gui-snapshot` branch bolted onto the heatmap/graph code. `(neuromuse-gui:gui some-som)` dispatches to it
automatically (`typep ... 'som`); `(neuromuse-gui:demo-som)` is the fake-data equivalent of `demo`. It
draws a `side x side` grid of cells (one per neuron, positioned by `2d`) shaded by *activity*, a row of
grey input circles above it, and a row of grey circles below showing the winning neuron's `output` —
plus a curve (reusing `build-error-plot`/`draw-error-plot` verbatim) tracing the winner's index over
time. Two things worth knowing if you touch this:
- "Activity" is read, not recomputed: `find-winner` already writes each neuron's distance-to-input into
  `(distance neuron)`, and its per-input weighted terms into `(output neuron)`, every time it's called
  (from `learn` during training, or from a REPL call like `pinson-som-winner`) — `gui-snapshot` just
  normalises `(distance neuron)` across all neurons (1.0 = winner, 0.0 = furthest) and reads
  `(output winner-neuron)` for the bottom row, rather than calling `find-winner` itself from the display
  thread, which would race the training thread's own writes to those same neuron slots. When computing
  the normalised min, do *not* pass `:initial-value 0.0` to the `reduce` finding the minimum distance —
  distances are never negative, so a 0.0 floor wins over every real distance and `(position min-d
  distances)` then never matches anything, leaving `winner` permanently `nil` (caught via `gui-snapshot`
  called directly and inspected, not via the Tk window itself — reading a live `ltk:label`'s text from
  a thread other than the GUI's own throws, since Ltk's output stream is bound only within that thread).
- Unlike `history-error` for the mlp, nothing in `som.lisp` accumulates a history of past winners, so the
  trace is built by the window itself: `som-refresh` (not `gui-snapshot`, which stays a stateless,
  side-effect-free read like its mlp counterpart) pushes each snapshot's winner index onto
  `(som-viewer-winner-history v)`, a field of the viewer struct — never written to the network itself.
  `rosom` inherits `som` but `(net rosom)` is `(content-neurons context-neurons)`, not a flat neuron
  list, so it has its own `gui-snapshot` method that just signals a clear error (caught by
  `som-guarded-refresh` same as any other) rather than silently misreading that structure.

`(net auto-assoc)` is a plain CL 2D array (`src/auto-assoc.lisp`), not a flat neuron list either, so the
auto-assoc viewer (section 9 of `gui.lisp`) is its own parallel implementation again (own
`auto-assoc-viewer`/`auto-assoc-snapshot` structs, `auto-assoc-refresh`/`launch-auto-assoc-gui`/
`auto-assoc-gui`). `(neuromuse-gui:gui some-auto-assoc)` dispatches to it automatically; `demo-auto-assoc`
is the fake-data equivalent of `demo`/`demo-som`. Simpler than the SOM viewer: no input row, no separate
output row — the whole grid *is* `(output source)`, the full reconstruction cell by cell, so a separate
output row would be redundant with it, and there's no "winner" concept to show an input row against
either (auto-assoc isn't competitive). The grid isn't necessarily square like a SOM's (`in-size` isn't
guaranteed to be a perfect square), so cell positions come from a generic `grid-shape` (rows/cols for
any N, not `2d`, which assumes a square). Below the grid, a curve (again reusing `build-error-plot`/
`draw-error-plot` verbatim) traces the reconstruction distance, `(euclidian (input source)
(output source))`, over time — accumulated by `auto-assoc-refresh` into `(auto-assoc-viewer-
distance-history v)`, same non-network-writing pattern as the SOM viewer's `winner-history`. Two things
worth knowing if you touch this:
- `learn` never writes `(output self)` — only `run-auto-assoc` does. Unlike the SOM viewer (where
  `find-winner`, called by `learn` during ordinary training, already refreshes `(distance neuron)`/
  `(output neuron)` as a side effect), a `learn`-only loop leaves the auto-assoc viewer showing nothing
  useful (`(output source)` stays at its initform, `nil`, so `gui-snapshot`'s `distance` is `nil` too,
  shown as "n/d"). The loop driving the network has to call `run-auto-assoc` itself from time to time —
  not a gap to fix, it matches how this was actually used in 2001 (see below): `run-aa` was *always*
  called explicitly to get the activation to look at, never implicitly during training.
- This isn't a toy architecture getting a GUI for the first time: a 180-cell `auto-assoc` (`ecarl180`)
  watched a dancer's movement live on stage in 2001 (*L'Écarlate*, Kasper T. Toeplitz/Myriam Gourfink,
  Ircam), fed over MIDI from [LabanOnLisp](https://github.com/FredVoisin/LabanOnLisp) — see
  [doc/auto-assoc-exemples.md](../doc/auto-assoc-exemples.md) for the full story. The reconstruction
  distance this viewer's curve plots is exactly the signal that performance watched continuously
  (`legacy/neuromuse-2001-concert/getmidi.lisp`'s `get-moment`, `*distances*`).

### Examples

`examples/` holds small, runnable demo scripts (load them after `(asdf:load-system :neuromuse)`):
`mlp-test.lisp` / `mlp-test2.lisp` (MLP backpropagation, e.g. learning XOR), `rmlp-test.lisp` (recurrent
MLP), `4accel2mlp.lisp` / `AirLedZep.lisp` / `test-eurecom1804.lisp` (feeding real sensor/audio-derived
data into an MLP), and `testing.lisp` (bare UDP send/receive smoke test). These assume the `:neuromuse`
package is current (`(in-package :neuromuse)`) since they reference unqualified symbols like `make-mlp`.

## Code conventions in this repo

- Mixed French/English identifiers and comments throughout (e.g. `voisins` = neighbors, `apprentissage`
  = training, `chapeau mexicain` = Mexican hat) — this is original authorial style, not inconsistency to
  "fix."
- Heavy use of `#+lisp-implementation` reader conditionals for portability across SBCL/MCL/CMUCL/etc.
  When editing shared code, preserve the existing conditional structure rather than assuming SBCL-only.
  When adding genuinely new functionality, SBCL-only is acceptable since that's the actively used
  implementation.
- Several methods/macros are explicitly marked incomplete in comments (`src/perceptron.lisp`:
  "unfinished ?, see mlp"; the commented-out `save`-for-`som` in `src/som.lisp`; the shelved
  `copy-MLP`/`duplicate` and large commented-out blocks at the end of `src/mlp.lisp`) — don't assume
  commented-out code is dead weight to delete without checking whether it's a preserved reference
  implementation or explicitly-shelved WIP.
