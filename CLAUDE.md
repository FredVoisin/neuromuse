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
under `src/`) and is loaded as an ASDF system, not via ad hoc `load` calls.

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
depends on `make-listarray` from `misc`, hence loading after it), `mlp`, `som`, `rosom`, `udp`. Load
order matters — later files depend on classes/functions (e.g. `ANN`, `make-new-symbol`, matrix helpers)
defined earlier. `maths.lisp` and `misc.lisp` used to be one file, `maths&misc.lisp`; split along the
line the old name already implied (numerical/matrix code vs. generic utilities), with the duplicate
`make-new-symbol` definition (also in `neuromuse-main.lisp`) dropped rather than carried into either
half. `src/perceptron.lisp` is not part of the `.asd` `:components` list (mirrors upstream: it's marked
"unfinished ?, see mlp") and must be loaded manually with `(load "src/perceptron.lisp")` after the
system if needed.

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
  in a comment, superseded by MLP. Not part of the `.asd` build (see above).
- `mlp` (`src/mlp.lisp`, subclass of `ANN`) — multi-layer perceptron with backpropagation. `net` is a
  list of weight matrices (one per layer transition), each matrix a list-of-lists. `hidden-fun`/
  `out-fun` are activation functions (default `logistic`). Has `activation`, `clear`, and `save`
  methods (`save` replaces the old `nnsave`).
- `rmlp` (`src/mlp.lisp`, subclass of `mlp`) — recurrent MLP (Elman style): one hidden layer's
  activation feeds back into the input on the next step via `recurrent-layer-activation`.
- `som` (`src/som.lisp`, subclass of `ANN`) — self-organizing (Kohonen) map. `net` is a flat list of
  `neuron` instances; `topology` describes the map's spatial layout for neighborhood computation.
- `rosom` (`src/rosom.lisp`, subclass of `som`) — "recurrent oscillatory SOM": pairs a content SOM with
  a context SOM (`net` is `(content-neurons context-neurons)`) plus phase/frequency-like state
  (`input-context`) so winners synchronize over time.

Instances are not just returned values — `initialize-instance :after` methods intern a fresh symbol
for each instance via `make-new-symbol` and bind the instance as that symbol's value (e.g. creating an
`ann` binds a global like `ANN-3` to the instance). Code elsewhere treats network names as symbols and
uses `eval`/`symbol-value` to dereference them; keep this in mind when reading or writing methods that
take a "network" argument, since it may be a symbol, a `neuron`/`ANN` instance, or a list depending on
the generic function (see the `net`/`id` generic methods in `src/neuromuse-main.lisp` for the
symbol/list dispatch pattern used throughout).

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

`structure-slot-names` (`src/neuromuse-main.lisp`) — used by `mlp`'s and `neuron`'s `save` methods to
list a class's slots — is implemented via `sb-mop:class-slots`/`sb-mop:slot-definition-name` (SBCL-only;
the upstream version was commented out and had `+sbcl` instead of `#+sbcl`, so it never actually ran).
`save`'s output isn't a perfect round-trip yet: `mlp`'s `save` method only wraps list-valued slots in a
quote when printing, so a non-list slot holding a symbol (e.g. `:name`) prints unquoted and would be read
back as a variable reference, not a literal — a pre-existing quirk, not something this pass redesigned.

The `:neuromuse` package exports nothing from its own `defpackage` form (`src/neuromuse.lisp`). Instead,
`src/udp.lisp` (last file in the `.asd` load order) and `src/perceptron.lisp` (loaded separately) each
end with a `do-symbols` sweep that exports every symbol interned in `:neuromuse` so far — restoring the
same unqualified, load-and-use-everything-at-the-REPL visibility the code had before the 2021 package
split (when everything lived directly in `:cl-user`). This is why a separate consumer package like
`neuromuse-test` (which `:use`s `:neuromuse`) can see `logistic`, `make-mlp`, `in-size`, etc. unqualified.
If you add a new top-level `src/` file to the `.asd` `:components` list, put it *before* `udp.lisp`, or
the export sweep won't see its symbols.

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
  `mlp`'s `backpropagate`/`run-mlp` call it on the net via `net-temp` to add weight jitter.
- Gotcha: `ANN`'s `learn-fact` slot defaults to `0.0`, and `make-MLP`/`make-rMLP` don't override it — a
  freshly-constructed net won't learn anything from `backpropagate` until you `(setf (learn-fact net)
  ...)` yourself (see `examples/mlp-test.lisp` or the `tests/neuromuse.lisp` MLP subtest).
- SOM topology/neighborhood helpers: `2d`/`d2`, `3d`/`d3` (index <-> spatial coordinate conversion),
  `voisins` (neighborhood lookup), `gaussian-hat` (Mexican-hat-style learning rate falloff).

`src/misc.lisp` — generic Lisp utilities and UDP wire-format conversion, loaded *before* `maths.lisp`
since `maths.lisp`'s matrix functions (`add-2-matrices`, `hadamar-product`) default-call `make-listarray`
from here:
- `make-listarray`, `ldlp`/`ldvp`, `round1`, `test-t`, `get-time`.
- String/wire-format conversion for the UDP layer: `st2v`, `st2list`, `vector2string`/`v2st`,
  `list2string`, `buf2string`, `split`.

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
