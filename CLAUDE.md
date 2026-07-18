# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

neuromuse is a Common Lisp library of artificial neural network architectures (perceptron, MLP,
recurrent MLP, self-organizing maps, recurrent oscillatory SOM) built since 1999-2000 for real-time,
network-controlled computer music and generative art. It targets SBCL today; historically it also ran
on Macintosh Common Lisp (MCL/OpenMCL), Lispworks, and CMUCL, and reader conditionals (`#+sbcl`,
`#+mcl`, `#+cmu`, ...) throughout the code still branch on this. There is no build system, package
manager, or test suite — this is a "load the files into a Lisp image and interact at the REPL" project.

## Running the code

Load everything via the entry point, from within an SBCL REPL started in this directory:

```lisp
(load "neuromuse.lisp")
```

`neuromuse.lisp` requires `sb-bsd-sockets` and then `load`s the other files in order:
`maths&misc.lisp`, `mlp.lisp`, `som.lisp`, `udp.lisp`, `rosom.lisp`. Load order matters — later files
depend on classes/functions (e.g. `ANN`, `make-new-symbol`, matrix helpers) defined earlier.
`perceptron.lisp` is not loaded by `neuromuse.lisp` and must be loaded separately if needed.

There is no CI, linter, or automated test harness. "Testing" in this codebase means evaluating the
example forms left as commented-out scratch code at the bottom of files (e.g. the block at the end of
`som.lisp` around `(setf SOM (make-instance 'som ...))`, or the `titi`/rosom examples in `rosom.lisp`)
directly at the REPL, and checking output/behavior by hand.

## Architecture

### Class hierarchy

All network types inherit from the base `ANN` class (`neuromuse.lisp`), which holds shared state:
`net` (the actual weights/topology), `input`/`output`, `epoch`, `learn-fact`, `history-error`,
`temp` (stochastic noise temperature), UDP-related fields (`udplist`, `iplist`, `daemons`,
`superdaemon`, `attention`, `latence`), and bookkeeping (`creation-date`, `history`, `verbose`).

- `neuron` (`neuromuse.lisp`) — a single unit with its own `net` (list of `(neuron weight
  last-activation)` triples), `fun` (activation function + args), `slope`, `bias`, `temp`. Used as the
  node type inside SOM/ROSOM nets.
- `perceptron` (`perceptron.lisp`, subclass of `ANN`) — simple single-layer net; marked "unfinished" in
  a comment, superseded by MLP.
- `mlp` (`mlp.lisp`, subclass of `ANN`) — multi-layer perceptron with backpropagation. `net` is a list
  of weight matrices (one per layer transition), each matrix a list-of-lists. `hidden-fun`/`out-fun`
  are activation functions (default `logistic`).
- `rmlp` (`mlp.lisp`, subclass of `mlp`) — recurrent MLP (Elman/Jordan style): one hidden layer's
  activation feeds back into the input on the next step via `recurrent-layer-activation`.
- `som` (`som.lisp`, subclass of `ANN`) — self-organizing (Kohonen) map. `net` is a flat list of
  `neuron` instances; `topology` describes the map's spatial layout for neighborhood computation.
- `rosom` (`rosom.lisp`, subclass of `som`) — "recurrent oscillatory SOM": pairs a content SOM with a
  context SOM (`net` is `(content-neurons context-neurons)`) plus phase/frequency-like state
  (`input-context`) so winners synchronize over time; ports MCL-era logic to OpenMCL/SBCL.

Instances are not just returned values — `initialize-instance :after` methods intern a fresh symbol
for each instance via `make-new-symbol` and bind the instance as that symbol's value (e.g. creating an
`ann` binds a global like `ANN-3` to the instance). Code elsewhere treats network names as symbols and
uses `eval`/`symbol-value` to dereference them; keep this in mind when reading or writing methods that
take a "network" argument, since it may be a symbol, a `neuron`/`ANN` instance, or a list depending on
the generic function (see the `net`/`id` generic methods in `neuromuse.lisp` for the symbol/list
dispatch pattern used throughout).

Networks are constructed via macros, not `make-instance` directly: `make-perceptron`, `make-MLP`,
`make-rMLP` (and the commented-out `copy-MLP`). Each expands to a `defvar`/`setf` that also guards
against clobbering an existing network bound to the same name (via `ann-p` + `warning-msg`).

### Math / utility layer

`maths&misc.lisp` has no dependencies on the other files and provides everything the network code is
built on:
- Activation/transfer functions as generics dispatching on `number`/`list`/`vector`: `binary`, `sign`,
  `logistic`, `sigmoide`, `linear`, `boltzmann` (all take `:thresh :temp :learn :slope`).
- Matrix/vector algebra on plain lists (not CL arrays), e.g. `multiply-matrix-and-vector`,
  `multiply-two-matrices`, `add-2-matrices`, `transpose`, `hadamar-product`, `substract-2-vectors`.
- Distance/error: `euclidian`, `euclidian-fast`, `check-error`, `compare-vectors`.
- SOM topology/neighborhood helpers: `2d`/`d2`, `3d`/`d3` (index <-> spatial coordinate conversion),
  `voisins` (neighborhood lookup), `gaussian-hat` (Mexican-hat-style learning rate falloff).
- String/wire-format conversion for the UDP layer: `st2v`, `st2list`, `vector2string`/`v2st`,
  `list2string`, `buf2string`, `split`.

### Real-time control (UDP)

`udp.lisp` implements the network's real-time I/O, gated by `#+sbcl`/`#+mcl` reader conditionals
(`send-udp`/`receive-udp` under SBCL use `sb-bsd-sockets`). A SOM can run as a set of independent
threaded daemons driven by its `superdaemon` flag:
- `som-input-server` — receives a vector over UDP into `(input som)`, computes the winner and
  activation.
- `som-control-server` — receives `(slot value)` pairs over UDP and `eval`s a `setf` on that slot of
  the SOM (i.e. remote parameter control, e.g. adjusting `learn-fact` or `temp` live).
- `som-output-server` / `som-activation-server` — periodically push `(output som)` / `(activation
  som)` out over UDP, paced by `(latence som)`.

This is the mechanism referenced in the README for driving Max/PureData or other real-time environments
externally over the network. Threads are started with `mk-process` (`neuromuse.lisp`), which wraps
`sb-thread:make-thread` under SBCL or MCL's process API under `#+mcl`.

## Code conventions in this repo

- Mixed French/English identifiers and comments throughout (e.g. `voisins` = neighbors, `apprentissage`
  = training, `chapeau mexicain` = Mexican hat) — this is original authorial style, not inconsistency to
  "fix."
- Heavy use of `#+lisp-implementation` reader conditionals for portability across SBCL/MCL/CMUCL/etc.
  When editing shared code, preserve the existing conditional structure rather than assuming SBCL-only.
  When adding genuinely new functionality, SBCL-only is acceptable since that's the actively used
  implementation.
- Several methods/macros are explicitly marked incomplete in comments (`perceptron.lisp`: "unfinished
  ?, see mlp"; `neuromuse.lisp` nnsave for `ann`; large commented-out blocks at the end of `mlp.lisp`
  and `som.lisp`) — don't assume commented-out code is dead weight to delete without checking whether
  it's a preserved reference implementation.
