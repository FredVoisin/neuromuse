# Output Formats: Saving, Tracing, and Visualizing a Network

Three complementary ways `src/read-write.lisp` writes a network's state to disk — none of them
change anything about training itself, they just read what's already there:

- **`save`** — serializes (most of) a network to a reloadable Lisp form: the way to persist a trained
  network and pick up where you left off.
- **`trace-activation`** / **`trace-output`** — append one line per call to a log file, meant to be
  called from inside a training/running loop without changing it otherwise: the way to record how a
  network behaves *over time*.
- **`write-dot`** — writes a network's current connectivity (weights and all) as a graph, in
  [Graphviz](https://graphviz.org/)'s DOT format, optionally rendered straight to PNG/SVG: the way to
  *look at* a network's structure.

See [EXAMPLES.md](EXAMPLES.md) for the general training workflow, [MLP-exemples.md](MLP-exemples.md)
and [SOM-exemples.md](SOM-exemples.md) for the `xor` and `my-som` networks used as examples below.

---

## 1. `save`: reload a network as-is

```lisp
(save xor)              ;; writes xor.lisp in the current directory
(save xor "xor.lisp")   ;; or an explicit path
```

`save` has a method per class with its own idea of what "the state that matters" is — it does **not**
blindly dump every slot for every class:

- **`mlp`/`rmlp`** — every slot, via `structure-slot-names` (SBCL's MOP), so the file created is
  essentially a `(make-instance 'mlp ...)` form with every current value spelled out, net included.
- **`som`** — not every slot: `(net som)` is a list of `neuron` instances and `(neighbourhood som)` is
  a function, neither reads back with `~S`. Instead, `save` writes a single
  `(let ((it (make-instance 'som :size ... :input ...))) ...)` form that re-triggers `init` (so the
  grid comes back correctly wired), then `setf`s the settings that actually matter (`topology`,
  `radius`, `learn-fact`, `temp`, `net-temp`, `distance`, `grid-distance`, `epoch`, `error-scaling`,
  `max-error`) and each neuron's learned weights.
- **`rosom`** — not supported yet (`(net rosom)` is `(content-neurons context-neurons)`, not a flat
  list like `som`'s — `save` errors clearly rather than misreading it).
- **`neuron`** — every slot, same as mlp.

Reload with plain `load`:

```lisp
(load "xor.lisp")   ;; rebinds xor.lisp's :name symbol to the reconstructed instance
```

## 2. `trace-activation` / `trace-output`: a log over time

Both are meant to be dropped into an existing loop without changing anything else about it:

```lisp
(dotimes (i 2000)
  (dolist (pattern *xor-in*)
    (setf (input xor) pattern (goal xor) ...)
    (trace-activation (learn xor) "xor-trace.lisp")))   ;; one line per call, appended
```

`trace-activation` appends one Lisp form per call to `path` (default `activation-trace.lisp`) and
returns its argument unchanged — the same "returns only itself" convention as `learn` across every
architecture, so it drops straight into a chain like the example above. What gets written is
`(activation-state ann)`: `(output ann)` for mlp/rmlp/perceptron, or one activation list per grid cell
for a som (`(mapcar #'output (net ann))` — reads what `find-winner`/`learn` already computed as a side
effect, doesn't recompute anything). Read the whole sequence back, oldest first, with
`(read-activation-trace "xor-trace.lisp")`.

`trace-output` is narrower and `som`-specific: it appends just the winning neuron's current
`activation` (it calls `find-winner` itself, so don't use it from a thread that's also training the
same network, cf. `src/udp.lisp`'s servers — and don't call it on an mlp/perceptron/etc., `find-winner`
has no method for them).

## 3. `write-dot`: the network as a graph

Every network already has a uniform graph view — `(nodes ann)`/`(edges ann)`, one `edges` method per
class, `nodes` derived from it mechanically (see `src/neuromuse-main.lisp`). `write-dot` is just that
view, written out as [DOT](https://graphviz.org/doc/info/lang.html): one node per element of
`(nodes ann)`, one directed edge `source -> target` per element of `(edges ann)`, labeled with the
weight (rounded to 3 decimals). It works on *any* `ann` subclass unchanged — including `rosom`, which
`save` still doesn't support:

```lisp
(write-dot xor)                  ;; writes xor.dot in the current directory, DOT source only
(write-dot my-som "map.dot")     ;; same, explicit path
```

### Rendering to PNG/SVG directly

Pass `:format`, and optionally `:engine` (any Graphviz layout engine — `"dot"`, the default, lays out
hierarchically; `"neato"`, `"fdp"`, `"sfdp"` lay out by force; `"circo"`, `"twopi"` lay out radially).
`write-dot` writes the DOT source to a temporary file, renders it with the external engine
(`sb-ext:run-program`, so this needs SBCL and a working Graphviz install — `apt install graphviz` on
Debian), and deletes the temporary file once the render succeeds:

```lisp
(write-dot xor "xor.png" :format :png)                      ;; dot -Tpng, hierarchical layout
(write-dot xor "xor.svg" :format :svg)                      ;; same, SVG
(write-dot my-som "map.png" :format :png :engine "neato")   ;; force-directed layout
```

If the engine can't be found or fails, `write-dot` warns and keeps the `.dot` file instead of silently
losing your run — nothing is lost, you can always render it by hand afterward:

```sh
dot -Tpng xor.dot -o xor.png
neato -Tsvg map.dot -o map.svg
```

`:format :dot` (the default) never touches an external process at all — the DOT text itself is always
the portable fallback, on any Lisp, with any Graphviz engine, now or later.

### Demo: `xor` (MLP-exemples.md)

```lisp
(make-mlp xor 2 1 2)
(setf (learn-fact xor) 0.4 (threshold xor) 0.1)
;; ... train as in MLP-exemples.md ...
(write-dot xor "xor.png" :format :png)
```

Three input/hidden/output layers, nodes named `(layer index)` (layer 0 = input), one edge per synapse:
*(Two input nodes `(0 0)` `(0 1)` feeding two hidden nodes `(1 0)` `(1 1)`, feeding one output node
`(2 0)` — six weighted edges in total, `dot`'s hierarchical layout puts the layers in order
top-to-bottom.)*

### Demo: `my-som` (SOM-exemples.md)

```lisp
(make-instance 'som :name 'my-som :size 144 :input 18)
;; ... train as in SOM-exemples.md ...
(write-dot my-som "map.png" :format :png :engine "neato")
```

Every neuron connects to every input dimension (a star/bipartite structure, not a layered one like an
mlp's), so nodes are either `neuron` instances (one per grid cell, 144 here) or `(:input j)` (one per
input dimension, 18 here) — `neato`'s force-directed layout reads this kind of densely-connected graph
better than `dot`'s hierarchical one, though at 144×18 = 2592 edges it's still dense enough that
cropping to a handful of neurons (`(net my-som)` is a plain list — `(write-dot <a toy som>)` on a much
smaller one, say `:size 9`, is far more legible than the full map).

A `rosom`'s graph is the same idea, twice over: content neurons' edges tagged `(:input j)`, context
neurons' edges tagged `(:context j)` — two different dimension spaces (input vs. `input-context`'s
per-neuron phase state), kept visually distinct even though both use the same `j` indices.

---

## Further Reading

- [EXAMPLES.md](EXAMPLES.md) — general training workflow.
- [MLP-exemples.md](MLP-exemples.md), [SOM-exemples.md](SOM-exemples.md) — the `xor`/`my-som` networks
  used above.
- `src/neuromuse-main.lisp` — `nodes`/`edges`, the generic graph view `write-dot` reads.
- `src/read-write.lisp` — `save`, `write-dot`, `trace-activation`/`trace-output`, full docstrings.
