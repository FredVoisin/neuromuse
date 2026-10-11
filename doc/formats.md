# Output Formats: Saving, Tracing, and Visualizing a Network

Three complementary ways `src/read-write.lisp` writes a network's state to disk — none of them
change anything about training itself, they just read what's already there.

See [EXAMPLES.md](EXAMPLES.md) for the general training workflow, [MLP-exemples.md](MLP-exemples.md)
and [SOM-exemples.md](SOM-exemples.md) for the `xor` and `my-som` networks used as examples below.

## Contents

1. [`save`: reload a network as-is](#1-save-reload-a-network-as-is)
2. [`trace-activation` and `trace-output`: a log over time](#2-trace-activation-and-trace-output-a-log-over-time)
3. [`write-dot`: the network as a graph](#3-write-dot-the-network-as-a-graph)
   - [Coloring edges by weight](#coloring-edges-by-weight)
   - [Rendering to PNG or SVG directly](#rendering-to-png-or-svg-directly)
   - [Demo: `xor`](#demo-xor)
   - [SOM: the grid instead of the connections](#som-the-grid-instead-of-the-connections)
- [Further Reading](#further-reading)

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

## 2. `trace-activation` and `trace-output`: a log over time

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

`write-dot` has one method per *shape* of graph, not one per class — what a network's connectivity
means to look at differs enough between an mlp and a som that one generic rendering can't serve both
well. Two shapes exist today:

- **mlp/rmlp/perceptron/hopfield/auto-assoc** — the uniform graph view every `ann` subclass already
  has, `(nodes ann)`/`(edges ann)` (one `edges` method per class, `nodes` derived from it mechanically,
  see `src/neuromuse-main.lisp`): one node per element of `(nodes ann)`, one directed edge
  `source -> target` per element of `(edges ann)`. This is the *input/output* connectivity — which
  weight feeds which unit — which is exactly what defines these architectures.
- **som** — the neighbour structure of the grid instead: a som is defined by *which neurons sit next to
  which*, not by its (very densely bipartite, and not very informative to look at) connections to the
  input. Own section below.

`rosom` isn't supported by either yet — `(net rosom)` doesn't match what either method assumes, same
situation as `save` (`write-dot` errors clearly rather than misreading it). `hopfield`/`auto-assoc` use
the generic method as-is; a grid-positioned layout for them (their cells don't have an inherent 2D
position, but historically got displayed on one — see `doc/auto-assoc-exemples.md`) hasn't been built.

```lisp
(write-dot xor)   ;; writes xor.dot in the current directory, DOT source only
```

### Coloring edges by weight

The generic method takes `:color` (nil by default) and `:label` (t by default): color edges red
(positive) / blue (negative), normalized by the net's largest `|weight|` — the same convention as the
heatmap in `src/gui.lisp` — instead of, or alongside, the numeric label:

```lisp
(write-dot xor "xor.png" :format :png :color t)                 ;; colored AND labeled
(write-dot xor "xor.png" :format :png :color t :label nil)      ;; colored only, no text
```

### Rendering to PNG or SVG directly

Pass `:format`, and optionally `:engine` (any Graphviz layout engine — `"dot"`, the default for the
generic method, lays out hierarchically; `"neato"`, `"fdp"`, `"sfdp"` lay out by force; `"circo"`,
`"twopi"` lay out radially). `write-dot` writes the DOT source to a temporary file, renders it with the
external engine (`sb-ext:run-program`, so this needs SBCL and a working Graphviz install —
`apt install graphviz` on Debian), and deletes the temporary file once the render succeeds:

```lisp
(write-dot xor "xor.png" :format :png)    ;; dot -Tpng, hierarchical layout
(write-dot xor "xor.svg" :format :svg)    ;; same, SVG
```

If the engine can't be found or fails, `write-dot` warns and keeps the `.dot` file instead of silently
losing your run — nothing is lost, you can always render it by hand afterward:

```sh
dot -Tpng xor.dot -o xor.png
```

`:format :dot` (the default) never touches an external process at all — the DOT text itself is always
the portable fallback, on any Lisp, with any Graphviz engine, now or later.

### Demo: `xor`

The network built and trained in [MLP-exemples.md](MLP-exemples.md):

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

### SOM: the grid instead of the connections

A som's `write-dot` method takes `:mode`, `:component`, `:labels`, and `:scale` in place of `:color`,
and defaults `:engine` to `"neato"` instead of `"dot"` — a different method, not the generic one with
different defaults. It draws `(grid-neighbour-pairs ann)` (`src/som.lisp`: unique pairs of neurons that
are actual grid neighbours — 4-connected for a `:square` lattice, 6-connected for `:hex`, whichever
`(topology my-som)` says — not the input connections `edges` would give) at **fixed positions**
(`pos="x,y!"`, which `neato` honors directly — hence the engine default) instead of letting the engine
lay them out:

```lisp
(make-instance 'som :name 'my-som :size 144 :input 18)
;; ... train as in SOM-exemples.md ...
(write-dot my-som "map.png" :format :png)                       ;; U-matrix (default mode)
(write-dot my-som "plane-3.png" :format :png :mode :component :component 3)  ;; one of 18 planes
```

**`:mode :u-matrix`** (the default) colors each edge, grayscale, by the distance between the two
neurons' weight vectors (`(euclidian (weights n1) (weights n2))`) — dark edges mark cluster boundaries.
Each node is colored by its *average* distance to its grid neighbours, same scale.

**`:mode :component`** needs `:component <k>`, the index of one input dimension (0 to `(1- (in-size
my-som))`): colors each node, white to blue, by that one weight component — one of `(in-size my-som)`
"component planes," showing how a single input dimension is distributed across the map. Call once per
dimension (`(dotimes (k (in-size my-som)) (write-dot my-som (format nil "plane-~D.png" k) :format :png
:mode :component :component k))` for all of them at once) — edges stay drawn, undecorated, just for
reference.

**`:labels`** (nil by default) annotates each node with a value you supply — a sequence as long as
`(net my-som)`, in that order. Nothing in `som.lisp` keeps a history of which neuron won how often (same
reason the live viewer in `src/gui.lisp` accumulates its own `winner-history` rather than reading one
off the network), so a best-matching-unit count has to be tallied by the caller first, e.g. with
`inventaire-h` (`src/maths.lisp`) over a logged sequence of winners, then passed in:

```lisp
(write-dot my-som "map.png" :format :png :labels (mapcar #'second (inventaire-h winner-log)))
```

A `:boundary :torus` map's wraparound edges (first and last neuron of a row/column) still get drawn —
`grid-neighbour-pairs` computes the topology correctly either way — but on this flat layout they end up
collinear with, and so visually indistinguishable from, the ordinary edges along that same row/column:
the topology is right, the picture just doesn't make the wraparound obvious.

---

## Further Reading

- [EXAMPLES.md](EXAMPLES.md) — general training workflow.
- [MLP-exemples.md](MLP-exemples.md), [SOM-exemples.md](SOM-exemples.md) — the `xor`/`my-som` networks
  used above.
- `src/neuromuse-main.lisp` — `nodes`/`edges`, the generic graph view the mlp/rmlp/perceptron/hopfield/
  auto-assoc `write-dot` method reads.
- `src/som.lisp` — `grid-neighbour-pairs`, the grid topology the som `write-dot` method reads.
- `src/read-write.lisp` — `save`, `write-dot` (both methods), `trace-activation`/`trace-output`, full
  docstrings.
