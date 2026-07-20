# `rmlp`: Elman-style, not Jordan-style

`rmlp` (`src/mlp.lisp`, subclass of `mlp`) is neuromuse's recurrent multi-layer perceptron. Its
class docstring already says "Elman Recurrent Multi-Layer Perceptron", and this note records how
that was verified — both by reading the code and by an empirical test — for anyone wondering
whether it instead behaves like a Jordan network.

## The difference

Both architectures add a "context" vector to the input on the next time step, computed from the
previous time step's activity:

- **Elman network**: the context vector is a copy of a **hidden** layer's activation.
- **Jordan network**: the context vector is a copy of the **output** layer's activation (often
  with a self-connection/decay term).

These are structurally distinguishable without even running anything: an Elman context vector has
the dimension of the chosen hidden layer, while a Jordan context vector has the dimension of the
network's output layer.

## What the code does

`rmlp` adds two slots on top of `mlp` (`src/mlp.lisp`):

```lisp
(defclass rMLP (mlp)
  ((recurrent-layer ...)             ; which hidden layer to feed back
   (recurrent-layer-activation ...)) ; the fed-back activation itself
  (:documentation "Elman Recurrent Multi-Layer Perceptron."))
```

In both `run-mlp` and `backpropagate` for `rmlp`, the current input is the network's own `input`
slot concatenated with `recurrent-layer-activation` from the previous step:

```lisp
(input (if in in (append (input mlp) (recurrent-layer-activation mlp))))
```

and after computing each hidden layer's activation during the forward pass, the layer whose index
matches `(recurrent-layer mlp)` is snapshotted into that slot for next time:

```lisp
(when (= down (recurrent-layer mlp))
  (setf (recurrent-layer-activation mlp) (car hidden-answer-cell)))
```

This is captured straight from `hidden-func`'s output at a chosen **hidden** layer — the actual
output vector (`(output mlp)`, produced afterwards by `out-func` from the *last* hidden layer) is
never involved. That's the Elman definition.

## Verifying it structurally

Build an `rmlp` where the recurrent hidden layer's size differs from the output size, run one
step, and compare vector lengths:

```lisp
(make-rmlp probe 6 1 0 4)   ; 6 in, 1 out, hidden layer 0 (size 4) is the recurrent layer
(setf (learn-fact probe) 0.3
      (input probe) '(1 0 1 0 1 0)
      (goal probe) '(1))
(backpropagate probe)

(length (recurrent-layer-activation probe))  ; => 4  (matches hidden-size, not out-size)
(out-size probe)                             ; => 1
```

Sample run:

```
out-size of probe:                    1
hidden-size of probe (layer 0):       4
(output probe):                       (0.445528)
(recurrent-layer-activation probe):   (0.5483496 0.34182772 0.66144615 0.6124841)
```

The fed-back vector has length 4, not 1 — it cannot be a copy of the (length-1) output. If `rmlp`
were Jordan-style, `recurrent-layer-activation` would always have the same length as `(output
mlp)`, regardless of which hidden layer's size is used.

## Verifying it behaviorally

Structure alone shows *what* is fed back, not whether the network actually uses it as working
memory. A 1-step delayed-copy task tests that directly: given a sequence of independent random
bits, predict `target(t) = input(t-1)`. The current input carries **zero** information about the
target (the bits are i.i.d.), so a network with no memory of the past can't beat chance (50%);
only a network that actually carries state across time steps can do better.

Trained a plain `mlp` (1-2-3 style hidden layer, no recurrence) and an `rmlp` (hidden layer 0
recurrent) on the same task, 40 passes over a 400-bit training sequence, then tested on a fresh
400-bit sequence:

```
plain MLP  (no memory) accuracy on delayed-copy task: 46.9%   (chance level ~= 50%)
recurrent rmlp (Elman) accuracy on delayed-copy task: 90.0%
```

The plain MLP sits at chance, as expected. The `rmlp` reaches 90%, confirming its recurrent
hidden-layer feedback functions as real short-term memory across time steps — the property Elman
and Jordan networks both share, even though the specific wiring here is Elman's.

## Conclusion

- `rmlp` is Elman-style: it feeds back a **hidden** layer's activation, not the output layer's.
- It genuinely behaves as a recurrent network: it solves a task that requires memory of the
  previous time step, which a plain (non-recurrent) `mlp` cannot.
- Training is standard backprop on the input+context vector (as in the original Elman 1990
  formulation), not backpropagation-through-time — the context slot is simply treated as extra
  input units for the current step's weight update.
- A Jordan-style variant would require capturing `(output mlp)` into the feedback slot instead of
  a hidden layer's activation; not implemented here.

Both test scripts above are reproducible at the REPL after `(asdf:load-system :neuromuse)` and
`(in-package :neuromuse)`.
