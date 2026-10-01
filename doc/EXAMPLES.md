# Examples: Training Neural Networks with neuromuse

This guide covers the general, architecture-agnostic workflow and Tips & Tricks. For
architecture-specific walkthroughs:

- **[MLP-exemples.md](MLP-exemples.md)** — XOR (and why its "expected output" isn't reliable without a
  few fixes), accelerometer data (6D input), recurrent MLP (rMLP), watching training live with
  `neuromuse-gui`.
- **[SOM-exemples.md](SOM-exemples.md)** — building/training a SOM, finding winners, a full worked
  example (chaffinch birdsong clustering), watching it train live.
- **[auto-assoc-exemples.md](auto-assoc-exemples.md)** — building/training an auto-associative memory,
  pattern completion, a worked example with real performance history (choreographic analysis for
  *L'Écarlate*, 2001), watching it run live.

---

## Custom Problems: General Workflow

To train a network on *your* problem:

1. **Collect data:** A list of input vectors and a list of matching goal outputs.
   ```lisp
   (defvar *my-inputs* '((x1 y1 z1) (x2 y2 z2) ...))
   (defvar *my-goals*  '((target1) (target2) ...))
   ```

2. **Choose an architecture:** MLP for supervised learning, SOM for clustering, rMLP for sequences.
   ```lisp
   (make-mlp mynet (length (car *my-inputs*)) (length (car *my-goals*)) 4 2)
   ;; Inputs → hidden layer of 4 → hidden layer of 2 → outputs
   ```

3. **Normalize data:** Neural networks train faster if inputs are scaled to ~[0, 1] or [-1, 1].
   ```lisp
   (setf *my-inputs* (mapcar #'(lambda (x) (mapcar #'(lambda (v) (/ v 255.0)) x)) *my-inputs*))
   ```

4. **Train:** Loop until error is acceptable.
   ```lisp
   (let ((net mynet) (e 999999))
     (setf (learn-fact net) 0.1 (threshold net) 0.05)
     (loop until (< e (threshold net))
           do (loop for i below (length *my-inputs*)
                    do (setf (input net) (nth i *my-inputs*)
                             (goal net) (nth i *my-goals*))
                       (learn net)
                       (setf e (current-error net)))))
   ```

5. **Test:** Run without learning.
   ```lisp
   (dolist (input *my-inputs*)
     (setf (input mynet) input)
     (run-mlp mynet)
     (format t "Input: ~S → Output: ~S~&" input (output mynet)))
   ```

---

## Tips & Tricks

### Learning Rate (`learn-fact`)

- Too high → network oscillates, never converges.
- Too low → training is very slow.
- Start with 0.1–0.5 and adjust based on error curves.

### Initialization

Fresh networks have random weights. Running again may give different results. To control randomness:
```lisp
(setf *random-state* (make-random-state t))  ;; Seed with time
(setf *random-state* (make-random-state 42)) ;; Reproducible: seed 42
```

### Overfitting

If training error drops but test error rises, the network is overfitting. Solutions:
- Use more training data.
- Add noise (`temp` parameter).
- Simplify the network (fewer hidden units).

### Batch vs. Online Learning

The examples above use *online* learning (update after each sample). For *batch* learning:
```lisp
(loop for epoch from 0 to max-epochs
      do (let ((total-error 0.0))
           (loop for i below (length *inputs*)
                 do (setf (input net) (nth i *inputs*)
                          (goal net) (nth i *goals*))
                    (learn net)
                    (incf total-error (current-error net)))
           (when (< total-error threshold)
             (return))))
```

---

## Further Reading

- See [MLP-exemples.md](MLP-exemples.md) for MLP/rMLP walkthroughs.
- See [SOM-exemples.md](SOM-exemples.md) for SOM walkthroughs.
- See `src/mlp.lisp` for the full MLP implementation and docstrings.
- See `src/som.lisp` for SOM details.
- See `src/rosom.lisp` for recurrent oscillatory SOMs.
- See [PHILOSOPHY.md](../doc/PHILOSOPHY.md) for the conceptual framework.
