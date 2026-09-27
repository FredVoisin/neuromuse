# Examples: Training Neural Networks with neuromuse

This guide walks through detailed examples, starting with the minimal XOR case and moving to real-world problems.

## 1. Training an MLP on XOR

**File:** `examples/mlp-test.lisp`

XOR (exclusive OR) is a classic problem: given two binary inputs, output 1 if exactly one is 1, else 0. It's linearly inseparable, so a single-layer network fails—you need hidden units and backpropagation.

### The Data

```lisp
(defvar *xor-in*   '((0 0) (1 0) (0 1) (1 1)))
(defvar *xor-goal* '((0)   (1)   (1)   (0)))
```

Each input is a list (e.g., `(1 0)`), each goal is its target output.

### Build the Network

```lisp
(make-mlp xor 2 1 2)
```

Expands to:
```lisp
(defvar xor (make-instance 'mlp :in-size 2 :out-size 1 :hidden-sizes '(2)))
```

This creates a multi-layer perceptron with:
- 2 input neurons
- 1 hidden layer of 2 neurons
- 1 output neuron

### Configure Learning Parameters

```lisp
(let ((net xor))
  (setf (learn-fact net) 0.4
        (temp net) 0.1
        (verbose net) t
        (threshold net) 0.1
        (input net) (car *xor-in*)
        (goal net) (car *xor-goal*)))
```

- **`learn-fact`** (learning rate): Controls how much weights change per epoch. Default is 0.0 (no learning), so you must set it.
- **`temp`** (temperature): Adds stochastic noise to prevent local minima.
- **`verbose`**: Print progress during training.
- **`threshold`**: Stop training when error drops below this.

### Train the Network

```lisp
(let ((net xor) (in *xor-in*) (goal *xor-goal*) (e 999999))
  (loop until (< e (threshold net))
        do (loop for i from 0 to (- (length in) 2)
                 do (setf (input net) (nth i in)
                          (goal net) (nth i goal)
                          e (backpropagate net))
                    (format t "~&epoch ~S, e = ~S" (epoch net) e))))
```

- **`backpropagate`**: One forward pass + one weight update. Returns the error for the current input/goal pair.
- **`epoch`**: Number of training iterations so far.

*Note:* The inner loop bound `(- (length in) 2)` visits only 3 of 4 patterns per epoch (a quirk to know about if comparing curves).

### Test the Result

```lisp
(dolist (input *xor-in*)
  (setf (input xor) input)
  (run-mlp xor)
  (format t "~S : ~S~&" input (apply #'round (output xor))))
```

- **`run-mlp`**: Forward pass only (no learning).
- **`output`**: Result after forward pass, a list like `(0.98)` or `(0.02)`.
- **`round`**: Convert to nearest integer.

Expected output:
```
(0 0) : 0
(1 0) : 1
(0 1) : 1
(1 1) : 0
```

---

## 2. Accelerometer Data (6D Input)

**File:** `examples/mlp-test2.lisp`

For problems with more inputs, the structure is identical—just change the data and network shape.

```lisp
(defvar *accel-in*   '((0.1 0.2 0.0 ...) ...))  ;; 6-dimensional vectors
(defvar *accel-goal* '((1) (0) (1) ...))        ;; Binary labels (e.g., "gesture detected")

(make-mlp accel 6 1 8)  ;; 6 inputs → 8 hidden → 1 output
```

All training/testing code above applies unchanged. Swap in accelerometer data, resize the network, and you're training a 6D classifier.

---

## 3. Self-Organizing Maps (SOM)

Self-organizing maps learn to cluster high-dimensional data without labels. They're useful for exploratory analysis and feature discovery.

**Basic example:**

```lisp
(make-som mysmap 3 10 10)  ;; 3-D input, 10×10 map grid
```

Train it:
```lisp
(let ((som mysmap))
  (setf (learn-fact som) 0.1
        (sigma som) 2.0)  ;; Neighborhood radius
  (loop for epoch from 1 to 1000
        do (let ((input (random-vector-from-data)))
             (setf (input som) input)
             (train-som som))))
```

After training, the map's neurons have learned to respond to different regions of input space. Nearby neurons respond to similar inputs (the "self-organizing" property).

---

## 4. Recurrent MLP (rMLP)

Recurrent networks have feedback connections, letting them process temporal sequences.

**Elman architecture (feedback from hidden layer):**

```lisp
(make-rmlp rnet 2 1 4)  ;; Similar signature to make-mlp
```

Train on a sequence:

```lisp
(let ((net rnet))
  (setf (learn-fact net) 0.3)
  (loop for t from 0 to (length sequence)
        do (setf (input net) (nth t sequence)
                 (goal net) (nth (+ t 1) sequence))  ;; Predict next step
            (backpropagate net)))
```

The recurrent connections let the network "remember" recent inputs when predicting the next one. See [doc/rmlp-elman-vs-jordan.md](../doc/rmlp-elman-vs-jordan.md) for details.

---

## 5. Custom Problems: General Workflow

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
   (let ((net mynet))
     (setf (learn-fact net) 0.1 (threshold net) 0.05)
     (loop until (< e (threshold net))
           do (loop for i below (length *my-inputs*)
                    do (setf (input net) (nth i *my-inputs*)
                             (goal net) (nth i *my-goals*)
                             e (backpropagate net)))))
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
                          (goal net) (nth i *goals*)
                          total-error (+ total-error (backpropagate net))))
           (when (< total-error threshold)
             (return))))
```

---

## Further Reading

- See `src/mlp.lisp` for the full MLP implementation and docstrings.
- See `src/som.lisp` for SOM details.
- See `src/rosom.lisp` for recurrent oscillatory SOMs.
- See [PHILOSOPHY.md](../doc/PHILOSOPHY.md) for the conceptual framework.
