# neuromuse

**Artificial neural networks for music and artistic production with real-time applications** (since 1999)

## Overview

Neuromuse is a Common Lisp library for designing and training artificial neural networks tailored to creative and research contexts. Originally developed by Fred Voisin at IRCAM to explore how symbolic neural systems could serve contemporary music composition, the project combines rigorous machine learning fundamentals with an artist's sensibility for real-time applications.

Rather than optimizing for raw computational speed, neuromuse prioritizes **clarity, adaptability, and artistic control**. Its architectures (Multi-Layer Perceptron, Self-Organizing Maps, recurrent variants) are kept intentionally simple so that both artists and researchers can understand the internal mechanisms, train networks live in a REPL, and integrate them into their work—whether that's generative composition, gesture-to-sound mapping, or live performance.

### Why neuromuse?

Traditional AI frameworks treat neural networks as black boxes to be optimized. Neuromuse inverts that: the *process* of learning and adaptation is itself the artwork, observable and controllable at every step. This philosophy emerged from Fred Voisin's work with choreographers and composers who needed systems that could:

- Learn from real human gesture and movement in real time
- Adapt to incomplete, contradictory, or intuitive input without requiring perfect labeling
- Remain interpretable—understand *why* a network makes a decision, or not
- Run on modest hardware (historically, a 68030 or PowerPC laptop; today - in 2026, even an iPhone 5s via neuromuse-ios)
- It's opensource, no dependencies, diy

## Table of Contents

1. [FAQ](#faq)
2. [Quick Start](#quick-start)
3. [Requirements](#requirements)
4. [What's New](#whats-new)
5. [Documentation](#documentation)
6. [Contributing & TO-DO](#contributing--to-do)
7. [License](#license)

## FAQ

- **Can I use this for real-time music applications?**  
Yes. The list-based matrix code is ~40× faster than GPU-accelerated alternatives at neuromuse's actual network sizes. See the benchmark in [examples/benchmark-mgl-mat.lisp](examples/benchmark-mgl-mat.lisp). If it runs on any CPU with large models, the code can also by easily adapted for GPU (see below, Contributing).

- **Can I use this for teaching AI?**  
Yes, you can. Freely. Suggestions and contributions are welcome!

- **Is the documentation only in code comments?**  
Historically, yes, and also by nature (see below: Why Lisp?). We're actively expanding it. Start with a 5-minute read of [PHILOSOPHY.md](doc/PHILOSOPHY.md) to understand the mindset, then dive into [EXAMPLES.md](doc/EXAMPLES.md).

- **Who is we?**  
Since Sept. 27, the 'we' above is Fred - the author - with the help of claude.ia (Anthropic). Collaborators are welcome! To avoid confusion, the AI generated parts of the documentation or of the code are flagged or mentioned as being AI-generated. The [legacy code](https://github.com/FredVoisin/neuromuse/tree/master/legacy) which is the reference has been learned by claude.ai under the supervision of the author who ensures that all contributions are relevant and amended when needed. Fred knows from experience that AI can assist in exploring and working with complex systems like neuromuse — whether they grow large or stay small.

- **Why Lisp?**  
Because it's the second mother language of the author, learned at Ircam underground when the last was on the roof, but not only. In Lisp, code and data share the same structure: a function call such as `(+ 1 2)` is simply a list, which a program (a list) can build, inspect or rewrite before evaluating it. This property, called homoiconicity, comes from Church's [Lambda Calculus](https://plato.stanford.edu/entries/lambda-calculus/) and [McCarthy's Lisp](https://www-formal.stanford.edu/jmc/recursive.html). It makes it natural to treat activation functions, learning rules, and even the shape of a network as objects one can compose and reshape while the system is learning. We hope to develop this property further, which allows code to observe and generate itself. Incidentally, this also makes the code its own documentation.

- **Is `rmlp` Elman or Jordan?**   
Elman. Its recurrent feedback comes from a hidden layer's activation, not the output layer's. See [doc/rmlp-elman-vs-jordan.md](doc/rmlp-elman-vs-jordan.md) for a structural and behavioral test. This question because the loops levels could be confusing while digging ;)


## Quick Start

### Install

1. Clone the repository:
   ```bash
   git clone https://github.com/FredVoisin/neuromuse.git ~/projects/neuromuse
   ```

2. Make it visible to ASDF. Link it to Quicklisp's `local-projects`:
   ```bash
   ln -s ~/projects/neuromuse ~/quicklisp/local-projects/neuromuse
   ```
   (or see [INSTALLATION.md](doc/INSTALLATION.md) for alternative setup)

3. Start Emacs + SLIME and load the system. The symlink alone isn't enough the first time —
   Quicklisp caches which systems live under `local-projects/` and won't see the new symlink until
   that cache is rebuilt, so `(ql:quickload :neuromuse)` on its own fails with `System "neuromuse" not
   found` right after step 2. Run `(ql:register-local-projects)` once to rebuild it, then quickload:
   ```lisp
   (ql:register-local-projects)   ;; only needed once after the symlink is created/moved
   (ql:quickload :neuromuse)
   (in-package :neuromuse)
   ```

### Train an MLP on XOR in 30 seconds

XOR is the canonical test: a simple problem that proves backpropagation and hidden layers actually work.
With only 2 hidden units and a fixed presentation order, plain backprop on XOR is a textbook case for
getting stuck in a symmetric local minimum — the snippet below seeds the random state, uses 3 hidden
units, and jitters the weights a little (`net-temp`) during training to reliably escape it; see
[EXAMPLES.md](doc/EXAMPLES.md) for the unabridged story, including what goes wrong without these.

```lisp
;; 1. Define training data
(defvar *xor-in*   '((0 0) (1 0) (0 1) (1 1)))
(defvar *xor-goal* '((0)   (1)   (1)   (0)))

;; 2. Build a 3-hidden-unit network (2 inputs → 3 hidden → 1 output).
;;    Seeding the RNG makes this reproduce the exact output below, on SBCL,
;;    every time.
(setf *random-state* (sb-ext:seed-random-state 123))
(make-mlp xor 2 1 3)

;; 3. Configure and train: learn-fact is the learning rate, net-temp a
;;    little synaptic noise that helps backprop escape XOR's local minima.
(setf (learn-fact xor) 0.4
      (net-temp xor) 0.05)

(dotimes (epoch 20000)
  (dotimes (i (length *xor-in*))
    (setf (input xor) (nth i *xor-in*)
          (goal xor) (nth i *xor-goal*))
    (learn xor)))

;; 4. Test (net-temp back to 0 first, or run-mlp would jitter the weights
;;    it reads)
(setf (net-temp xor) 0)
(dolist (input *xor-in*)
  (setf (input xor) input)
  (run-mlp xor)
  (format t "~S → ~S~&" input (apply #'round (output xor))))
```

Expected output: `(0 0) → 0`, `(1 0) → 1`, `(0 1) → 1`, `(1 1) → 0`. Runs in well under a second.

For a full walkthrough and more examples, see [EXAMPLES.md](doc/EXAMPLES.md).

## Requirements

- **SBCL** 2.0+ ([http://www.sbcl.org](http://www.sbcl.org))
- **Quicklisp** ([https://www.quicklisp.org](https://www.quicklisp.org)) — fetches `:prove` for the test suite
- **Emacs + SLIME** (recommended for interactive development; see [INSTALLATION.md](doc/INSTALLATION.md) for details)

## What's New

**Sept 2026** — Perceptron added with demos, MLP demos, Ltk-based GUI looking like legacy versions.

**July 2026** — The project has been reorganized as a proper ASDF system with:
- Code moved into a dedicated `:neuromuse` package
- Proper directory layout: `src/`, `tests/`, `examples/`, `doc/`
- Long-standing bugs fixed: instance saving, network constructors, UDP SOM input
- A real test suite (using `:prove`)

## Documentation

- **[INSTALLATION.md](doc/INSTALLATION.md)** — Environment setup, Emacs/SLIME workflow, troubleshooting
- **[EXAMPLES.md](doc/EXAMPLES.md)** — Detailed walkthroughs: XOR, accelerometer data, custom problems
- **[PHILOSOPHY.md](doc/PHILOSOPHY.md)** ([français](doc/PHILOSOPHY.fr.md)) — Why neuromuse exists (Nice 2005 presentation)
- **[HISTORY.md](doc/HISTORY.md)** — 25 years of development, key collaborations, artistic applications
- **[GALLERY.md](doc/GALLERY.md)** — Visual history: early experiments, performances, research projects
- **[PUBLICATIONS.md](doc/PUBLICATIONS.md)** — Academic papers and references

## Available Architectures

- **MLP** — Multi-layer perceptron with backpropagation; classic supervised learning
- **SOM** — Kohonen self-organizing maps for unsupervised clustering and pattern recognition
- **ROSOM** — Recurrent oscillatory SOM (Elman-style feedback); captures temporal patterns
- **Perceptron** — Single-layer classifier (minimal but complete)
- ...


## Contributing & TO-DO

Open issues and PRs welcome. Known items:

- Port graphical network rendering (originally built on Macintosh Common Lisp's CLOS toolbox) to SBCL —
  **en cours**: a first Ltk-based version lives in `src/gui.lisp` (heatmap and neuron-graph views, live activity)
- Extend `rmlp` to support Jordan-style recurrence alongside Elman
- Complete the `perceptron.lisp` implementation
- Investigate GPU optimization under modern CUDA (previous attempts hit platform limits)

## License

GPL-3.0. See [LICENSE](LICENSE).

---

**Curious about the history?** Start with [HISTORY.md](doc/HISTORY.md).  
**Want to understand the philosophy?** Read [PHILOSOPHY.md](doc/PHILOSOPHY.md).  
**Ready to code?** Jump to [INSTALLATION.md](doc/INSTALLATION.md) and [EXAMPLES.md](doc/EXAMPLES.md).
