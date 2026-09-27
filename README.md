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

1. [Quick Start]([#quick-start)
2. [Requirements](https://github.com/FredVoisin/neuromuse#requirements)
3. [What's New]([#whats-new))
4. [Documentation]([#documentation))
5. [FAQ](#faq)
6. [Contributing & TO-DO](#contributing--to-do)
7. [License](#license)



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

3. Start Emacs + SLIME and load the system:
   ```lisp
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
    (backpropagate xor)))

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
- **[PHILOSOPHY.md](doc/PHILOSOPHY.md)** — Why neuromuse exists (Nice 2005 presentation, français & English)
- **[HISTORY.md](doc/HISTORY.md)** — 25 years of development, key collaborations, artistic applications
- **[GALLERY.md](doc/GALLERY.md)** — Visual history: early experiments, performances, research projects
- **[PUBLICATIONS.md](doc/PUBLICATIONS.md)** — Academic papers and references

## Available Architectures

- **MLP** — Multi-layer perceptron with backpropagation; classic supervised learning
- **SOM** — Kohonen self-organizing maps for unsupervised clustering and pattern recognition
- **ROSOM** — Recurrent oscillatory SOM (Elman-style feedback); captures temporal patterns
- **Perceptron** — Single-layer classifier (minimal but complete)
- ...

## FAQ

**Can I use this for real-time music applications?**  
Yes. The list-based matrix code is ~40× faster than GPU-accelerated alternatives at neuromuse's actual network sizes (a handful of neurons). See the benchmark in [examples/benchmark-mgl-mat.lisp](examples/benchmark-mgl-mat.lisp). If it runs on any CPU with large models, the code can also by easily adapted for GPU (see below, Contributing).

**Can I use this for teaching AI?**
Yes, you can. Freely. Suggestions and contributions are welcome (see [philosophy](doc/PHILOSOPHY.md))

**Is the documentation only in code comments?**  
Historically, yes. We're actively expanding it. Start with a 5-minute read of [PHILOSOPHY.md](doc/PHILOSOPHY.md) to understand the mindset, then dive into [EXAMPLES.md](doc/EXAMPLES.md).
The 'we' being Fred - the author - with the help of claude.ia (Anthropic), since Sept. 27 - collaborators are welcome. When needed,
to avoid confusion, the ai generated parts of the documentation or of the code are flagged or mention as being ai-generated (claude.ia).
By the way, the [legacy code](https://github.com/FredVoisin/neuromuse/tree/master/legacy) was learned by claude.ai under the supervision of the author, who ensures that the contribution is relevant. And Fred know for 
such an ai is the tool to explore and play such complex systems as the ones to be built with neuromuse, how big or small they can be.

**Why Lisp?**
Because it's the second mother language of the author, learned at Ircam undergound when underground when last was on the roof. But not only :)

**Is `rmlp` Elman or Jordan?**  
Elman. Its recurrent feedback comes from a hidden layer's activation, not the output layer's. See [doc/rmlp-elman-vs-jordan.md](doc/rmlp-elman-vs-jordan.md) for a structural and behavioral test. This question because the loops levels could be confusing while digging ;)


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
