# Installation & Setup

## Requirements

- **SBCL** 2.0+ ([http://www.sbcl.org](http://www.sbcl.org)) — tested on Linux, macOS, Windows
- **Quicklisp** ([https://www.quicklisp.org](https://www.quicklisp.org)) — *recommended*, not strictly
  required; see [With or without Quicklisp](#with-or-without-quicklisp) below for what it buys you and
  how to skip it
- **Emacs** with **SLIME** ([https://slime.common-lisp.dev/](https://slime.common-lisp.dev/)) — recommended for interactive development
- **Git** — to clone the repository

### With or without Quicklisp

The core library (`:neuromuse` — MLP, SOM, ROSOM, perceptron, UDP) depends on nothing but SBCL itself:
`neuromuse.asd` lists only `:sb-bsd-sockets`, a contrib module built into SBCL and resolved via `require`,
not fetched from anywhere. Plain ASDF (also built into SBCL) is enough to load it — no Quicklisp needed.

Quicklisp is what gets you three things beyond that, each pulled in on demand, not up front:
- **The test suite** — `neuromuse-test` depends on `:prove`, a real third-party library that has to come
  from somewhere; Quicklisp is that somewhere.
- **The optional GUI** — `neuromuse/gui` depends on `:ltk`, likewise fetched via Quicklisp. It's a
  separate ASDF system on top of `:neuromuse`, loaded with `(ql:quickload :ltk)` then
  `(asdf:load-system "neuromuse/gui")` (needs Tk itself too — Debian/Ubuntu: `sudo apt install tk`).
  Calling `neuromuse-gui:gui` before that fails with `Package "NEUROMUSE-GUI" not found`. See
  [MLP-exemples.md](MLP-exemples.md#watching-it-train-live-neuromuse-gui-and-the-noisy-xor-demos) for
  it in use, or [SOM-exemples.md](SOM-exemples.md#5-watching-it-train-live-the-som-viewer) for the SOM
  viewer.
- **Convenience** — symlink the repo into `~/quicklisp/local-projects/` once (step 4 below) and
  `(ql:quickload :neuromuse)` finds it by name from any directory, no path to remember or push onto
  `asdf:*central-registry*` by hand.

Skip Quicklisp entirely and you still get the full core library at the REPL — `make-mlp`, `learn`,
`run-mlp`, everything in `examples/mlp-test.lisp` — just not the test suite or the GUI window, short of
fetching `:prove` and `:ltk` by some other means and registering them with ASDF yourself. Steps 4 and 5
below show both paths side by side.

## Step-by-Step Installation

### 1. Install SBCL

**Linux (Debian/Ubuntu):**
```bash
sudo apt-get install sbcl
```

**macOS (via Homebrew):**
```bash
brew install sbcl
```

**Windows & others, including macOS x86_64:** See [http://www.sbcl.org/platform-table.html](http://www.sbcl.org/platform-table.html)

### 2. Install Quicklisp (recommended — skip to step 3 if you're going without)

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
```

This installs Quicklisp to `~/quicklisp/`. To have it available in every future SBCL session, load it
from SBCL's own init file — **not** a shell init file, and not as a shell command (`sbcl --load ...` in
`.bashrc`/`.zshrc` would try to start an interactive SBCL REPL every time you open a terminal). Right
after the install above, from the same SBCL session, let Quicklisp add itself to `~/.sbclrc`:
```lisp
(ql:add-to-init-file)
```
This appends the standard, defensive form — guarded so it's a no-op if Quicklisp isn't installed on a
given machine, and safe to copy verbatim to `~/.sbclrc` by hand instead if you'd rather not run it:
```lisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
```
Skipping this step is the most common way "starting from scratch" breaks: every command below that
starts with `ql:` (`ql:quickload`, `ql:add-to-init-file`, ...) needs Quicklisp loaded first, in *that*
SBCL session — a fresh `sbcl` with nothing loaded doesn't have the `QL` package and fails immediately
with `Package QL does not exist`.

### 3. Clone neuromuse

```bash
git clone https://github.com/FredVoisin/neuromuse.git ~/projects/neuromuse
cd ~/projects/neuromuse
```

### 4. Make neuromuse visible to ASDF

**Option A: Symlink (if you installed Quicklisp)**
```bash
ln -s ~/projects/neuromuse ~/quicklisp/local-projects/neuromuse
```

**Option B: Manual registry (works either way — required if you skipped Quicklisp)**, from the SBCL REPL:
```lisp
(require :asdf)   ;; built into SBCL, but not auto-loaded without Quicklisp around to have required it
(push #P"~/projects/neuromuse/" asdf:*central-registry*)
```

### 5. Load the system

**With Quicklisp** (found automatically via the step 4 symlink):
```lisp
(ql:quickload :neuromuse)
(in-package :neuromuse)
```

**Without Quicklisp** (needs the step 4 Option B registry push first, this session or every session):
```lisp
(require :asdf)                  ;; built into SBCL, no Quicklisp involved
(asdf:load-system :neuromuse)
(in-package :neuromuse)
```

Either way you land in the same place — same package, same `make-mlp`/`learn`/`run-mlp`. The
test suite and the GUI, though, both need something Quicklisp fetches (`:prove`, `:ltk`) and so only
work with Quicklisp installed:
```lisp
(ql:quickload :prove)          ;; first time only -- asdf:test-system won't fetch it on its own
(asdf:test-system :neuromuse)
```

## Emacs + SLIME Workflow

### Install SLIME

If you use Emacs but don't have SLIME:
```bash
git clone https://github.com/slime/slime.git ~/.emacs.d/slime
```

Add to your `.emacs` or `init.el`:
```elisp
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
```

### Start a REPL

In Emacs:
```
M-x slime
```

A new buffer opens with a live Common Lisp REPL. From here you can:

```lisp
(ql:quickload :neuromuse)
(in-package :neuromuse)
```

### Day-to-Day Commands

Once a `.lisp` file is open in Emacs:

| Key binding | Action |
|---|---|
| `C-c C-k` | Compile/load the entire file into the REPL |
| `C-c C-c` | Compile the top-level form under point |
| `C-c C-z` | Jump to the REPL buffer |
| `C-c C-d d` | Show documentation for the symbol at point |

**Live editing:** When you redefine a function or method and recompile it, the running Lisp image updates immediately. No restart needed.

### Quick Test

In the REPL:
```lisp
(load "examples/mlp-test.lisp")
```

You should see training epochs and then a final test showing XOR results.

## Troubleshooting

**`Package QL does not exist`**

Quicklisp isn't loaded in this SBCL session. Either it was never added to `~/.sbclrc` (see step 2
above — a shell init file doesn't do this), or you're running a mode that skips init files (e.g.
`sbcl --script`, which always does, on purpose). Load it directly to confirm the diagnosis, then fix
`~/.sbclrc` if that's the real cause:
```lisp
(load "~/quicklisp/setup.lisp")
```

**`Error: cannot find neuromuse` when loading**

Make sure the symlink or registry path is correct:
```lisp
(asdf:locate-system :neuromuse)  ;; Should return the path to neuromuse.asd
```

If nil, check your symlink or re-add the path to `asdf:*central-registry*`.

**SLIME not connecting to SBCL**

Ensure `inferior-lisp-program` is set in Emacs:
```elisp
(setq inferior-lisp-program "sbcl")
```

Then restart SLIME: `M-x slime-quit` followed by `M-x slime`.

**`Component :PROVE not found` when testing**

`asdf:test-system` doesn't fetch missing dependencies by itself. Prime `:prove` via Quicklisp first
(needs to be online the first time):
```lisp
(ql:quickload :prove)
(asdf:test-system :neuromuse)
```

**Tests fail or hang**

Check the SBCL version (`sbcl --version`). Neuromuse is tested on SBCL 2.0+. If you're on an older version, update:
```bash
brew upgrade sbcl          ;; macOS
sudo apt-get upgrade sbcl  ;; Linux
```

## Next Steps

- **Quick example:** Load `examples/mlp-test.lisp` in SLIME and step through it
- **Learn by doing:** Open `src/mlp.lisp` and read the docstrings while interacting in the REPL
- **Detailed guide:** See [EXAMPLES.md](EXAMPLES.md)
- **Understand the philosophy:** Read [PHILOSOPHY.md](PHILOSOPHY.md)
