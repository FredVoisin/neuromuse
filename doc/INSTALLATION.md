# Installation & Setup

## Requirements

- **SBCL** 2.0+ ([http://www.sbcl.org](http://www.sbcl.org)) — tested on Linux, macOS, Windows
- **Quicklisp** ([https://www.quicklisp.org](https://www.quicklisp.org)) — Common Lisp package manager
- **Emacs** with **SLIME** ([https://slime.common-lisp.dev/](https://slime.common-lisp.dev/)) — recommended for interactive development
- **Git** — to clone the repository

## Step-by-Step Installation

### 1. Install SBCL

**macOS (via Homebrew):**
```bash
brew install sbcl
```

**Linux (Debian/Ubuntu):**
```bash
sudo apt-get install sbcl
```

**Windows & others:** See [http://www.sbcl.org/platform-table.html](http://www.sbcl.org/platform-table.html)

### 2. Install Quicklisp

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

**Option A: Symlink (recommended)**
```bash
ln -s ~/projects/neuromuse ~/quicklisp/local-projects/neuromuse
```

**Option B: Manual registry** (from SBCL REPL):
```lisp
(push #P"~/projects/neuromuse/" asdf:*central-registry*)
```

### 5. Load the system

From the REPL:
```lisp
(ql:quickload :neuromuse)
(in-package :neuromuse)
```

Or test the suite. Plain `asdf:test-system` does *not* fetch missing dependencies on its own — the
first time, prime `:prove` via Quicklisp before running it, or you'll get `Component :PROVE not found`:
```lisp
(ql:quickload :prove)          ;; first time only
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
