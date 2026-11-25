# History: 25 Years of Neuromuse

## 1999–2001: Birth at IRCAM

Fred Voisin initiated neuromuse in 1999 to explore how artificial neural networks could serve contemporary music creation. The very first program, written at IRCAM in March 1999, was a single-layer perceptron in Macintosh Common Lisp (MCL), inside OpenMusic (IRCAM's visual programming environment; package `:om`) — preserved in [`legacy/neuromuse-1999-03/`](../legacy/neuromuse-1999-03/). It grew into `ANN 1.4.lisp`, a Common Lisp Object System (CLOS) library with MIDI as the primary real-time interface, the core of the future neuromuse.

Key influences: David Wessel and Adrian Freed at UC Berkeley's CNMAT inspired the philosophical approach—networks as adaptive systems for live creative work, not just optimized classifiers.

**First performances (2001):**
- February 2001: "Taire" (Toeplitz & Myriam Gourfink), Centre National de la Danse, Paris — dance-derived input trained and controlled a generative sound system
- 7 June 2001: "L'Écarlate" (Kasper T. Toeplitz & Myriam Gourfink), IRCAM (Espace de projection, Agora festival) — the dancer's movement, analyzed live by [LabanOnLisp](https://github.com/FredVoisin/LabanOnLisp) on a separate computer, was streamed over MIDI (MidiShare) to neuromuse. A 180-cell auto-associative memory (`ecarl180`) measured how far each moment of movement sat from the learned choreographic "situations", alongside a Jordan-style recurrent MLP (`sin1C`) driving synthesis parameters. See [auto-assoc-exemples.md](auto-assoc-exemples.md) and the concert version of the code in [`legacy/neuromuse-2001-concert/`](../legacy/neuromuse-2001-concert/).

Both works demonstrated the viability of real-time neural systems in performance, running in MCL on Mac OS 9 on PowerPC laptops (a PowerBook G3 "Pismo" for L'Écarlate) at symbolic (MIDI) resolution.

## 2003: neurOMuse, an OpenMusic library

At CIRM (Nice), neuromuse was packaged as **neurOMuse v1**, a library for OpenMusic 4.7 / MCL 5 (release archive dated 26 September 2003), preserved in [`legacy/neuromuse-2003-openmusic/`](../legacy/neuromuse-2003-openmusic/). The same year saw a demo/workshop at National Taiwan University, Taipei.

## 2004–2005: Expansion to Max/Pd

Around 2004, Fred Voisin and collaborators Robin Meier, Arshia Cont, and Ali Momeni ported the MLP and SOM architectures from Lisp into Max/MSP and Pure Data for broader adoption in music production studios. This era saw workshops and residencies at CIRM (Centre International de Recherche Musicale, Nice), an invited talk at Ars Electronica (2005), and **"Caresses de Marquise"** (2004, 12-hour performance, Gare de l'Est, Paris), an early multi-agent experiment.

## 2006–2008: Real-Time Multi-Agent Systems

From 2006 to 2008, neuromuse evolved into a stable real-time multi-agent system using various Common Lisp implementations (Lispworks, OpenMCL, CMU-CL, SBCL) with TCP/IP and multi-threading on Linux and macOS. This enabled distributed computing across modest clusters.

**Major artistic works:**
- **"Symphonie des machines"** (2006, Meier & Voisin, Sophia-Antipolis) — 25 neuromuse agents running on an Infineon compute cluster (donated Sun servers and PCs), coordinated via Pure Data, rendering real-time music
- **"Last Manoeuvres in the Dark"** (2008, Giraud & Siboni, Palais de Tokyo, Paris) — music information retrieval feeding SOM-based stylistic analysis, with neural activation decoding to a sculpture array of Darth Vader helmets. The agents were ROSOM agents listening over UDP: C and Java implementations, by John MacCallum (CNMAT), were tested, and the Java one ran on distributed ARM boards (Calao motherboards) under Linux.

## 2007–2010: Teaching & Consolidation

Fred Voisin taught neuromuse and computer music at the Conservatoire de Montbéliard, establishing it as teaching material for master classes. The codebase matured but remained research-oriented.

## 2011–2015: Underground Persistence

After 2010, neuromuse continued quietly. Fred used it in research applications:
- Cognitive rehabilitation projects (mime gesture recognition + gesture-to-sound mapping)
- Collaborations at CNRS (Université de Bourgogne, LEAD lab)
- Robot control (avatit agent driving a robotic arm at IRCAM, 2015)

During this period, interest in neural networks revived globally (deep learning boom ~2012 onward), but neuromuse remained deliberately small-scale and interpretable—a philosophical counterpoint to the massive data-hungry models emerging elsewhere.

## 2012–2025: Contemporary Applications

Neuromuse found new life in:
- **Sound design & sonic interaction** — mapping movement or gesture to synthesis parameters
- **Research in embodied AI** — understanding how networks learn from human-centric, low-dimensional inputs
- **Ethnomusicological fieldwork** — analyzing and regenerating traditional music patterns
- **Mobile platforms** — neuromuse-ios (iPhone 5s, via Theos jailbreak) demonstrates the system's portability

## 2026: Restructuring and New Developments

This restructuring follows on from Florian Iochem's doctoral thesis work, whose contribution in
bringing neuromuse's historical sources online is acknowledged in [`legacy/README.md`](../legacy/README.md).

**July 2026** — the master repository was reorganized as a proper ASDF system with:
- Clean package structure (`:neuromuse`)
- Dedicated directories: `src/`, `tests/`, `examples/`, `doc/`
- Bug fixes in instance saving, network constructors, UDP input
- A full test suite using `:prove`
- Expanded documentation for artists and researchers

**September–October 2026** — historical sources and new features:
- Historical versions archived in [`legacy/`](../legacy/): the March 1999 perceptron, the 2001 L'Écarlate concert code, and the 2003 neurOMuse OpenMusic library
- Architectures ported from those sources: single-layer perceptron (1999), Hopfield network and auto-associative memory (2001)
- An optional Ltk window (`neuromuse/gui`) to watch MLPs, SOMs, and auto-associative memories learn live
- SOMs: configurable input-space and grid distances, torus and hexagonal topologies (see [som-distances.md](som-distances.md))
- Activation tracing, saving for SOMs, and Graphviz export of networks (see [formats.md](formats.md))
- Open Sound Control input/output and remote control (see [opensoundcontrol.md](opensoundcontrol.md)), Standard MIDI File reading, and a transformer class

---

## Key Collaborators

- **Robin Meier** — [composer](https://robinmeier.net), Max/MSP specialist, co-author of foundational papers and pieces of code
- **Myriam Gourfink** — [choreographer](https://www.myriam-gourfink.com), first major artistic collaborator
- **Kasper T. Toeplitz** — [composer](https://www.sleazeart.com), co-author of "Taire" and "L'Écarlate"
- **Arshia Cont** — [real-time music systems researcher](https://forum.ircam.fr/article/metronaut-arshia-cont/) (Ircam, Antescofo), founder of the [Metronaut](https://metronautapp.com) iOS app
- **Ali Momeni** — [music technology developer](https://alimomeni.net)
- **John MacCallum** — [composer at CNMAT](https://cnmat.berkeley.edu/node/7608), C and Java implementations of the ROSOM agent on the distributed ARM (Calao) boards of "Last Manoeuvres in the Dark" (2008)
- **Jean-Luc Hervé** — [composer](https://jeanlucherve.com), "Effet Lisière" collaboration
- **Cyrille Henry** — [chdh](https://chdh.net); inspiration and some Pd patches
- **Florian Iochem** — [musician and researcher](https://accra-recherche.unistra.fr/laccra/membres/doctorants/florian-iochem/), ACCRA, Université de Strasbourg; bringing neuromuse's historical sources online (2026, see [`legacy/README.md`](../legacy/README.md))

---

## Publications

- Voisin, F., Meier, R. (2004). "Playing Integrated Music Knowledges with Artificial Neural Networks." *JIM/SMC*, Ircam/Centre Pompidou.
- Voisin, F., Meier, R. (2009). "On Analytical vs. Schizophrenic Procedures for Computing Music." *Contemporary Music Review*, 28(2), 205–219.
- Voisin, F. (2015). "De la brousse dans les synthés." In *Gilles Deleuze: la pensée-musique* (Criton & Chouvel, eds.), HAL-01611947.

---

## Timeline

| Year | Milestone |
|---|---|
| 1999 | First perceptron written in MCL/OpenMusic at IRCAM |
| 2001 | "Taire" & "L'Écarlate" premieres |
| 2003 | neurOMuse v1, OpenMusic library (CIRM) |
| 2004–2005 | Max/Pd ports; CIRM residencies; Ars Electronica talk |
| 2006–2008 | Multi-agent systems; "Symphonie des machines"; ARM cluster work |
| 2007+ | Teaching at Conservatoire de Montbéliard |
| 2012+ | Deep learning emerges elsewhere; neuromuse goes quiet but persistent |
| 2015 | Avatit robot control at IRCAM (night sessions, -2nd floor) |
| 2026 | Codebase restructured; historical sources archived; Hopfield, auto-assoc, GUI, OSC, transformer added |

---

## Why It Still Matters

Even as the AI landscape has shifted dramatically toward GPT, transformers, and massive language models, neuromuse remains relevant because:

1. **Scale & interpretability:** It operates in a regime (dozens to hundreds of neurons) where humans can understand what's happening.
2. **Real-time responsiveness:** No cloud latency, no waiting for inference. Networks train and run live.
3. **Artistic agency:** The artist remains in control, not subordinate to a model's predictions.
4. **Philosophical coherence:** Neuromuse never pretended neural networks were magic. It showed them as what they are: learnable systems for mapping patterns.

For artists, researchers, and students asking "How do neural networks actually work?", neuromuse is still a clearer, more intimate answer than any modern framework offers.
