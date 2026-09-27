# History: 25 Years of Neuromuse

## 1999–2001: Birth at IRCAM

Fred Voisin initiated neuromuse in 1999 to explore how artificial neural networks could serve contemporary music creation. The first implementation used Macintosh Common Lisp (MCL) and the Common Lisp Object System (CLOS), with OpenMusic (IRCAM's visual programming environment) and MIDI as the primary interface.

Key influences: David Wessel and Adrian Freed at UC Berkeley's CNMAT inspired the philosophical approach—networks as adaptive systems for live creative work, not just optimized classifiers.

**First performances (2001):**
- February 2001: "Taire" (Toeplitz & Myriam Gourfink), Centre National de la Danse, Paris — dance-derived input trained and controlled a generative sound system
- June 2001: "L'Écarlate" (Toeplitz & Gourfink), IRCAM — recurrent MLP mapped live movement to synthesis parameters (frequency, amplitude, duration)

Both works demonstrated the viability of real-time neural systems in performance, running on PowerPC laptops at symbolic (MIDI) resolution.

## 2004–2005: Expansion to Max/Pd

Around 2004, Fred Voisin and collaborators Robin Meier, Arshia Cont, and Ali Momeni ported the MLP and SOM architectures from Lisp into Max/MSP and Pure Data for broader adoption in music production studios. This era saw workshops and residencies at CIRM (Centre International de Recherche Musicale, Nice) and invited talks at Ars Electronica (2005).

## 2006–2008: Real-Time Multi-Agent Systems

From 2006 to 2008, neuromuse evolved into a stable real-time multi-agent system using various Common Lisp implementations (Lispworks, OpenMCL, CMU-CL, SBCL) with TCP/IP and multi-threading on Linux and macOS. This enabled distributed computing across modest clusters.

**Major artistic works:**
- **"Caresses de Marquise"** (2004, 12-hour performance, Gare de l'Est, Paris) — early multi-agent experiment
- **"Symphonie des machines"** (2006, Meier & Voisin, Sophia-Antipolis) — 25 neuromuse agents running on an Infineon compute cluster (donated Sun servers and PCs), coordinated via Pure Data, rendering real-time music
- **"Last Manoeuvres in the Dark"** (2008, Giraud & Siboni, Palais de Tokyo, Paris) — music information retrieval feeding SOM-based stylistic analysis, with neural activation decoding to a sculpture array of Darth Vader helmets. Agents ran on distributed ARM boards (Calao motherboards) under Linux.

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

## July 2026: Restructuring

The master repository was reorganized as a proper ASDF system with:
- Clean package structure (`:neuromuse`)
- Dedicated directories: `src/`, `tests/`, `examples/`, `doc/`
- Bug fixes in instance saving, network constructors, UDP input
- A full test suite using `:prove`
- Expanded documentation for artists and researchers

---

## Key Collaborators

- **Robin Meier** — composer, Max/MSP specialist, co-author of foundational papers
- **Myriam Gourfink** — choreographer, first major artistic collaborators
- **Arshia Cont** — real-time music systems researcher
- **Ali Momeni** — music technology developer
- **John McCallum** — composer at CNMAT, distributed ARM architecture expertise
- **Jean-Luc Hervé** — composer, "Effet Lisière" collaboration

---

## Publications

- Voisin, F., Meier, R. (2004). "Playing Integrated Music Knowledges with Artificial Neural Networks." *JIM/SMC*, Ircam/Centre Pompidou.
- Voisin, F., Meier, R. (2009). "On Analytical vs. Schizophrenic Procedures for Computing Music." *Contemporary Music Review*, 28(2), 205–219.
- Voisin, F. (2015). "De la brousse dans les synthés." In *Gilles Deleuze: la pensée-musique* (Criton & Chouvel, eds.), HAL-01611947.

---

## Timeline

| Year | Milestone |
|---|---|
| 1999 | First MLP written in MCL at IRCAM |
| 2001 | "Taire" & "L'Écarlate" premieres |
| 2004–2005 | Max/Pd ports; CIRM residencies; Ars Electronica talk |
| 2006–2008 | Multi-agent systems; "Symphonie des machines"; ARM cluster work |
| 2007+ | Teaching at Conservatoire de Montbéliard |
| 2012+ | Deep learning emerges elsewhere; neuromuse goes quiet but persistent |
| 2015 | Avatit robot control at IRCAM |
| 2026 | Codebase restructured; documentation expanded |

---

## Why It Still Matters

Even as the AI landscape has shifted dramatically toward GPT, transformers, and massive language models, neuromuse remains relevant because:

1. **Scale & interpretability:** It operates in a regime (dozens to hundreds of neurons) where humans can understand what's happening.
2. **Real-time responsiveness:** No cloud latency, no waiting for inference. Networks train and run live.
3. **Artistic agency:** The artist remains in control, not subordinate to a model's predictions.
4. **Philosophical coherence:** Neuromuse never pretended neural networks were magic. It showed them as what they are: learnable systems for mapping patterns.

For artists, researchers, and students asking "How do neural networks actually work?", neuromuse is still a clearer, more intimate answer than any modern framework offers.
