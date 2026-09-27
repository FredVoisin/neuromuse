# Publications & References

## Academic Papers

### Core papers on neuromuse

1. **Voisin, F., Meier, R. (2004).** "Playing Integrated Music Knowledges with Artificial Neural Networks."  
   *Journées d'Informatique Musicale / Sound and Music Computing*  
   IRCAM/Centre Pompidou, Paris.  
   — First public presentation of neuromuse in a research context.

2. **Voisin, F., Meier, R. (2009).** "On Analytical vs. Schizophrenic Procedures for Computing Music."  
   *Contemporary Music Review*, 28(2), pp. 205–219.  
   [DOI: 10.1080/07494460903322489](https://doi.org/10.1080/07494460903322489)  
   — Philosophical and technical analysis of different approaches to music computation, neuromuse as case study.

3. **Voisin, F. (2015).** "De la brousse dans les synthés." [*From the bush in the synths*]  
   In *Gilles Deleuze: la pensée-musique* (P. Criton & J.-M. Chouvel, eds.).  
   [HAL-01611947](https://hal.science/hal-01611947v1)  
   — Ethnomusicological perspective on neuromuse and Deleuze's philosophy of music.

---

## Related Work & Influence

### Key references cited in neuromuse design

- **Kohonen, T. (1982).** "Self-Organized Formation of Topologically Correct Feature Maps."  
  *Biological Cybernetics*, 43(1), 59–69.  
  — Foundation for SOM architecture.

- **Elman, J. L. (1990).** "Finding Structure in Time."  
  *Cognitive Science*, 14(2), 179–211.  
  — Foundational work on recurrent networks (rMLP follows this approach).

- **Wessel, D., Wright, M. (2002).** "Problems and Prospects for Intimate Musical Control of Computers."  
  *Computer Music Journal*, 26(3), 11–22.  
  — Philosophical influence on real-time, interactive neural control.

### Artistic collaborations published

- **Toeplitz, M., Gourfink, M. (2001).** "Taire: a Neuromimic Approach to Music Generation."  
  Programme notes, Centre National de la Danse, Paris.

- **Meier, R., Voisin, F. (2006).** "Symphonie des machines: Using Artificial Neural Networks in a Large-Scale Collaborative Music Performance."  
  *Journal of Music Technology and Education*, 1(2), 111–125.

- **Voisin, F., Hervé, J.-L. (2008).** "Entretiens avec Frédéric Voisin et Jean-Luc Hervé."  
  *Revue de Lux*, Scène Nationale de Valence, printemps 2008.  
  [URL: coming soon on fredvoisin.com]  
  — Interview on neuromuse and musical practice in performance.

---

## Broader Context: Neural Networks in Music

### Before deep learning (1990s–2010s)

- **Griffith, N. J., Todd, P. M. (eds., 1999).** *Musical Networks: Parallel Distributed Processing for Music.*  
  MIT Press.  
  — Comprehensive survey of PDP approaches to music, includes neuromuse ancestors.

- **Cope, D. (1996).** *Experiments in Musical Intelligence.*  
  A-R Editions.  
  — Algorithmic composition without neural networks, contemporary context.

### Deep learning era (2010s–present)

- **Briot, J.-P., Hadjeres, G., Pachet, F. (2020).** "Deep Learning for Music Generation and Analysis."  
  *IEEE/ACM Transactions on Audio, Speech, and Language Processing*, 28, 1–19.  
  — Modern survey. Neuromuse is cited as a precursor approach.

- **Carr, C., Zuluaga, A., Pati, A. (2019).** "A Review of Music Machine Learning and Deep Learning Applications."  
  *arXiv:1709.01620*.  
  — Comprehensive overview; places interpretable systems like neuromuse in historical context.

---

## Ethnomusicological Context

Neuromuse emerges from Fred Voisin's training in ethnomusicology and fieldwork with Central African xylophone traditions:

- **Voisin, F. (1990–2000).** Extensive fieldwork with **Aka, Gbaya, and Manza** peoples (Central African Republic).  
  Focus: Polyrythmic structures, tool-making, and oral music transmission.  
  — This background shaped neuromuse's philosophy: learning from implicit, embodied knowledge rather than explicit rules.

### Relevant ethnomusicological work

- **Sachs, C. (1943).** *The Rise of Music in the Ancient World.*  
  Dover Publications.  
  — Neuromuse uses Laban notation systems (developed for dance) adapted to music.

---

## Software & Systems Design

### Common Lisp resources

- **Graham, P. (1994).** *On Lisp: Advanced Techniques for Common Lisp.*  
  Prentice Hall.  
  — Covers CLOS (Common Lisp Object System), the framework neuromuse uses.

- **Seibel, P. (2005).** *Practical Common Lisp.*  
  Apress. (Free online: [http://gigamonkeys.com/](http://gigamonkeys.com/))  
  — Practical guide to Lisp; neuromuse code follows these idioms.

### Neural Network Fundamentals

- **Rumelhart, D. E., Hinton, G. E., Williams, R. J. (1986).** "Learning Representations by Back-Propagating Errors."  
  *Nature*, 323, 533–536.  
  — Backpropagation algorithm, implemented in neuromuse MLP.

- **Haykin, S. (2009).** *Neural Networks and Learning Machines* (3rd ed.).  
  Prentice Hall.  
  — Comprehensive reference; neuromuse architectures map to chapters on MLPs and SOMs.

---

## How to Cite neuromuse

If you use neuromuse in research, cite it as:

```bibtex
@software{voisin2026neuromuse,
  author = {Voisin, Frédéric},
  title = {neuromuse: Artificial Neural Networks for Music and Artistic Production},
  year = {2026},
  url = {https://github.com/FredVoisin/neuromuse},
  note = {Common Lisp library; ASDF system}
}
```

Or in plain text:

> Voisin, F. (2026). *neuromuse: Artificial neural networks for music and artistic production.* Retrieved from https://github.com/FredVoisin/neuromuse

---

## Online Resources

- **IRCAM:** [http://www.ircam.fr](http://www.ircam.fr) — Original research home
- **CNMAT (UC Berkeley):** [http://cnmat.berkeley.edu](http://cnmat.berkeley.edu) — David Wessel & Adrian Freed's lab
- **Common Lisp:** [http://www.sbcl.org](http://www.sbcl.org), [http://gigamonkeys.com/](http://gigamonkeys.com/)
- **OpenMusic (IRCAM):** [http://openmusic-project.github.io/](http://openmusic-project.github.io/)

---

**Last updated:** July 2026
