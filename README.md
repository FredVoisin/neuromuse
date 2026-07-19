# neuromuse

Artificial neural networks to generate music and artistic productions with real-time applications (since 2000)

This project was initiated by Fred Voisin ([www.fredvoisin.com](http://www.fredvoisin.com)) in 1999 to study the application of artificial neural nets
to contemporary music creation using, at first, Lisp language (Macintosh Common Lisp and Common Lisp Object System), OpenMusic software (Ircam, [www.ircam.fr](http://www.ircam.fr)) and MIDI protocol. Some overall principles were inspired by David Wessel (<http://music.berkeley.edu/who-was-david-wessel/>) and Adrian Freed at CNMAT (<http://cnmat.berkeley.edu>)
At this time, the very first ('alpha') version of this project was available at [www.neuromuse.net](http://www.neuromuse.net) and at the Openmusic Ircam's Forum (an archived snapshot of the original www.neuromuse.org site, from September 2005, can be found here: <https://web.archive.org/web/20050910170552/http://www.neuromuse.org/>).
Its first public music applications were produced in February 2001 at the Centre National de la Danse in Paris (cf. "Taire", by Toeplitz and Gourfink) and in June 2001 at Ircam (cf. "L'ecarlate", by Toeplitz and Gourfink), with the use of a Macintosh Common Lisp client to control a MIDI server (cf. <http://midishare.sourceforge.net>).
Later, around 2004, different Lisp versions of this project developped by the author was used to design equivalent neural net architectures in "Max" and Puredada real-time programming environments for music production (Ircam & Cycling74) in collaboration with Robin Meier (<http://robinmeier.net>), Arshia Cont and Ali Momeni, with the support of the Centre International de Recherche Musicale at Nice ([www.cirm-manca.org](http://www.cirm-manca.org)).
From 2006 to 2008, new Lisp developments by the author provided a stable real-time multi-agent system, using various Lisp such as Lispworks, OpenMCL, CMU-CL and SBCL ([www.sbcl.org](http://www.sbcl.org)) Common Lisp implementations, with the use of TCP/IP and multi-threading on CMU-CL and SBCL implementations for Linux and Mac OS X. Computer music productions such as "Symphonie des machines" (Meier & Voisin, Sophia-Antipolis 2006) and "Last Manoeuvres in the Dark" (Giraud & Siboni, Palais de Tokyo, Paris 2008) were artistic applications of this version, with some particular adaptations in Python, Java and C++ for distributed multicore ARM ad-hoc architectures (thank's to John McCallum).

Even if this project is becoming quite old, it may be a good start for new developments since it's short, simple, easy to use and efficient enough for education and artistic productions.

Update, July 2026: the project has been reorganized as a proper ASDF system (src/, tests/, examples/,
doc/), with the code moved into its own :neuromuse package, a handful of long-standing bugs fixed
(instance saving, network constructors, UDP SOM input), and a real test suite added. Still short, still
simple, hopefully still useful.

New updates in progress with the help of Claude AI, in the former spirit of the neuromuse project.

Fred Voisin.

---

## Présentation (Nice, mai 2005)

### neuromuse

*Etudes et applications musicales des réseaux neuromimétiques*

Compositeurs, musiciens, chorégraphes et danseurs recourent de plus en plus à l'informatique dans leurs projets, depuis le travail de création, pour l'exploration des possibles jusque la réalisation, selon différentes modalités de communication homme-machine. L'informatique — la machine universelle — est un outil privilégié de formalisation, d'expérimentation, de simulation et de réalisation.

Cependant, lors de mes différentes expériences comme ethnomusicologue et assistant musical, j'ai pu constater que lorsque la communication avec les machines s'effectue au moyen d'un code écrit dans un langage purement logique, la formalisation nécessaire à l'écriture de ce code peut s'avérer contradictoire avec la nature des connaissances invoquées, lesquelles peuvent être intuitives, inconscientes, implicites, contradictoires, irrationnelles ou magiques. L'informatique traditionnelle est encore peu adaptée à ces situations pourtant courantes, et naturelles, où opèrent des connaissances transitoires et des croyances. L'établissement d'une communication pertinente requiert une adaptation et une plasticité instantanées du programme, si ce n'est un mimétisme que suscite inévitablement le test de Turing. Dans ce contexte, les systèmes de réseaux de neurones artificiels, en s'inspirant de processus neurobiologiques, apparaissent comme une alternative particulièrement intéressante dès lors qu'ils situent l'auto-adaptation au cœur même du dispositif informatique.

C'est dans le cadre d'une recherche chorégraphique, en 2000, que j'ai été amené à écrire en langage Lisp mon premier réseau de neurones artificiels. Je constatai alors que si les systèmes neuromimétiques étaient bien capables d'apporter des réponses pertinentes dans les domaines artistiques, la compréhension et la maîtrise de leur fonctionnement constituait un vaste projet.

*Frédéric Voisin, Nice, mai 2005*

<p align="center">
  <img src="img/wwwneuromuse2005.png" alt="neuromuse.org home page, January 2005" width="360"><br>
  <sub><i>www.neuromuse.org home page as of January 22, 2005 (Wayback Machine)</i></sub>
</p>
