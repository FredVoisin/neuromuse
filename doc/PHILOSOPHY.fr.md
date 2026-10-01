# Philosophie : pourquoi neuromuse existe

*[English version](PHILOSOPHY.md)*

## Présentation (Nice, mai 2005)

### neuromuse — Études et applications musicales des réseaux neuromimétiques

Compositeurs, musiciens, chorégraphes et danseurs recourent de plus en plus à l'informatique dans leurs projets, depuis le travail de création, pour l'exploration des possibles jusque la réalisation, selon différentes modalités de communication homme-machine. L'informatique — la machine universelle — est un outil privilégié de formalisation, d'expérimentation, de simulation et de réalisation.

Cependant, lors de mes différentes expériences comme ethnomusicologue et assistant musical, j'ai pu constater que lorsque la communication avec les machines s'effectue au moyen d'un code écrit dans un langage purement logique, la formalisation nécessaire à l'écriture de ce code peut s'avérer contradictoire avec la nature des connaissances invoquées, lesquelles peuvent être intuitives, inconscientes, implicites, contradictoires, irrationnelles ou magiques. L'informatique traditionnelle est encore peu adaptée à ces situations pourtant courantes, et naturelles, où opèrent des connaissances transitoires et des croyances. L'établissement d'une communication pertinente requiert une adaptation et une plasticité instantanées du programme, si ce n'est un mimétisme que suscite inévitablement le test de Turing. 

Dans ce contexte, les systèmes de réseaux de neurones artificiels, en s'inspirant de processus neurobiologiques, apparaissent comme une alternative particulièrement intéressante dès lors qu'ils situent l'auto-adaptation au cœur même du dispositif informatique.

C'est dans le cadre d'une recherche chorégraphique, en 2000, que j'ai été amené à écrire en langage Lisp mon premier réseau de neurones artificiels. Je constatai alors que si les systèmes neuromimétiques étaient bien capables d'apporter des réponses pertinentes dans les domaines artistiques, la compréhension et la maîtrise de leur fonctionnement constituait un vaste projet.

**Frédéric Voisin, Nice, mai 2005**

<p align="center">
  <img src="../img/wwwneuromuse2005.png" alt="Page d'accueil du site neuromuse.net en 2005" width="500"><br>
  <sub>Le site neuromuse.net tel qu'il se présentait en 2005, hébergé au CIRM (Nice).</sub>
</p>

---

## Principes fondamentaux

*Section ajoutée depuis 2005 ; traduite de l'anglais, à relire.*

### 1. **L'art est le processus, pas seulement le produit**

Les cadres classiques de l'IA traitent les réseaux comme des outils : on injecte des données, on en extrait des prédictions, on passe à autre chose. Neuromuse inverse cette logique. L'acte d'apprendre — les neurones qui s'activent, les poids qui s'ajustent, la courbe d'erreur qui descend — *est* l'œuvre. Cela compte pour plusieurs raisons :

- **Transparence :** l'artiste peut regarder le réseau apprendre en temps réel, voir ce qu'il « pense », et réagir.
- **Dialogue :** l'artiste ne se contente pas de *fabriquer* le système ; le système *fabrique* avec l'artiste.
- **Incarnation :** neuromuse tournant sur du matériel modeste (même un iPhone 5s), le réseau peut habiter un espace physique — un bras robotisé, un ensemble de haut-parleurs — et y *exister* réellement.

### 2. **La connaissance peut être implicite, intuitive, contradictoire**

Les systèmes fondés sur la logique exigent une formalisation parfaite : chaque règle, chaque entrée, parfaitement étiquetée. La connaissance artistique et scientifique réelle n'est pas de cet ordre. Le vocabulaire gestuel d'un chorégraphe est incarné, jamais totalement articulé. L'intuition harmonique d'un musicien est en partie consciente, en partie ressentie.

Neuromuse accepte ce désordre. Les réseaux apprennent à partir d'*exemples* (connaissance implicite) plutôt que de règles. Ils s'adaptent à l'ambiguïté, à la contradiction, à l'incomplétude — les conditions mêmes dans lesquelles travaillent réellement artistes et chercheurs.

### 3. **L'auto-adaptation en est le cœur**

Plutôt que de se demander « Comment programmer le comportement que je veux ? », neuromuse se demande « Comment mettre en place les conditions pour qu'un comportement adaptatif émerge ? ». Ce basculement est profond :

- **L'apprentissage se fait en direct,** en dialogue avec l'utilisateur.
- **Le système reste interprétable** — on peut toujours demander pourquoi il a fait tel choix.
- **L'échelle se mesure en neurones, pas en paramètres.** Les réseaux de neuromuse tournent sur des dizaines ou des centaines de neurones, pas des milliards.

### 4. **Le code est aussi un art**

Le code de neuromuse est écrit pour être *lu* — par des artistes, des chercheurs, des étudiants. Une méthode dans `src/mlp.lisp` est un mini-essai sur le fonctionnement de la rétropropagation. Le langage Lisp lui-même, par sa structure homoiconique (le code comme donnée), reflète la nature auto-réflexive des systèmes apprenants.

---

## Contexte historique

Neuromuse est né entre 1999 et 2001, à une époque où :

- les GPU n'existaient pas encore (CUDA a été lancé en 2006) ;
- les réseaux de neurones étaient scientifiquement intéressants mais semblaient peu praticables pour un travail créatif en temps réel ;
- Lisp restait un environnement sérieux pour la recherche en IA (Ircam, CNMAT, Symbolic Technologies) ;
- la question « un réseau peut-il apprendre une chorégraphie ? » était véritablement inédite.

Aujourd'hui, le paysage a changé. L'apprentissage profond domine. Mais neuromuse sert encore un but que les cadres modernes ne remplissent pas : c'est un espace où artistes et chercheurs peuvent **penser avec des systèmes neuronaux**, pas seulement les utiliser.

---

## Pour les artistes

Si vous êtes chorégraphe, musicien, plasticien ou designer sonore :

- neuromuse permet de construire des systèmes qui *apprennent de votre pratique* — vos gestes, vos improvisations, vos partitions.
- Vous pouvez entraîner un réseau à interpoler entre deux de vos vocabulaires gestuels, ou à générer des variations sur un thème.
- Le système reste à vous : à comprendre, à modifier. Pas de boîte noire. Pas de cloud propriétaire.

## Pour les chercheurs

Si vous étudiez :

- la **recherche d'information musicale**, la **reconnaissance gestuelle**, l'**IA créative** ou la **cognition incarnée** : neuromuse est un laboratoire pour des expériences à petite échelle, interprétables.
- Le code est assez compact pour être compris de bout en bout, mais assez complet pour un travail réel.
- Vous pouvez l'étendre, le combiner avec un travail de terrain ethnomusicologique, ou l'utiliser comme outil pédagogique.

---

## Référence

- Voisin, F., Meier, R. (2004). « Playing Integrated Music Knowledges with Artificial Neural Networks. » *Journées d'Informatique Musicale*, Ircam/Centre Pompidou, Paris.
- Voisin, F., Meier, R. (2009). « On Analytical vs. Schizophrenic Procedures for Computing Music. » *Contemporary Music Review*, 28(2), pp. 205–219. [DOI](https://doi.org/10.1080/07494460903322489)
