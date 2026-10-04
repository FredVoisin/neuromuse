---
title: "neuromuse — A proposs distances du SOM"
subtitle: "Récapitulatif des changements d'octobre 2026"
date: "4 octobre 2026"
lang: fr
---

# Corrections des neurones sur la carte selon principe général - flexbilité - prévu initialement (Nice ~2004)

# Ce qui a changé

Deux commits modifient la carte auto-organisatrice (`src/som.lisp`) :

- **la distance euclidienne d(x, w) devient la distance par défaut**. Jusqu'ici, `find-winner` comparait l'entrée x à l'*activation* du neurone, x⊙w (produit composante par composante), et non à ses poids w ;
- **la largeur du voisinage pilotée par l'erreur est normalisée**  - voir infra: [A propos du voisinage normalisé](#a-propos-du-voisinage-normalisé) ;
- **la distance sur la grille devient un réglage à part**, distinct de la distance entre l'entrée et les poids.

| Réglage | Rôle | Valeur par défaut | Autres valeurs |
|---|---|---|---|
| `distance` | écart entre l'entrée et les poids d'un neurone | `'euclidian` | `'cosine`, `'neuromuse-distance`, ou toute fonction (x w) |
| `grid-distance` | métrique de l'écart sur la grille entre un voisin et le gagnant | `'euclidian` | `'chebyshev`, `'manhattan` |
| `topology` | taille, dimension, puis topologie de la grille | `(144 2)` | `:lattice :hex`, `:boundary :torus` |
| `error-scaling` | largeur de la gaussienne de voisinage | `:normalized` | `:raw` |
| `max-error` | plus grande erreur de gagnant vue depuis `init` | `0.0` | mis à jour par `learn` |

Les anciennes versions se rejouent avec ce réglage, à une différence près, aux bords de la carte (voir plus bas le défaut de `voisins`).
Avant cette correction, la reproduction était exacte au bit près (avec carte 12×12  - 128 entrées : poids identiques après 15 époques, même graine) :

```lisp
(setf (distance som) 'neuromuse-distance
; neuromuse-distance est donc l'ancienne distance, (non-)euclienne
      (error-scaling som) :raw)
```

**Autres effets :**

- `(output neurone)` contient toujours l'activation entrée×poids, comme pour le MLP et le perceptron, quelle que soit la distance choisie. L'interface graphique et `trace-activation` n'ont rien à changer. Les poids du neurone, sans bruit, se lisent avec `(weights neurone)`.
- Une erreur nulle ne fait plus échouer `learn` : avant, la correction restait à `nil`.
- `save` enregistre les nouveaux réglages. Une carte sauvegardée **avant** ces changements contient `(distance it) 'euclidian`, qui désignait alors la distance à l'activation : il faut lui appliquer le réglage historique - donc neuromuse-distance - après le chargement pour garder son comportement.

# Deux espaces, deux distances

Dans `learn`, le premier patch calculait la distance sur la grille avec `euclidian`, écrit en dur.
Avec `(funcall (distance ann) voisin coord-w)`, prévu à Nice, pour que l'apprentissage se fasse dans la métrique de la classe (ANN, SOM...)
Et le SOM travaille dans **deux géométries** :

- **l'espace des entrées** contient l'entrée x et les poids w, des vecteurs de dimension d (16, 128…). La distance y mesure *à quel point un neurone ressemble à l'entrée* ; c'est elle qui désigne le gagnant ;
- **l'espace de la grille** contient les positions des neurones sur la carte, des coordonnées comme (3, 7). La distance y mesure *à quel point deux neurones sont voisins* ; c'est elle qui décide combien un voisin du gagnant est entraîné.

Dans `learn`, `voisin` et `coord-w` sont des positions sur la grille. Leur appliquer la fonction du slot `distance`, c'est mesurer la carte avec un outil fait pour les entrées.
Avec `'euclidian`, cela fonctionnait par coïncidence : la même formule convient aux deux espaces.

## Ce que donnait `neuromuse-distance` sur la grille

Avec v la position du voisin et c celle du gagnant, on calculerait √(Σ vᵢ² (1 − cᵢ)²). Le résultat dépend alors de *l'endroit* où se trouve le gagnant sur la carte, et non de l'écart entre les deux neurones :

| Gagnant | Voisin | Écart réel | Distance obtenue | Effet |
|---|---|---|---|---|
| (1, 1) | (3, 1) | 2 cases | **0** | le voisin est corrigé comme le gagnant |
| (5, 5) | (4, 5) | 1 case | **≈ 25,6** | le voisin ne reçoit presque rien |

Cette distance n'est pas non plus symétrique : d(voisin, gagnant) ≠ d(gagnant, voisin). Mesurée sur la carte d'essai, cette confusion faisait tomber le nombre de neurones utilisés de 29 à 9, et les poids ne correspondaient plus à ceux de l'ancien code.

## La métrique de la classe intervient déjà dans l'apprentissage

Le slot `distance` agit bien sur l'apprentissage, à deux endroits : il choisit le gagnant, et il donne l'erreur de chaque voisin, qui fixe la largeur de la gaussienne.

La règle de mise à jour `w += c·(x − w)` reste, elle, euclidienne : c'est le gradient de la distance euclidienne. Pour `neuromuse-distance`, le gradient cohérent serait xᵢ²(1 − wᵢ), qui pousserait tous les poids vers 1. Avec toute distance autre qu'euclidienne, la compétition et la mise à jour ne visent donc plus exactement le même objectif.

## Principe posé à Nice

Dans l'ancienne version de `som.lisp`, le slot `topology` valait `'(espace nombredimension autresdescripteurs)`, initialisé avec `'euclidian` en premier élément, et `learn` lisait cet « espace » dans une variable restée inutilisée. La géométrie de la grille avait donc déjà sa place prévue, à côté de la distance des entrées et non mêlée à elle.
Le slot `topology` reprend maintenant cette intention, avec ses propriétés `:lattice` et `:boundary`.

# Les variantes de distance

## Dans l'espace des entrées (slot `distance`)

| Distance | Formule | Propriétés | Usage |
|---|---|---|---|
| `'euclidian` | ‖x − w‖ | cohérente avec la règle d'apprentissage ; tient compte de l'amplitude | **par défaut** |
| `'neuromuse-distance` | ‖x − x⊙w‖ = √(Σ xᵢ²(1 − wᵢ)²) | ignore l'amplitude globale et le signe de x ; une composante nulle de x ne compte pas ; le neurone idéal est w = (1 … 1) pour toute entrée | rejouer les versions 1999–2001 |
| `'cosine` | 1 − ⟨x, w⟩ / (‖x‖‖w‖) | ignore l'amplitude, de façon cohérente | coder la *forme* d'un spectre plutôt que son intensité |
| euclidienne pondérée *(à écrire)* | √(Σ xᵢ²(xᵢ − wᵢ)²) | ne compte que ce qui est présent dans x ; optimum en w = x | essai peu concluant (voir plus bas) |

Pour ignorer l'amplitude, on peut aussi normaliser les entrées puis garder la distance euclidienne : c'est la solution la plus simple.

Avec `:raw`, l'échelle de la fonction compte : une distance au carré, comme `euclidian-fast`, élargirait ou rétrécirait le voisinage. Avec `:normalized`, seul le rapport erreur / `max-error` intervient, mais une distance au carré rend ce rapport lui-même carré.

**Essai comparatif** (simulation à part, carte 12×12, 16 entrées, formes qui varient continûment en position, largeur et amplitude ; voisinage piloté par l'erreur brute) :

| Distance | Neurones utilisés | Part du neurone le plus gagnant | Erreur topographique |
|---|---|---|---|
| neuromuse | 26 / 144 | 12,3 % | 22,5 % |
| euclidienne | 138 / 144 | 2,2 % | 6,0 % |
| cosinus | 41 / 144 | 4,3 % | 27,4 % |
| euclidienne pondérée | 144 / 144 | 3,0 % | 43,3 % |

Le cosinus et neuromuse utilisent moins de neurones, en partie parce qu'ils ignorent l'amplitude, l'un des trois paramètres des données. Mais neuromuse concentre bien plus les victoires que le cosinus : les neurones aux poids élevés deviennent des « carrefours ». La version pondérée organise mal la carte, probablement parce que son erreur plus petite rétrécit trop le voisinage piloté par l'erreur.

## Sur la grille : une métrique et une topologie

Une distance de grille doit être **symétrique**, **nulle seulement au gagnant** et **indépendante de la position absolue** sur la carte. Elle combine deux choix indépendants : une **métrique**, nommée dans le slot `grid-distance`, et une **topologie**, décrite dans le slot `topology`.

| Métrique (`grid-distance`) | Formule | Forme du voisinage |
|---|---|---|
| `'euclidian` (défaut) | √(Σᵢ Δᵢ²) | ronde, dans la fenêtre carrée de `voisins` : les coins (à √8 ≈ 2,8 cases pour un rayon de 2) sont moins corrigés que les bords |
| `'chebyshev` | maxᵢ \|Δᵢ\| | carrée, exactement la fenêtre de `voisins` : coins et bords à égalité |
| `'manhattan` | Σᵢ \|Δᵢ\| | en losange |

| Topologie (`topology`) | Effet |
|---|---|
| `:lattice :square` (défaut) | grille carrée, coordonnées (colonne, ligne) telles quelles |
| `:lattice :hex` | lignes impaires décalées d'une demi-case : le neurone (c, l) est placé en x = c + (l mod 2)/2, y = l·√3/2, et a six voisins directs. `'euclidian` y mesure la distance dans le plan ; `'chebyshev` et `'manhattan` comptent les pas hexagonaux, ce qui donne un voisinage en hexagone |
| `:boundary :bounded` (défaut) | grille bornée |
| `:boundary :torus` | bords opposés recollés : la distance est la plus courte vers les copies du voisin décalées d'un côté de grille dans chaque direction, et `voisins` fait revenir sa fenêtre par le bord opposé. Côté pair exigé pour un tore hexagonal |

Par exemple, `'(144 2 :lattice :hex :boundary :torus)` décrit un tore hexagonal de 12×12 neurones. `learn` lit cette topologie une fois par pas et la transmet sous forme de valeurs simples, par les mots-clés `:side`, `:lattice` et `:boundary`, à la métrique et à `voisins`. Les fonctions de `maths.lisp` restent ainsi indépendantes des classes de réseau, et s'appellent toujours comme avant sans mot-clé : `(euclidian a b)` est la distance ordinaire. `init` et `save` conservent la topologie.

La numérotation des neurones (`2d`/`d2`) reste la même pour toutes les topologies : seules leurs positions, donc leurs distances, changent. L'interface graphique dessine toujours une grille carrée.

**Défaut d'origine, corrigé :** sur une grille bornée, `voisins` laissait des doublons dans sa fenêtre aux bords de la carte, car `remove-duplicates` comparait les coordonnées avec `eql`, qui ne reconnaît pas deux listes égales comme identiques. Au coin (0, 0) avec un rayon de 1, il renvoyait 9 coordonnées pour 4 cases, et `learn` corrigeait ces neurones plusieurs fois par pas : le gagnant d'un coin, 4 fois. La comparaison se fait maintenant avec `equal`. Cela change aussi le ROSOM, qui utilise `voisins`, et le réglage historique ne reproduit plus l'ancien code aux bords de la carte. Le tore n'était pas concerné.

# Mesures avec le vrai code

Carte 12×12, 16 entrées, formes qui varient continûment, 15 époques, une seule graine. Toutes les erreurs sont mesurées à la distance euclidienne.

| Réglage | Neurones utilisés | Erreur topographique | Erreur de quantification |
|---|---|---|---|
| historique (`neuromuse-distance`, `:raw`) | 32 / 144 | 25,3 % | 0,63 |
| euclidienne, `:raw` | 136 / 144 | 5,6 % | 0,22 |
| **défaut** (euclidienne, `:normalized`) | **135 / 144** | **2,4 %** | **0,26** |
| défaut + métrique `'manhattan` | 133 / 144 | 7,5 % | 0,23 |
| défaut + métrique `'chebyshev` | 135 / 144 | 3,5 % | 0,26 |
| défaut + grille hexagonale (`:lattice :hex`) | 137 / 144 | 7,1 % | 0,26 |
| grille hexagonale + `'chebyshev` (pas hexagonaux) | 138 / 144 | 5,5 % | 0,26 |
| défaut + tore (`:boundary :torus`) | 113 / 144 | 3,7 % | 0,29 |
| défaut + distance `'cosine` | 96 / 144 | 12,9 % | 0,42 |

L'erreur topographique est la part des entrées dont le premier et le deuxième gagnant ne sont pas voisins sur la grille, voisins voulant dire à une distance d'au plus 1,5 dans la géométrie de la carte : 8 voisins sur la grille carrée et sur le tore, 6 sur la grille hexagonale, plus exigeante. L'erreur de quantification est l'écart moyen entre une entrée et les poids de son gagnant.

Ces mesures sont faites après la correction de `voisins`. La normalisation améliore nettement la topologie (de 5,6 % à 2,4 %), pour une erreur de quantification à peine plus haute. Toutes les grilles s'organisent bien. Mais il s'agit d'un seul essai, avec une seule graine : la correction de `voisins`, qui ne touche que les bords, a suffi à faire passer l'erreur topographique de Manhattan de 4,1 % à 7,5 %, et celle du cosinus de 6,8 % à 12,9 %. Des écarts de quelques points entre réglages relèvent donc du hasard de l'apprentissage ; il faudrait plusieurs graines pour classer les réglages.

Le tore utilise moins de neurones parce que ces données ne sont pas cycliques : la position des formes va de 0 à 1 sans boucler, et la couture du tore reste en partie vide. Un tore convient aux données qui bouclent : phases, classes de hauteur, cycles rythmiques.

Le cosinus ignore l'amplitude, l'un des trois paramètres des données, d'où moins de neurones utilisés ; son erreur de quantification, mesurée en distance euclidienne donc amplitude comprise, n'est pas comparable aux autres.

# La normalisation du voisinage

Avec `:normalized`, la largeur de la gaussienne vaut :

  largeur = radius × min(1, erreur / max-error)

où `max-error` est la plus grande erreur de gagnant vue depuis `init`. L'erreur pilote toujours la largeur, comme en 1999, mais elle est ramenée entre 0 et 1, donc exprimée en cases de la grille et indépendante de l'échelle des données. C'est l'idée du PLSOM de Berglund et Sitte (2006).

À erreur maximale, on retrouve exactement la formule classique laissée en commentaire au-dessus de `gaussian-hat`, exp(−d² / (2·radius)²). Quand la carte s'ajuste, les erreurs diminuent et le voisinage se resserre de lui-même.

`max-error` ne fait que croître : une entrée aberrante peut le figer trop haut et resserrer ensuite tout le voisinage. On peut le remettre à zéro à la main, et `init` le fait.

# Exemples

```lisp
;; SOM de Kohonen classique : rien à régler
(make-instance 'som :name 'carte :size 144 :input 128)

;; voisinage carré, fidèle à la fenêtre de VOISINS
(setf (grid-distance carte) 'chebyshev)

;; grille hexagonale
(setf (topology carte) '(144 2 :lattice :hex))

;; tore
(setf (topology carte) '(144 2 :boundary :torus))

;; tore hexagonal, voisinage en hexagone (pas hexagonaux)
(setf (topology carte) '(144 2 :lattice :hex :boundary :torus)
      (grid-distance carte) 'chebyshev)

;; coder la forme des entrées plutôt que leur intensité
(setf (distance carte) 'cosine)

;; appel direct, comme EUCLIDIAN
(cosine '(1 0) '(0 1))           ; => 1.0
(euclidian '(0 0) '(11 0))                            ; => 11.0
(euclidian '(0 0) '(11 0) :side 12 :boundary :torus)  ; => 1.0
(chebyshev '(1 1) '(0 2) :lattice :hex)               ; => 2 pas

;; comportement des versions 1999-2001 (sauf aux bords, cf. VOISINS)
(setf (distance carte) 'neuromuse-distance
      (error-scaling carte) :raw)

;; recharger une carte sauvegardée avant octobre 2026
(load "ancienne-carte.lisp")   ; la carte reprend son nom, ici ANCIENNE-CARTE
(setf (distance ancienne-carte) 'neuromuse-distance
      (error-scaling ancienne-carte) :raw)
```

----

##A propos du voisinage normalisé##

D'une discussion avec claude.ia, il résulte ce qui suit :

La normalisation change : l'unité de la largeur, son évolution au cours de l'apprentissage, et le rôle de radius. Elle a aussi deux limites, qu'il faut connaître.

###1. La largeur ne dépend plus de l'échelle des données###

Avant, la largeur valait l'erreur brute du neurone, exprimée dans les unités de l'entrée, puis comparée à une distance en cases de la grille. Le voisinage dépendait donc de l'amplitude des données :
avec des données dix fois plus grandes, les erreurs et la largeur sont dix fois plus grandes : toute la fenêtre de voisins est corrigée presque comme le gagnant ;
avec des données petites, la largeur tombe sous une case. Pour une erreur de 0,3, un voisin direct ne reçoit que exp(−1/0,6²) ≈ 6 % de la correction du gagnant. La carte se comporte alors presque comme un k-means, où chaque neurone apprend seul.

Avec :normalized, seul le rapport erreur / max-error compte. Multiplier toutes les données par 10 ne change plus rien au voisinage.

###2. Le voisinage se resserre de lui-même###

Au début, les poids sont aléatoires et les erreurs grandes : le rapport est proche de 1 et la largeur vaut radius. La carte s'organise alors globalement, et la topologie se met en place. Quand la carte s'ajuste, les erreurs baissent par rapport au maximum vu, et le voisinage se resserre.
C'est l'équivalent du rayon décroissant d'un SOM classique, sans calendrier à fixer. Cela explique les mesures :

l'erreur topographique dmiminue mieux parce que les voisins sont vraiment entraînés ensemble au début ;
l'erreur de quantification monte un peu, de 0,23 à 0,25, parce qu'un voisinage plus large tire les neurones les uns vers les autres. C'est le compromis habituel d'un SOM, qui sacrifie un peu de précision à la continuité de la carte.

###3. Chaque voisin garde sa propre largeur###

Comme dans le code de Nice, chaque voisin utilise sa propre erreur, maintenant divisée par max-error :

un voisin déjà proche de l'entrée a une petite erreur, donc une gaussienne étroite, et il est peu corrigé ;
un voisin éloigné de l'entrée a une erreur forte, plafonnée à 1, et il est corrigé presque au maximum ;
le gagnant, à distance zéro sur la grille, apprend toujours au taux learn-fact, quelle que soit sa largeur.

Le voisinage tire donc surtout les neurones qui détonnent, ce qui renforce la continuité de la carte. Ce trait vient de Nice (2004) et la normalisation le conserve.

###4. `radius` change de rôle###

En `:raw`, le radius ne faisait que borner la fenêtre carrée de voisins ; la forme de la gaussienne n'en dépendait pas. Maintenant, il fixe aussi la largeur maximale de la gaussienne. Le rejeu de versions de SOM précédente aura un voisinage nettement plus large qu'avant.

###5. Limite : max-error a une mémoire sans oubli###

`max-error` ne fait que croître depuis init. Deux conséquences :

une valeur aberrante fige l'échelle : une seule entrée très éloignée fait monter max-error, et tous les rapports deviennent petits. Le voisinage se resserre d'un coup, et la carte se fige trop tôt ;
la plasticité ne revient qu'en partie : si de nouvelles données très différentes arrivent, par exemple en situation de jeu en temps réel via UDP, leurs erreurs font remonter le rapport vers 1, et la carte se réorganise. C'est un bon comportement, mais seulement si max-error n'a pas été gonflé auparavant par une valeur aberrante.

Une variante courante consiste à faire oublier lentement le maximum, à chaque pas : max-error ← max(erreur, max-error × facteur), avec un facteur proche de 1, par exemple 0,999. La carte s'adapte alors à une échelle d'erreur qui évolue.

**6. Limite : le taux d'apprentissage reste constant**

Le PLSOM de Berglund et Sitte normalise aussi le taux d'apprentissage par le même rapport.
`learn-fact` est fixe puisque destiné à être controlé manuellement, ou par une lambda (à préciser).

Nuance avec neuromuse-distance :

Avec cette distance, l'erreur est proportionnelle à l'amplitude de l'entrée. La normalisation supprime l'échelle globale des données, mais pas l'effet relatif : une trame forte, comparée à la plus forte déjà vue, élargit encore le voisinage plus qu'une trame faible.
