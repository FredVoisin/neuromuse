# neuromuse — version du concert *L'Écarlate* (2001)

**Archive historique, non maintenue.** Ces fichiers sont conservés tels
quels, comme référence. Ils ne sont pas destinés à être corrigés ni
chargés avec la version actuelle de neuromuse.

Le fichier `ANN1.4.lisp` est le code qui a donné naissance par la suite
au projet Lisp neuromuse. Il est lui-même issu d'une version antérieure,
datée de mars 1999 (voir `../neuromuse-1999-03/`, à venir).

## Contexte

Cette version de neuromuse est celle qui a été utilisée pour
*L'Écarlate*, de Kasper T. Toeplitz et Myriam Gourfink, créée le
7 juin 2001 à l'Ircam (Espace de projection, Paris), dans le cadre du
festival Agora.

Rôle de neuromuse dans l'œuvre : [à compléter]

Fiche du concert :
https://ressources.ircam.fr/fr/media/x218df0_lecarlate-myriam-gourfink-kasper-t-toeplit

## Environnement d'origine

- Macintosh Common Lisp, Mac OS 9
- PowerBook G3 « Pismo » (PowerPC)
- Communication en temps réel via MIDI (MidiShare) avec la scène et avec
  l'ordinateur dédié à l'analyse de la danse (voir le projet Laban on
  Lisp)

## Contenu

## Contenu

Chargement et interface
- ` ECARLATE-loader.lisp` : loader de l'ensemble (l'espace initiale le
  plaçait en tête dans le Finder)
- `Ecarlate-menu.lisp` : menu MCL propre au concert
- `gel-control.lisp`, `Resin-control.lisp` : interfaces graphiques en
  Lisp, ajoutées au menu, pour d'autres contrôles pendant le concert

Réseaux de neurones
- `ANN 1.4.lisp` : bibliothèque de réseaux de neurones, noyau du futur
  neuromuse
- `ANN 1.4-old.pfsl` : version compilée antérieure de la bibliothèque
- `annthermometer.pfsl` : affichage du taux d'erreur en temps réel et
  contrôle de la température du réseau (compilé) ; ancêtre de
  `neurOM-thermometer` dans la version OpenMusic de 2003
- `ecarl180-1.lisp`, `ecarl180-1.net@3584-2805` : sauvegardes
  d'instances du réseau `ecarl180` ; `@3584-2805` indique les numéros
  d'epochs d'apprentissage
- `sin1C` : sauvegarde de l'instance du réseau `sin1C`, un MLP récurrent
  à la Jordan
- `Temp.lisp` : commandes Lisp pour afficher la courbe d'apprentissage

Temps réel et MIDI
- `MidiShare.pfsl` : interface MidiShare pour MCL, compilée
- `getmidi.lisp`, `MIDI&PROC.lisp` : contrôle en MIDI via MidiShare
- `chrono.lisp` 

Exécution pendant le concert
- `run-ecsin.lisp`, `run-ecsinII.lisp`, `run-ecsin2.lisp` : variantes
  d'expressions prêtes à être évaluées pendant le concert

Les fichiers `.pfsl` sont des fichiers compilés (fasl) pour MCL sur
PowerPC.

## Notes de conservation

- Fichiers en fins de ligne CR (Mac classique) ; pour les lire sur un
  système actuel :
  `tr '\r' '\n' < fichier | iconv -f MACINTOSH -t UTF-8`
- Les dates de modification d'origine des fichiers ont été perdues lors
  de copies successives. La date du commit correspond à celle du concert.
