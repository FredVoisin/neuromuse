## Contexte

neurOMuse v1 (bêta) est l'intégration de neuromuse dans OpenMusic
(IRCAM), réalisée par Frédéric Voisin au CIRM (Nice). L'archive de
release `neurOMuse_v1.sit` a été créée le 26 septembre 2003.

## Contenu

- `neurOMuse_v1.sit` : archive StuffIt 5 d'origine (référence)
- `neurOMuse_v1.sit.lsar.txt` : métadonnées de l'archive (dates, codes
  type/créateur Mac), extraites avec `lsar -L`
- `neurOMuse_v1/` : contenu extrait avec `unar -k visible`
  - `>for OM lib folder/neurOMuse/` : la bibliothèque, à placer dans le
    dossier des bibliothèques d'OpenMusic
    - `neurOMuse.lisp` : loader
    - `neurOMuse sources/` : sources et fasl MCL (`.cfsl`), dont
      `neuromuse#03` (février 2003) et `neuromuse#04` (septembre 2003),
      deux états du moteur neuromuse
  - `>for OM lib folder/neurOMuse exemples/` : patches OpenMusic

## Notes de conservation

- Les fichiers `*.rsrc` sont les resource forks Mac d'origine, au format
  AppleDouble. `CLPF.rsrc` n'a pas de data fork : tout son contenu est
  dans `CLPF.rsrc.rsrc`.
- Les dates d'origine des fichiers, perdues par Git, sont consignées dans
  `neurOMuse_v1.sit.lsar.txt`.
- `scrolling-windows.lisp` (créé en 1990) est du code MCL tiers,
  antérieur à neuromuse.
