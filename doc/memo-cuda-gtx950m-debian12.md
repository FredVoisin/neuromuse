# Mémo — Configuration CUDA sur GTX 950M (Debian 12 "frog")

**Machine** : `frog`, Debian 12 (Bookworm)
**GPU** : NVIDIA GeForce GTX 950M — architecture Maxwell (GM108), compute capability 5.0
**Date** : juillet 2026
**Objectif** : faire fonctionner CUDA/cuBLAS sur cette carte, pour résoudre le problème `CUBLAS_STATUS_ARCH_MISMATCH` rencontré dans le projet [neuromuse](https://github.com/FredVoisin/neuromuse) (benchmark `mgl-mat`).

---

## 1. Contexte : pourquoi ça ne marchait pas "out of the box"

La GTX 950M est une puce Maxwell de première génération. Deux informations à bien dissocier :

- **Le driver NVIDIA** (pilote graphique) : toujours activement maintenu pour Maxwell dans les dépôts Debian (branche 535.x sur Bookworm, ou plus récent via le dépôt NVIDIA officiel). Aucun souci de ce côté.
- **Le toolkit CUDA** (compilateur `nvcc` + bibliothèques comme `cuBLAS`) : à partir de **CUDA 13.x**, NVIDIA a abandonné la génération de code pour les architectures Maxwell/Pascal (`sm_50`/`sm_60`). Résultat : `nvcc` refuse de compiler pour `sm_50`, et surtout, la bibliothèque `libcublas.so` livrée avec CUDA 13.x ne contient plus aucun kernel compilé pour Maxwell → erreur `CUBLAS_STATUS_ARCH_MISMATCH` à l'exécution, même si le code compile.

**Solution retenue** : garder le driver système (580.x) tel quel, et installer **CUDA Toolkit 12.6** en parallèle dans un dossier séparé (`/usr/local/cuda-12.6`), sans toucher au CUDA 13.3 déjà présent sur le système.

Note : une page glanée sur le web annonçait un driver "473.81 (2023)" comme dernière version stable pour cette carte — information non fiable (page générique, non sourcée). L'info correcte, vérifiée via Wikipedia et le wiki Debian : le support *Game Ready Driver* grand public de la série GeForce 900 s'est arrêté en novembre 2025 (sécurité jusqu'en octobre 2028), mais cela ne concerne pas le paquet driver maintenu par Debian, toujours fonctionnel.

---

## 2. Vérification du driver existant

```bash
nvidia-smi
```

Résultat obtenu :
```
NVIDIA-SMI 580.173.02   Driver Version: 580.173.02   CUDA Version: 13.0
GPU: NVIDIA GeForce GTX 950M
```

⚠️ **Piège** : la ligne `CUDA Version: 13.0` affichée par `nvidia-smi` indique la version *maximale de l'API CUDA que le driver sait piloter* — **pas** la version du toolkit à utiliser pour compiler. Le driver reste rétrocompatible avec CUDA 12.x.

---

## 3. Installation de CUDA Toolkit 12.6 en parallèle

### 3.1 Téléchargement

Récupéré depuis l'archive officielle NVIDIA (runfile local, sélection Linux/x86_64/Debian 12) :
```bash
wget https://developer.download.nvidia.com/compute/cuda/12.6.3/local_installers/cuda_12.6.3_560.35.05_linux.run
```

### 3.2 Premier essai — échec (conflit avec le driver existant)

```bash
sudo sh cuda_12.6.3_560.35.05_linux.run --toolkit --toolkitpath=/usr/local/cuda-12.6
```

Log d'erreur (`/var/log/cuda-installer.log`) :
```
[INFO]: Installing: Driver
[INFO]: Installing: 560.35.05
[ERROR]: Install of driver component failed.
[ERROR]: Install of 560.35.05 failed, quitting
```

**Cause** : en mode interactif (menu de cases à cocher), l'option `--toolkit` en ligne de commande ne décoche pas automatiquement la case "Driver". L'installeur a donc tenté d'installer son propre driver embarqué (560.35.05) en plus du 580.173.02 déjà actif et en cours d'utilisation par X11 → conflit → échec.

### 3.3 Deuxième essai — succès (mode silencieux)

```bash
sudo sh cuda_12.6.3_560.35.05_linux.run --silent --toolkit --toolkitpath=/usr/local/cuda-12.6
```

Avec `--silent`, l'installeur n'affiche plus de menu et n'installe **que** ce qui est listé explicitement (ici : le toolkit seul, pas de driver, pas de samples, pas de `nvidia-fs`). ✅ Terminé sans erreur.

**Note sur `--override`** : ce flag sert uniquement à contourner la vérification de version du compilateur hôte (gcc) si elle n'est pas dans la liste blanche de l'installeur — ça ne gère pas les conflits de driver. Avec gcc 12.2.0 (Debian 12 standard), il n'était en réalité pas nécessaire.

**Note sur "Kernel Objects (nvidia-fs)"** : à décocher/ignorer — c'est le module pour GPUDirect Storage (transferts NVMe→GPU directs), utile uniquement en contexte serveur/HPC, sans aucun intérêt ici.

---

## 4. Test de compilation simple (Hello World CUDA)

### 4.1 Fichier de test

```cuda
// test.cu
#include <stdio.h>
__global__ void hello() { printf("Hello depuis le GPU thread %d\n", threadIdx.x); }
int main() {
    hello<<<1, 5>>>();
    cudaDeviceSynchronize();
    return 0;
}
```

### 4.2 Premier essai — échec

```bash
/usr/local/cuda-12.6/bin/nvcc -arch=sm_50 test.cu -o test
# nvcc fatal : Failed to preprocess host compiler properties.
```

### 4.3 Diagnostic

Vérifications effectuées (toutes OK) :
```bash
which gcc g++      # /usr/bin/gcc, /usr/bin/g++
gcc --version      # gcc (Debian 12.2.0-14+deb12u1) 12.2.0
```

Le vrai coupable, révélé par le mode verbeux (`-v`) :
```
cc1plus: fatal error: test.cu: No such file or directory
```

➡️ Cause réelle : **le fichier `test.cu` n'était pas dans le répertoire courant** au moment de l'appel (il avait été créé dans `~` plutôt que dans `~/dev/frog`). Rien à voir avec CUDA ou gcc — simple erreur de chemin.

### 4.4 Succès

```bash
cd ~/dev/frog
LC_ALL=C /usr/local/cuda-12.6/bin/nvcc -ccbin g++ -arch=sm_50 test.cu -o test
./test
```

Résultat :
```
Hello depuis le GPU thread 0
Hello depuis le GPU thread 1
Hello depuis le GPU thread 2
Hello depuis le GPU thread 3
Hello depuis le GPU thread 4
```

✅ Le compilateur CUDA 12.6 compile et exécute correctement du code pour `sm_50` sur la GTX 950M.

*(Remarque : `LC_ALL=C` a été ajouté par précaution pendant le débogage, pour éviter tout souci de parsing de messages de compilateur en locale non-anglaise — mais le blocage réel était bien le chemin du fichier, pas la locale.)*

---

## 5. Test cuBLAS (le vrai objectif : reproduire/corriger `ARCH_MISMATCH`)

```cuda
// test_cublas.cu
#include <cublas_v2.h>
#include <cstdio>
int main() {
    cublasHandle_t handle;
    cublasStatus_t stat = cublasCreate(&handle);
    printf("cublasCreate status: %d (0 = SUCCESS)\n", stat);
    if (stat == CUBLAS_STATUS_SUCCESS) cublasDestroy(handle);
    return 0;
}
```

```bash
LC_ALL=C /usr/local/cuda-12.6/bin/nvcc -ccbin g++ test_cublas.cu -lcublas -o test_cublas
LD_LIBRARY_PATH=/usr/local/cuda-12.6/lib64 ./test_cublas
```

✅ Test réussi (`cublasCreate status: 0`) — confirmant que **CUDA 12.6 contient encore les kernels cuBLAS compilés pour Maxwell**, contrairement à CUDA 13.x.

---

## 6. Prochaine étape : brancher `mgl-mat`/`neuromuse` sur ce toolkit

Pour que le projet Lisp utilise cette installation CUDA 12.6 plutôt que le CUDA 13.3 système, lancer SBCL avec le bon `LD_LIBRARY_PATH` :

```bash
LD_LIBRARY_PATH=/usr/local/cuda-12.6/lib64:$LD_LIBRARY_PATH sbcl
```

À intégrer si besoin directement dans le script de lancement du projet `neuromuse`.

---

## 7. Résultat du rebranchement : `ARCH_MISMATCH` résolu, nouveau blocage en aval

Test effectué : relancer le benchmark `examples/benchmark-mgl-mat.lisp` du projet `neuromuse` avec
`LD_LIBRARY_PATH=/usr/local/cuda-12.6/lib64:$LD_LIBRARY_PATH`.

**Bonne nouvelle** : `CUBLAS_STATUS_ARCH_MISMATCH` a disparu — `cublasCreate` réussit désormais via
`mgl-mat`, comme le confirmait déjà le test C autonome de la section 5. Le fix CUDA 12.6 fonctionne
bien pour cuBLAS.

**Nouveau blocage, distinct** : `mgl-mat:with-cuda*` échoue quand même, mais plus tôt et pour une
autre raison. En traçant précisément (impression + `force-output` avant/après chaque étape), le
corps de `with-cuda*` n'est **jamais atteint** — l'échec a lieu pendant l'initialisation interne de
`with-cuda*` lui-même, avant tout code utilisateur :

- `with-cuda*` construit systématiquement un pool d'états aléatoires cuRAND (XORWOW) au démarrage,
  qu'on utilise ou non le hasard.
- Cette initialisation compile un noyau CUDA via `cl-cuda` (`nvcc -arch=sm_50 ... -ptx`, compilation
  réussie) puis tente de le charger/lancer sur le GPU.
- Ce chargement/lancement échoue ou reste bloqué. Après 30 secondes d'attente interne (le mécanisme
  de `with-cuda-pool` qui attend que les finaliseurs GC libèrent la mémoire CUDA), `mgl-mat` abandonne
  et lève une assertion sur la mémoire du pool (`n-bytes-allocated` non nul — exactement
  `n-random-states × 48` octets, reproductible avec `n-random-states` réduit à 32 comme à 4096) — un
  message qui masque la vraie erreur CUDA sous-jacente.

Reproductible avec un test minimal (aucune opération matricielle, juste entrer/sortir de
`with-cuda*`), donc indépendant de tout code du projet `neuromuse` ou du benchmark.

**Hypothèse** : contrairement au test C de la section 5 (qui utilise l'API CUDA Runtime de haut
niveau), `cl-cuda`/`mgl-mat` passent par l'API CUDA *Driver* bas niveau (`libcuda.so` système) pour
charger et lancer le PTX généré par `nvcc` 12.6. Il est possible que le JIT du driver système
(580.173.02, piloté par `libcuda.so`) ait un souci avec ce PTX ciblant `sm_50` dans ce contexte précis
— à creuser si besoin (voir si un lancement de noyau plus simple via `cl-cuda` seul, sans passer par
`mgl-mat`, reproduit le blocage).

**Statut actuel** : cuBLAS fonctionne (prouvé en C), mais `mgl-mat` ne peut pas encore l'utiliser
depuis Lisp sur cette carte à cause de ce blocage en amont, dans l'initialisation cuRAND. Le
benchmark CPU/BLAS (sans CUDA) de `neuromuse` reste, lui, pleinement fonctionnel et concluant : à
la taille réelle des réseaux du projet, le code list-based existant reste ~40x plus rapide que
`mgl-mat`/BLAS, donc ce blocage CUDA ne change pas la conclusion pratique pour `neuromuse` — piste
mise de côté pour l'instant, pas abandonnée.

---

## 8. Récapitulatif des composants finaux

| Composant | Version | Emplacement |
|---|---|---|
| Driver NVIDIA | 580.173.02 | système (dépôt) |
| CUDA Toolkit (système, non utilisé pour cette carte) | 13.3 | `/usr/local/cuda-13.3` |
| CUDA Toolkit (utilisé pour Maxwell) | 12.6.3 | `/usr/local/cuda-12.6` |
| GPU | GTX 950M (Maxwell, `sm_50`) | — |
| cuBLAS via CUDA 12.6 | fonctionne (`cublasCreate` OK, testé en C et via `mgl-mat`) | — |
| `mgl-mat`/`cl-cuda` depuis Lisp | bloqué : lancement de noyau cuRAND au démarrage de `with-cuda*` échoue/reste bloqué | — |

---

*Fin du mémo.*
