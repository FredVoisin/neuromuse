;;;; xor4-noisy-train.lisp - Entrainement du MLP XOR sur des entrees legerement
;;;; bruitees, une pause separant chaque essai : pense pour etre regarde en
;;;; direct via neuromuse-gui (heatmap ou graphe des poids, courbe d'erreur),
;;;; le bruit et la pause donnant le temps de suivre les couleurs a l'oeil.
;;;;
;;;; Usage, package :neuromuse courant :
;;;;   (load "examples/xor4-noisy-train.lisp")
;;;;   (neuromuse-gui:gui 'xor')          ; ouvre la fenetre (autre systeme, voir gui.lisp)
;;;;   (train-xor4-noisy)                 ; bloquant : le REPL attend la fin
;;;;
;;;; Pour garder le REPL libre pendant l'entrainement (par exemple pour
;;;; cliquer sur les boutons de la fenetre entre-temps), lancez la boucle dans
;;;; son propre thread, comme le fait deja neuromuse-gui:gui pour la fenetre :
;;;;   (mk-process "train-xor" #'train-xor-noisy)
;;;;
;;;; Pour interrompre une boucle en cours (bloquante ou non) :
;;;;   (setf *stop* t)

(in-package :neuromuse)

;; Cree le reseau seulement s'il n'existe pas deja : ce fichier peut etre
;; charge seul, ou apres examples/mlp-test.lisp, sans reinitialiser un XOR
;; deja en cours d'apprentissage.
(unless (and (boundp 'xor4) (ann-p xor4))
  (make-mlp xor4 4 1 2))

(setf (learn-fact xor4) .4
      (temp xor4) 0.1
      (net-temp xor4) .01
      (threshold xor4) .1
      (latence xor4) .01)

(defparameter *xor4-patterns*
  '(((1 0 0 0) (0))
    ((1 0 1 0) (1))
    ((0 1 0 1) (1))
    ((1 1 1 1) (0)))
  "Table de verite du XOR : (entrees) (sortie attendue), non bruitee.")

(defvar *stop-xor4* nil
  "Positionner a T (depuis une autre commande, ou un autre thread si la
boucle tourne via MK-PROCESS) pour interrompre une TRAIN-XOR-NOISY en cours.")


(defun noisy (input amount)
  "INPUT (liste de nombres) avec, sur chaque valeur, un bruit uniforme entre
-AMOUNT et +AMOUNT ajoute."
  (mapcar (lambda (x) (+ x (- (random (* 2.0 amount)) amount))) input))

(defun train-xor4-noisy (&key (net 'xor4) (iterations 10000) (noise .10)
                              (verbose nil))
  "Boucle d'apprentissage du XOR sur NET (instance mlp, ou symbole qui la
nomme -- XOR par defaut) : a chaque essai, un des 4 cas du XOR tire au hasard,
ses deux entrees legerement bruitees (+/- NOISE), sa sortie cible inchangee ;
une pause de SLEEP secondes separe chaque essai pour suivre l'apprentissage a
l'oeil (voir neuromuse-gui:gui). S'arrete apres ITERATIONS essais, ou
immediatement si (setf *stop* t).

Note : la convergence du XOR classique (2 neurones caches, sans momentum) est
sensible a l'initialisation aleatoire des poids -- selon le tirage, quelques
centaines ou plusieurs dizaines de milliers d'essais peuvent etre necessaires
avant que le reseau ne se distingue du hasard. Ce n'etait pas la peine
d'arreter la boucle des que l'erreur d'UN SEUL exemple bruite tombe bas par
chance (ce que faisait une premiere version de cette fonction) : ca stoppait
avant toute vraie convergence."
  (let ((mlp (if (symbolp net) (symbol-value net) net)))
    (setf *stop-xor4* nil)
    (dotimes (k iterations)
      (let ((sleep (latence mlp)))
	(when *stop*
          (when verbose (format t "~&> interrompu a l'epoch ~D.~%" (epoch mlp)))
          (return))
	;(destructuring-bind (in goal) (nth (random 4) *xor-patterns*)
	(destructuring-bind (in goal) (nth (mod k 4)  *xor4-patterns*)
	  (setf (input mlp) (noisy in noise)
		(goal mlp) goal)
          (let ((e (backpropagate mlp)))
            (push e (history-error mlp))
            (incf (epoch mlp))
            (when verbose
              (format t "~&epoch ~D : entree ~,3F ~,3F -> ~,3F (but ~D), erreur ~,5F~%"
                      (epoch mlp) (first (input mlp)) (second (input mlp))
                      (first (output mlp)) (first goal) e))))
	(when (plusp sleep)  (sleep sleep))))
    (values mlp (epoch mlp) (car (history-error mlp)))))

;; EOF
