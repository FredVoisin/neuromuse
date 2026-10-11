;;;; auto-assoc.lisp
;;;; Memoire auto-associative : NET une matrice IN-SIZE x IN-SIZE symetrique
;;;; de poids, apprise par la regle de Widrow-Hoff pour reproduire/associer
;;;; chaque motif presente a lui-meme (LEARN), puis rappelee en faisant
;;;; traverser un motif (eventuellement partiel/bruite) a ces memes poids
;;;; (RUN-AUTO-ASSOC) -- la meme idee qu'un auto-encodeur moderne (une seule
;;;; couche ici, pas de goulot d'etranglement), d'ou le fichier ; la classe
;;;; elle-meme garde le nom du code d'origine, AUTO-ASSOC, plus precis pour
;;;; ce que fait reellement cette regle d'apprentissage.
;;;;
;;;; Porte dans neuromuse depuis legacy/neuromuse-2001-concert/"ANN 1.4.lisp"
;;;; (MCL, 2001, "RESEAUX AUTO-ASSOCIATIFS"). L'original est conserve tel
;;;; quel sous legacy/.
;;;;
;;;; Correspondances avec le code de 2001 :
;;;;   init-auto-assoc  -> init-auto-assoc-net / make-auto-assoc
;;;;   train-aa         -> learn (specialise sur auto-assoc, meme convention
;;;;                       "ne renvoie que self" que pour mlp/rmlp/som/rosom/
;;;;                       perceptron/hopfield)
;;;;   run-aa (defun)   -> run-auto-assoc (methode)
;;;;   widrow-Hoff, logistic : deja dans maths.lisp (WIDROW-HOFF/LOGISTIC)
;;;;   vector-difference, vector-addition : memes fonctions, jamais
;;;;     appelees, que celles du fichier Hopfield de 2001 -- non reprises
;;;;     ici pour la meme raison (cf. hopfield.lisp).
;;;;   see, view-aa-activation, make-activation-window : utilitaires
;;;;     d'affichage specifiques a MCL (fenetre QuickDraw) et a la forme
;;;;     exacte des donnees de l'epoque (grille 53x11 codee en dur) -- pas
;;;;     repris ; src/gui.lisp (Ltk) est l'equivalent actuel, mais seulement
;;;;     pour mlp/rmlp/som a ce jour, pas encore pour cette classe.

(in-package :neuromuse)

(defclass auto-assoc (ann)
  ((name :initform 'auto-assoc :initarg :name :accessor name :type symbol)
   (in-size :initform 2 :initarg :in-size :accessor in-size :type integer)
   (net :initform nil :initarg :net :accessor net :type (or null array))
   (previous :initform '() :initarg :previous :accessor previous :type list))
  (:documentation
   "Memoire auto-associative (un seul auto-encodeur lineaire a une couche :
pas de goulot d'etranglement, IN-SIZE neurones s'associant entre eux).
NET est un tableau CL IN-SIZE x IN-SIZE (pas une liste comme pour
mlp/perceptron, ni des instances NEURON comme pour som/rosom) : NET[i][j]
est le poids entre les cellules i et j, toujours egal a NET[j][i] -- LEARN
le maintient symetrique a chaque mise a jour."))

(defmethod print-object ((self auto-assoc) stream)
  (format stream "<auto-assoc ~S: ~D cell~:P>"
          (string (name self)) (in-size self))
  (values))

(defun init-auto-assoc-net (in-size)
  "Matrice IN-SIZE x IN-SIZE de poids aleatoires dans [-1, 1[, symetrique
(NET[i][j] = NET[j][i] des le tirage) : l'etat d'un AUTO-ASSOC neuf."
  (let ((net (make-array (list in-size in-size))))
    (dotimes (i in-size)
      (dotimes (j in-size)
        (let ((r (- (random 2.0) 1.0)))
          (setf (aref net i j) r
                (aref net j i) r))))
    net))

(defmacro make-auto-assoc (name in-size)
  ;; comme make-perceptron (perceptron.lisp) : l'expansion est du CODE qui
  ;; construit l'instance au chargement, pas une instance litterale.
  (cond ((not (boundp name))
         `(defvar ,name
            (make-instance 'auto-assoc
              :name ',name
              :in-size ,in-size
              :net (init-auto-assoc-net ,in-size)
              :creation-date (get-universal-time))))
        ((ann-p (symbol-value name))
         (warning-msg (format nil "~S already exists ! ~S"
                              name (type-of (symbol-value name)))))
        (t
         `(setf ,name
            (make-instance 'auto-assoc
              :name ',name
              :in-size ,in-size
              :net (init-auto-assoc-net ,in-size)
              :creation-date (get-universal-time))))))

(defmethod learn ((self auto-assoc))
  "Un pas d'apprentissage : presente (input self), et corrige chaque
synapse par la regle de Widrow-Hoff (WIDROW-HOFF, maths.lisp) a partir de
l'activation de chaque cellule : activation_i = somme_j NET[i][j] * IN[j]
(FIX : le code de 2001, TRAIN-AA, utilisait (ELT INPUT I) au lieu de
(ELT INPUT J) dans cette somme et dans le XI de WIDROW-HOFF -- l'activation
de la cellule I valait alors IN[I] * (somme de sa ligne de poids), qui ne
depend pas du reste de l'entree ; cf. RUN-AUTO-ASSOC plus bas, meme fix).
Renvoie SELF, comme tout LEARN."
  (let ((input (input self))
        (l (learn-fact self)))
    (dotimes (i (length input))
      (let ((activation 0))
        (dotimes (j (length input))
          (incf activation (* (aref (net self) i j) (elt input j))))
        (dotimes (j (length input))
          (setf (aref (net self) i j)
                (widrow-hoff (aref (net self) i j) (elt input i)
                             (logistic activation) (elt input j) l)
                (aref (net self) j i)
                (aref (net self) i j)))))
    (setf (epoch self) (1+ (epoch self)))
    (values self)))

(defmethod edges ((self auto-assoc))
  "Les arcs de SELF : (net self) est un tableau IN-SIZE x IN-SIZE
symetrique -- un seul arc par paire (i < j), pas les deux sens. Noeuds
identifies par l'indice de cellule (un entier), meme convention que
EDGES sur HOPFIELD."
  (let ((net (net self)) (n (in-size self)))
    (loop for i below n
          append (loop for j from (1+ i) below n
                       collect (list i j (aref net i j))))))

(defmethod run-auto-assoc ((self auto-assoc) &key in (fct #'tanh))
  "Rappel : pour chaque cellule i, (FCT (somme_j NET[i][j] * IN[j])) -- IN
par defaut (input self) (FIX : le code de 2001, RUN-AA, utilisait
(ELT INPUT I) au lieu de (ELT INPUT J) ici, meme probleme que LEARN
ci-dessus). Ecrit et renvoie (output self), comme
RUN-MLP/RUN-PERCEPTRON/RUN-HOPFIELD -- mais ici une LISTE (comme
(output mlp)), pas un vecteur (RUN-HOPFIELD renvoie un vecteur ; meme
asymetrie qu'en 2001, non harmonisee ici)."
  (let ((input (or in (input self)))
        (answer (list)))
    (dotimes (i (length input))
      (let ((temp (list)))
        (dotimes (j (length input))
          (push (* (aref (net self) i j) (elt input j)) temp))
        (push (funcall fct (apply #'+ temp)) answer)))
    (setf (output self) (nreverse answer))))

;;; exemple (cf. le #| |# original en 2001, legacy/neuromuse-2001-concert) :
;(make-auto-assoc aa-test 9)
;(setf (learn-fact aa-test) .1)
;(dotimes (n 50)
;  (setf (input aa-test) #(1 0 0 0 0 0 0 0 1)) (learn aa-test)
;  (setf (input aa-test) #(0 1 0 0 0 0 0 1 0)) (learn aa-test)
;  (setf (input aa-test) #(0 0 0 1 1 1 0 0 0)) (learn aa-test))
;(run-auto-assoc aa-test :in #(1 0 0 0 0 0 0 0 1))

; eof
