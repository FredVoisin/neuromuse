;;;; hopfield.lisp
;;;; Reseau de Hopfield : memoire associative a seuil binaire, NET une
;;;; matrice IN-SIZE x IN-SIZE symetrique de poids (NET[i][j] = NET[j][i]),
;;;; apprise par une regle hebbienne (LEARN) et interrogee par un rappel a
;;;; seuil (RUN-HOPFIELD) -- presenter un motif partiel/bruite retrouve le
;;;; motif appris le plus proche.
;;;;
;;;; Porte dans neuromuse depuis legacy/neuromuse-2001-concert/"ANN 1.4.lisp"
;;;; (MCL, 2001, "reseau de Hopfield"). L'original est conserve tel quel sous
;;;; legacy/.
;;;;
;;;; Correspondances avec le code de 2001 :
;;;;   init-hopfield          -> init-hopfield-net / make-hopfield
;;;;   train-hopfield         -> learn (specialise sur hopfield, meme
;;;;                              convention "ne renvoie que self" que pour
;;;;                              mlp/rmlp/som/rosom/perceptron)
;;;;   run-hopfield (defun)   -> run-hopfield (methode)
;;;;   binary                 -> deja dans maths.lisp
;;;;   vector-difference, vector-addition, vector-* : definies dans le code
;;;;     de 2001 mais jamais appelees par TRAIN-HOPFIELD/RUN-HOPFIELD -- non
;;;;     reprises ici ; SUBSTRACT-2-VECTORS/ADD-TWO-VECTORS/MULTIPLY-2-VECTORS
;;;;     (maths.lisp) couvrent deja le meme role si jamais utile un jour.

(in-package :neuromuse)

(defclass hopfield (ann)
  ((name :initform 'hopfield :initarg :name :accessor name :type symbol)
   (in-size :initform 2 :initarg :in-size :accessor in-size :type integer)
   (net :initform nil :initarg :net :accessor net :type (or null array))
   (previous :initform '() :initarg :previous :accessor previous :type list))
  (:documentation
   "Reseau de Hopfield. NET est un tableau CL IN-SIZE x IN-SIZE (pas une
liste comme pour mlp/perceptron, ni des instances NEURON comme pour
som/rosom) : NET[i][j] est le poids entre les cellules i et j, toujours
egal a NET[j][i] -- LEARN ne touche d'ailleurs qu'a la moitie superieure
(i < j) de la matrice, recopiee dans l'autre pour garder la symetrie."))

(defmethod print-object ((self hopfield) stream)
  (format stream "<hopfield ~S: ~D cell~:P>"
          (string (name self)) (in-size self))
  (values))

(defun init-hopfield-net (in-size)
  "Matrice IN-SIZE x IN-SIZE a zero : l'etat d'un Hopfield neuf, avant tout
apprentissage. (Le code de 2001 laissait MAKE-ARRAY sans :INITIAL-ELEMENT,
ce qui revient au meme sous SBCL -- un tableau general non initialise y est
deja a 0 -- mais ce n'est pas garanti par le standard ; explicite ici plutot
que de compter dessus.)"
  (make-array (list in-size in-size) :initial-element 0))

(defmacro make-hopfield (name in-size)
  ;; comme make-perceptron (perceptron.lisp) : l'expansion est du CODE qui
  ;; construit l'instance au chargement, pas une instance litterale.
  (cond ((not (boundp name))
         `(defvar ,name
            (make-instance 'hopfield
              :name ',name
              :in-size ,in-size
              :net (init-hopfield-net ,in-size)
              :creation-date (get-universal-time))))
        ((ann-p (symbol-value name))
         (warning-msg (format nil "~S already exists ! ~S"
                              name (type-of (symbol-value name)))))
        (t
         `(setf ,name
            (make-instance 'hopfield
              :name ',name
              :in-size ,in-size
              :net (init-hopfield-net ,in-size)
              :creation-date (get-universal-time))))))

(defmethod learn ((self hopfield))
  "Un pas d'apprentissage hebbien : presente (input self) (un vecteur/une
liste bipolaire, 0/1 par cellule) et renforce chaque synapse i<j selon la
coincidence des deux cellules (regle de Hebb, la contribution de ce pas
normalisee par IN-SIZE puis ajoutee au poids existant -- cf. le fix plus
bas pour ce qui a change par rapport a TRAIN-HOPFIELD en 2001). Renvoie
SELF, comme tout LEARN."
  (let ((input (input self))
        (tnet (net self)))
    (dotimes (i (length input))
      (dotimes (j (length input))
        (when (< i j)
          ;; FIX : le code de 2001 divisait (+ ancien-poids contribution) par
          ;; IN-SIZE -- donc l'ancien poids ETAIT LUI AUSSI redivise a chaque
          ;; apprentissage, pas seulement la nouvelle contribution. Avec des
          ;; appels repetes (plusieurs motifs, ou plusieurs epoques), cela
          ;; fait decroitre geometriquement les motifs appris plus tot, bien
          ;; avant la limite de capacite habituelle d'un Hopfield standard
          ;; (mesure : 1/5 motifs aleatoires rappeles exactement sur 100
          ;; cellules, la limite ~0.138*N en prevoyant ~13). La regle de Hebb
          ;; normalise la CONTRIBUTION de chaque motif par IN-SIZE, pas le
          ;; poids deja accumule par les motifs precedents -- seule la
          ;; contribution est donc divisee ci-dessous, avant d'etre ajoutee.
          (setf (aref (net self) i j)
                (float (+ (aref tnet i j)
                          (/ (* (- (* 2 (elt input i)) 1)
                                (- (* 2 (elt input j)) 1))
                             (length input))))
                (aref (net self) j i)
                (aref (net self) i j)))))
    (setf (epoch self) (1+ (epoch self)))
    (values self)))

(defmethod run-hopfield ((self hopfield) &key in)
  "Rappel a seuil binaire : pour chaque cellule i, (binary (sum_j NET[j][i]
* IN[j])) -- IN par defaut (input self). Ecrit et renvoie (output self),
meme convention que RUN-MLP/RUN-PERCEPTRON."
  (let* ((input (or in (input self)))
         (tnet (net self))
         (v (list)))
    (dotimes (i (length input))
      (let ((temp (list)))
        (dotimes (j (length input))
          (push (* (aref tnet j i) (elt input j)) temp))
        (push (binary (apply #'+ temp)) v)))
    (setf (output self) (coerce (nreverse v) 'vector))))

;;; exemple (cf. le #| |# original en 2001, legacy/neuromuse-2001-concert) :
;(make-hopfield h-test 4)
;(setf (input h-test) #(1 0 1 0)) (learn h-test)
;(run-hopfield h-test :in #(1 0 1 0))
;(run-hopfield h-test :in #(0 1 0 1))

; eof
