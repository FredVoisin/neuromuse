;;;; perceptron.lisp
;;;; Perceptron simple (une seule couche de synapses, entrées -> sorties),
;;;; apprentissage par la règle de Widrow-Hoff (règle delta), biais optionnel.
;;;;
;;;; Portage dans neuromuse du perceptron original
;;;; IRCAM, mars 1999 (perceptron.lisp / run_perceptron.lisp, package :om,
;;;; MCL), code d'origine du projet neuromuse. Les originaux sont conservés
;;;; tels quels sous legacy/.
;;;;
;;;; Correspondances avec le code de 1999 :
;;;;   init-perceptron      -> init-perceptron-net / make-perceptron
;;;;   outputcell-activity  -> perceptron-activity
;;;;   perceptron-out       -> run-perceptron        (out-fun = #'binary)
;;;;   run-perceptron-B     -> run-perceptron        (out-fun = #'boltzmann, temp > 0)
;;;;   percept-learn,
;;;;   percept-B-learn      -> learn (specialise sur perceptron ; ex LEARN-PERCEPTRON,
;;;;                           renomme pour rejoindre le LEARN de mlp/rmlp/som/rosom)
;;;;   train-perceptron,
;;;;   train-perceptron-B   -> train-perceptron
;;;;   cell-output          -> binary              (maths.lisp)
;;;;   cell-output-boltz    -> boltzmann           (maths.lisp)
;;;;   widrow-hoff, multiply-2-vectors, vector-sum, compare-vectors,
;;;;   boltzmann-distr, ldlp, scale-input, test-t : déjà dans maths.lisp / misc.lisp

(in-package :neuromuse)

(defclass perceptron (ann)
  ((name :initform 'perceptron :initarg :name :accessor name :type symbol)
   (in-size :initform 2 :initarg :in-size :accessor in-size :type integer)
   (out-size :initform 1 :initarg :out-size :accessor out-size :type integer)
   (bias :initform nil :initarg :bias :accessor bias)
   (previous :initform '() :initarg :previous :accessor previous :type list)
   (goal :initform '() :initarg :goal :accessor goal)
   (learn-fact :initform 0.1 :initarg :learn-fact :accessor learn-fact :type float)
   (out-fun :initform #'binary :initarg :out-fun :accessor out-fun :type function)
   (target-error :initform 0.0 :initarg :target-error :accessor target-error :type number)
   (stop :initform 1000 :initarg :stop :accessor stop :type integer))
  (:documentation
   "Perceptron simple (Rosenblatt), sans couche cachée.
NET est une liste de IN-SIZE lignes de OUT-SIZE poids : (nth j (nth i net)) est
le poids de la synapse de l'entrée i vers la sortie j.
Si BIAS est vrai, NET a une ligne de plus, la dernière, qui porte le poids de
biais de chaque sortie : une entrée supplémentaire toujours à 1, ajoutée
automatiquement au stimulus (qui garde donc IN-SIZE valeurs). Le biais est
appris comme les autres poids ; il déplace le seuil de chaque cellule et permet
par exemple d'apprendre le ET ou le OU logique. Sans biais (défaut, comme en
1999), toute cellule répond 0 au stimulus nul.
OUT-FUN est la fonction de transfert des cellules de sortie : #'binary (défaut,
perceptron déterministe) ou #'boltzmann (perceptron probabiliste « B » de 1999 ;
TEMP est alors la température et doit être > 0).
TARGET-ERROR est le taux d'erreur (entre 0 et 1) toléré en fin d'apprentissage
(nom propre au perceptron : contrairement au THRESHOLD du mlp, qui est aussi un
paramètre des fonctions de transfert, celui-ci ne sert qu'à TRAIN-PERCEPTRON),
STOP le nombre maximum d'époques par appel à TRAIN-PERCEPTRON."))

(defmethod print-object ((self perceptron) stream)
  (format stream "<perceptron ~S: ~D input~:P, ~D output~:P~:[~;, bias~]>"
          (string (name self)) (in-size self) (out-size self) (bias self))
  (values))

(defun init-perceptron-net (in-size out-size &key (range 2.0) bias)
  "Initialisation aléatoire du réseau d'un perceptron : IN-SIZE lignes de
OUT-SIZE poids tirés dans [-range/2, range/2[ (range 2.0 = [-1, 1[ comme en 1999),
plus une ligne de poids de biais si BIAS est vrai."
  (make-listarray (if bias (1+ in-size) in-size) out-size
                  #'(lambda () (- (random (float range)) (/ range 2)))))

(defmacro make-perceptron (name in out &key (range 2.0) bias)
  ;; comme make-MLP (mlp.lisp) : l'expansion est du CODE qui construit
  ;; l'instance au chargement, pas une instance littérale.
  (cond ((not (boundp name))
         `(defvar ,name
            (make-instance 'perceptron
              :name ',name
              :in-size ,in
              :out-size ,out
              :bias ,bias
              :net (init-perceptron-net ,in ,out :range ,range :bias ,bias)
              :creation-date (get-universal-time))))
        ((ann-p (symbol-value name))
         (warning-msg (format nil "~S already exists ! ~S"
                              name (type-of (symbol-value name)))))
        (t
         `(setf ,name
            (make-instance 'perceptron
              :name ',name
              :in-size ,in
              :out-size ,out
              :bias ,bias
              :net (init-perceptron-net ,in ,out :range ,range :bias ,bias)
              :creation-date (get-universal-time))))))

;;; Propagation

(defun perceptron-net-activity (net input)
  "Activité (somme pondérée) de chaque cellule de sortie de NET pour le
stimulus INPUT (liste ou vecteur)."
  (let ((activity (make-list (length (car net)) :initial-element 0)))
    (loop for row in net
          for i from 0
          for xi = (elt input i)
          unless (zerop xi)
            do (loop for w in row
                     for cell on activity
                     do (incf (car cell) (* w xi))))
    activity))

(defmethod perceptron-input ((self perceptron) input)
  "Le stimulus INPUT tel que le voit le réseau : suivi de l'entrée de biais,
toujours à 1, si le perceptron a un biais."
  (assert (= (length input) (in-size self)) ()
          "~S attend ~D entrées, stimulus de longueur ~D."
          self (in-size self) (length input))
  (if (bias self)
      (append (coerce input 'list) (list 1))
      input))

(defmethod perceptron-activity ((self perceptron) &key in)
  "Activité des cellules de sortie du perceptron pour le stimulus IN
(par défaut (input self)), avant la fonction de transfert."
  (let ((input (perceptron-input self (or in (input self))))
        (net (noise self (net-temp self))))
    (assert (= (length net) (length input)) ()
            "Le réseau de ~S a ~D lignes, ~D attendues~:[~; (biais compris)~]."
            self (length net) (length input) (bias self))
    (perceptron-net-activity net input)))

(defmethod run-perceptron ((self perceptron) &key in)
  "Sortie du perceptron pour le stimulus IN (par défaut (input self)) ;
met à jour (output self) et la renvoie sous forme de liste."
  (setf (output self)
        (funcall (out-fun self)
                 (perceptron-activity self :in in)
                 :thresh 0
                 :temp (temp self))))

(defmethod activation ((self perceptron) &key neuron)
  (declare (ignore neuron))
  (run-perceptron self :in (input self)))

;;; Apprentissage : règle de Widrow-Hoff

(defun perceptron-errors (output goal)
  "Nombre de cellules de sortie dont la valeur diffère du but."
  (count 1 (compare-vectors output goal)))

(defmethod learn ((self perceptron))
  "Un pas d'apprentissage : présente le stimulus (input self), compare la
sortie au but (goal self) et corrige chaque synapse par la règle de
Widrow-Hoff, w_ij <- w_ij + l (t_j - o_j) x_i (avec x_i = 1 pour le poids
de biais). Le nombre de sorties erronées AVANT correction est dans
(current-error self) ensuite ; renvoie SELF, comme LEARN pour mlp/rmlp/som/
rosom (voir TRAIN-PERCEPTRON plus bas pour la boucle multi-époques -- cette
methode-ci, renommee depuis LEARN-PERCEPTRON, prenait avant IN/GOAL en
mots-clefs plutot que de les lire sur SELF ; la boucle fait maintenant le
SETF elle-meme a chaque stimulus)."
  (let* ((stimulus (input self))
         (goal (goal self))
         (output (run-perceptron self :in stimulus))
         (input (perceptron-input self stimulus))
         (l (learn-fact self))
         (e (perceptron-errors output goal)))
    (assert (= (length goal) (out-size self)) ()
            "~S attend ~D sorties, but de longueur ~D."
            self (out-size self) (length goal))
    (unless (zerop e)
      (loop for row on (net self)
            for i from 0
            for xi = (elt input i)
            unless (zerop xi)
              do (setf (car row)
                       (loop for w in (car row)
                             for j from 0
                             collect (widrow-hoff w (elt goal j) (nth j output) xi l)))))
    (when (verbose self)
      (format t "~&~S : ~S <<< ~S, ~D erreur~:P" stimulus goal output e))
    (setf (output self) output
          (current-error self) e)
    (values self)))

(defmethod edges ((self perceptron))
  "Les arcs de SELF : (net self) est IN-SIZE lignes (IN-SIZE + 1 si BIAS)
de OUT-SIZE poids, (nth j (nth i net)) le poids de l'entree i vers la
sortie j -- cf. la docstring de la classe. Noeuds identifies par
(COUCHE INDICE), couche 0 = entree (le biais eventuel y est le dernier
indice, IN-SIZE), couche 1 = sortie."
  (loop for row in (net self)
        for i from 0
        append (loop for w in row
                     for j from 0
                     collect (list (list 0 i) (list 1 j) w))))

(defmethod train-perceptron ((self perceptron) stimuli goals &key verbose)
  "Apprentissage par époques de la liste de STIMULI vers la liste de GOALS,
jusqu'à ce que le taux d'erreur d'une époque (fraction des stimuli mal classés)
soit <= (target-error self), ou après (stop self) époques.
Chaque taux d'erreur est empilé dans (history-error self), le plus récent
restant lisible dans (car (history-error self)) -- (current-error self), lui,
continue de porter le nombre d'erreurs du dernier pas de LEARN (pas le taux
d'époque), comme pour mlp/rmlp/som/rosom : interrogeable à tout instant, même
en cours d'apprentissage. Renvoie SELF (le perceptron appris), pour pouvoir le
réinjecter dans une fonction -- même convention que LEARN."
  (assert (= (length stimuli) (length goals)))
  (setf (previous self) (copy-tree (net self)))
  (let ((n (length stimuli)))
    (dotimes (k (stop self))
      (let* ((wrong (loop for in in stimuli
                          for goal in goals
                          do (setf (input self) in (goal self) goal)
                          count (plusp (current-error (learn self)))))
             (rate (float (/ wrong n))))
        (incf (epoch self))
        (push rate (history-error self))
        (when verbose
          (format t "~&Epoch ~D : ~D erreur~:P (~,1F %)" (epoch self) wrong (* 100 rate)))
        (when (<= rate (target-error self))
          (setf (last-stop self) (list (epoch self) 'completed))
          (when verbose
            (format t "~&>> Fin de l'apprentissage à l'époque ~D.~%" (epoch self)))
          (return-from train-perceptron (values self)))))
    (setf (last-stop self) (list (epoch self) 'interrupted))
    (when verbose
      (format t "~&>> Apprentissage interrompu à l'époque ~D (taux d'erreur ~,1F %).~%"
              (epoch self) (* 100 (car (history-error self)))))
    (values self)))

(defmethod clear ((self perceptron))
  (setf (previous self) (net self)
        (net self) (init-perceptron-net (in-size self) (out-size self) :bias (bias self))
        (epoch self) 0
        (last-stop self) '(0 nil)
        (current-error self) 1000
        (last-error self) 1000
        (history-error self) '()
        (history self) '()
        (creation-date self) (get-universal-time)
        (modification-date self) '())
  (format t "~%Perceptron ~S cleared.~%" (name self))
  self)

;; EOF
