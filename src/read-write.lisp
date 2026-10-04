;;;; read-write.lisp - Lire et ecrire un ANN dans un fichier : SAVE
;;;; serialise l'etat (quasi-)complet d'un neuron/mlp en une forme Lisp
;;;; rechargeable ; TRACE-ACTIVATION, plus bas, journalise juste son
;;;; activation au fil d'une boucle d'apprentissage ou d'execution, comme une
;;;; sequence de formes Lisp, une par appel.

(in-package :neuromuse)

;;; ------------------------------------------------------------------
;;; SAVE : serialisation d'un neuron/mlp vers un fichier rechargeable
;;; ------------------------------------------------------------------

(defun structure-slot-names (s-name)
  "Given a class name such as a neural net's, returns the list of the slots for the class."
  #+sbcl (mapcar #'sb-mop:slot-definition-name
		 (sb-mop:class-slots
		  (find-class s-name)))
  #-sbcl
  (error "structure-slot-names is not defined for this lisp dialect,
 some features won't work (as save...)"))

(defgeneric save (self &optional path)
  (:documentation "Save neural net <self> to <path>."))

(defmethod save ((self neuron) &optional (path "ann.lisp"))
  (let ((slots (structure-slot-names (type-of self))))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :append
			    :if-does-not-exist :create)
      (format stream "(in-package :neuromuse)")
      (format stream "~&(make-instance 'neuron")
      (loop for s in slots
	    do
	    (format stream " :~S \'~S" s (funcall s self)))
      (format stream ")~%"))))

(defmethod save ((self list) &optional (path "ann.lisp"))
  (with-open-file (stream path
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :create)
    (if (atom (car self))
	(format stream " ~S~&" self)
	(save self path)))
  (values))

(defmethod save ((self t) &optional (path "ann.lisp"))
  (declare (ignore path))
  (format t "~&No method for saving ~S !~&" self))

(defmethod save ((self mlp) &optional path)
  (when (not path) (setf path (format nil "~S.lisp" (name self))))
  (let ((slots (structure-slot-names (type-of self))))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "(in-package :neuromuse)")
      (format stream "~&(make-instance 'mlp")
      (loop for s in slots
	 do
	   (let ((slot-value (funcall s self)))
	     (if (listp slot-value)
		 (format stream " :~S '~S~&" s slot-value)
		 (format stream " :~S ~S~&" s slot-value))))
      (format stream ")~%")))
  (format t "~& MLP ~S saved to file ~S !" (name self) path)
  (values))

(defmethod save ((self som) &optional path)
  "Serialise l'etat  SELF (pas tous ses slots) dans PATH (par
defaut ~S.lisp), sous une forme rechargeable avec LOAD. Contrairement a SAVE
pour un mlp, ne dumpe pas bêtement toutes les slots via
STRUCTURE-SLOT-NAMES : (net self) est une liste d'instances NEURON, pas de
nombres, et (neighbourhood self) une fonction (#'VOISINS par defaut) -- ni
l'une ni l'autre ne se relit avec ~S. A la place, un unique MAKE-INSTANCE
reconstruit la grille a la bonne taille (declenchant INIT, donc de nouveaux
NEURON correctement cables -- NN, ID -- comme a la construction d'origine),
puis les reglages vraiment utiles (TOPOLOGY, RADIUS, LEARN-FACT, TEMP,
NET-TEMP, DISTANCE, GRID-DISTANCE, EPOCH, ERROR-SCALING, MAX-ERROR) et, pour chaque neurone, son NET -- les poids
synaptiques appris, la seule partie de l'etat d'un neurone qui compte pour
l'usage du SOM (AGE, OUTPUT, DISTANCE d'un neurone restent a leurs valeurs
par defaut ; NEIGHBOURHOOD aussi, laisse a #'VOISINS -- le seul jamais
utilise dans ce depot). Tout est ecrit a l'interieur d'un seul LET, pas
plusieurs formes qui referenceraient (NAME SELF) par symbole : si ce nom est
deja lie au moment du rechargement, INITIALIZE-INSTANCE en choisit un autre
(cf. MAKE-NEW-SYMBOL), et des formes separees rateraient alors l'instance
reellement creee -- LET lie IT a ce que MAKE-INSTANCE renvoie vraiment, et
tout le reste opere sur IT."
  (when (not path) (setf path (format nil "~S.lisp" (name self))))
  (with-open-file (stream path
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "(in-package :neuromuse)~%")
    (format stream "(let ((it (make-instance 'som :name '~S :size ~D :input ~D)))~%"
	    (name self) (length (net self)) (length (input self)))
    (format stream "  (setf (topology it) '~S (radius it) ~S (learn-fact it) ~S~%        (temp it) ~S (net-temp it) ~S (distance it) '~S (grid-distance it) '~S (epoch it) ~D~%        (error-scaling it) ~S (max-error it) ~S)~%"
	    (topology self) (radius self) (learn-fact self)
	    (temp self) (net-temp self) (distance self) (grid-distance self) (epoch self)
	    (error-scaling self) (max-error self))
    (dolist (n (net self))
      (format stream "  (setf (net (nth ~D (net it))) '~S)~%" (car (id n)) (net n)))
    (format stream "  it)~%"))
  (format t "~& SOM ~S saved to file ~S !" (name self) path)
  (values))

(defmethod save ((self rosom) &optional path)
  (declare (ignore path))
  (error "SAVE ne gere pas encore ROSOM : (net rosom) est (neurones-contenu
neurones-contexte), pas une liste plate de neurones comme pour SOM. A faire."))

;;; ------------------------------------------------------------------
;;; TRACE-ACTIVATION : journal d'activation au fil d'une boucle
;;; ------------------------------------------------------------------
;;;
;;; Peut être appelée à chaque pas d'une boucle d'apprentissage ou
;;; d'execution sans rien changer par ailleurs -- comme GUI-SNAPSHOT dans
;;; src/gui.lisp.

(defgeneric activation-state (ann)
  (:documentation "Etat d'activation de ANN a tracer par TRACE-ACTIVATION :
la couche de sortie, (OUTPUT ANN), pour un mlp/rmlp/perceptron -- ou toute
autre classe qui tient son activation courante dans ce meme emplacement,
d'ou la methode par defaut sur ANN plutot qu'une par classe. Pour un som,
une liste par case de la grille : (OUTPUT neuron) de chaque neurone de
(NET ann), dans cet ordre -- c'est-a-dire l'ordre de (ID neuron), la
position sur la grille (cf. 2D, src/maths.lisp)."))

(defmethod activation-state ((ann ann))
  (output ann))

(defmethod activation-state ((ann som))
  (mapcar #'output (net ann)))

(defmethod activation-state ((ann rosom))
  (error "ACTIVATION-STATE ne gere pas encore ROSOM."))

(defun trace-activation (ann &optional (path "activation-trace.lisp"))
  "Ajoute a la suite de PATH (mode APPEND ; un fichier de formes Lisp, une
par ligne, cree si besoin) l'etat d'activation courant de ANN (une instance,
ou le symbole qui la nomme) -- cf. ACTIVATION-STATE pour ce qui est
effectivement ecrit selon sa classe. Pensee pour s'inserer dans n'importe
quelle boucle d'apprentissage ou d'execution sans rien y changer par
ailleurs, par exemple :

  (dolist (frame chant)
    (setf (input som) (coerce frame 'vector))
    (find-winner som)               ; met a jour (output neuron) de chaque case
    (trace-activation som \"trace.lisp\"))

Chaque appel ajoute UN etat, sur UNE ligne (*PRINT-PRETTY* NIL le temps de
l'ecrire : sinon le pretty-printer de ~S replie les etats les plus larges --
144 cases pour un som -- sur plusieurs lignes, ce qui rendrait le fichier
plus penible a parcourir/grep ligne par ligne sans rien changer a ce qu'on
relit avec READ). La sequence est l'ordre des appels dans le fichier, relue
par READ-ACTIVATION-TRACE. Renvoie ANN, comme BACKPROPAGATE, LEARN (som),
ROSOM-LEARN et TRAIN-PERCEPTRON -- pour pouvoir l'imbriquer directement, par
exemple (trace-activation (learn som))."
  (let ((ann (if (symbolp ann) (symbol-value ann) ann)))
    (with-open-file (stream path :direction :output
                                  :if-exists :append :if-does-not-exist :create)
      (let ((*print-pretty* nil))
        (format stream "~S~%" (activation-state ann))))
    (values ann)))
 
(defun read-activation-trace (path)
  "Relit la sequence d'etats ecrite par TRACE-ACTIVATION dans PATH : une
liste de formes Lisp, dans l'ordre d'ecriture (la plus ancienne en tete)."
  (with-open-file (stream path :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))


(defun trace-output (ann &optional (path "output-trace.lisp"))
  "Ajoute a la suite de PATH l'output courant de ANN (une instance,
ou le symbole qui la nomme) - S'insère dans n'importe
quelle boucle sans rien changer, par exemple :

  (dolist (frame chant)
    (setf (input som) (coerce frame 'vector))
    (trace-output som \"trace.lisp\"))"
  (let ((ann (if (symbolp ann) (symbol-value ann) ann)))
    (with-open-file (stream path :direction :output
                                  :if-exists :append :if-does-not-exist :create)
      (let ((*print-pretty* nil))
        (format stream "~S~%"  (activation ann :n (car (id (car (find-winner ann))))))))
    (values ann)))


; eof
