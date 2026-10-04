;;; code for Self-Organizing maps

(in-package :neuromuse)

;(format t "som.lisp ...~%")

(defclass som (ANN)
  ((radius
    :initform 1 :initarg :radius :accessor radius :type number)
   (neighbourhood
    :initform #'voisins :initarg :neighbourhood :accessor neighbourhood)
   (winner
    :initform 0 :initarg :winner :accessor winner :type integer)
   (distance
    ;; Fonction (son symbole) qui mesure l'ecart entre l'entree X et les
    ;; poids W d'un neurone, dans FIND-WINNER comme dans LEARN (cf.
    ;; NEURON-DISTANCE) :
    ;;   'EUCLIDIAN          -- d(x, w), par defaut : le SOM de Kohonen
    ;;                          habituel, ou le poids est un prototype dans
    ;;                          l'espace des entrees, et ou l'apprentissage (w
    ;;                          vers x) fait baisser la quantite meme qui a
    ;;                          designe le gagnant.
    ;;   'NEUROMUSE-DISTANCE -- d(x, x*w), la distance historique des versions
    ;;                          1999-2001 (cf. maths.lisp) : l'entree y est
    ;;                          comparee a l'activation du neurone, le poids y
    ;;                          est un gain plutot qu'un prototype. A combiner
    ;;                          avec ERROR-SCALING a :RAW pour rejouer les
    ;;                          anciennes versions.
    ;; Toute fonction (x w) -> nombre convient. (Le DISTANCE de chaque NEURON
    ;; est un autre slot, du meme nom : la derniere distance calculee.)
    :initform 'euclidian :initarg :distance :accessor distance :type symbol)
   (grid-distance
    ;; Fonction (son symbole) qui mesure, dans LEARN, l'ecart SUR LA GRILLE
    ;; entre un voisin et le gagnant, a partir de leurs coordonnees (cf. 2D) :
    ;; c'est elle qui fixe la forme du voisinage, la ou DISTANCE mesure la
    ;; ressemblance dans l'espace des entrees -- deux geometries differentes,
    ;; a ne pas confondre (NEUROMUSE-DISTANCE appliquee a des coordonnees de
    ;; grille dependrait de la position absolue du gagnant sur la carte).
    ;;   'EUCLIDIAN -- par defaut, comme toujours jusqu'ici : voisinage rond
    ;;                 dans la fenetre carree de VOISINS, dont les coins
    ;;                 (a radius*sqrt 2) sont donc moins corriges que les bords.
    ;;   'CHEBYSHEV -- max des ecarts par axe : distance exacte de la fenetre
    ;;                 carree de VOISINS, coins et bords traites a egalite.
    ;;   'MANHATTAN -- somme des ecarts par axe : voisinage en losange.
    ;; Toute fonction symetrique (a b) -> nombre, nulle seulement pour a = b,
    ;; convient. (Une grille torique ou hexagonale demanderait aussi de revoir
    ;; VOISINS et 2D/D2, pas seulement cette distance.)
    :initform 'euclidian :initarg :grid-distance :accessor grid-distance :type symbol)
   (topology ;; '(taille-du-net nombredimension autresdescripteurs) une fois
    ;; INIT passe (toujours le cas : INITIALIZE-INSTANCE :AFTER l'appelle) --
    ;; le premier element devient le nombre total de neurones, le second la
    ;; dimension de la grille pour 2D/D2 (VOISINS, LEARN). L'initform
    ;; ci-dessous ('euclidian 2) ne survit donc jamais telle quelle : seul son
    ;; second element (la dimension) est repris par INIT.
    :initform '(euclidian 2) :initarg :topology :accessor topology :type list)
   (temp
    :initform 0.0 :initarg :temp :accessor temp :type number)
   (error-scaling
    ;; Largeur de la gaussienne de voisinage dans LEARN (cf.
    ;; NEIGHBOURHOOD-WIDTH) :
    ;;   :normalized -- RADIUS * min(1, erreur / MAX-ERROR) : l'erreur pilote
    ;;                  toujours la largeur, mais ramenee entre 0 et 1 par la
    ;;                  plus grande erreur de gagnant deja vue (a la maniere du
    ;;                  PLSOM de Berglund et Sitte, 2006), donc en cases de la
    ;;                  grille et independante de l'echelle des donnees. A
    ;;                  erreur maximale, on retrouve exactement la formule
    ;;                  classique mise en commentaire au-dessus de GAUSSIAN-HAT
    ;;                  (maths.lisp) ; quand la carte s'ajuste, le voisinage
    ;;                  se resserre de lui-meme.
    ;;   :raw        -- l'erreur brute du neurone, comme avant : la largeur
    ;;                  est alors en unites de l'entree, pas en cases.
    :initform :normalized :initarg :error-scaling :accessor error-scaling :type keyword)
   (max-error
    ;; Plus grande erreur de gagnant vue depuis INIT (cf. ERROR-SCALING
    ;; :NORMALIZED). Ne fait que croitre : une valeur aberrante la fige haut et
    ;; resserre ensuite tout le voisinage -- la remettre a 0.0 a la main au
    ;; besoin (INIT le fait).
    :initform 0.0 :initarg :max-error :accessor max-error :type number))
  (:documentation "som"))

;; Naming an explicitly-named instance (binding SELF to NAME itself, cf.
;; MAKE-NEW-SYMBOL) is ANN's own INITIALIZE-INSTANCE :AFTER's job
;; (neuromuse-main.lisp) -- :AFTER methods for every applicable class all
;; run (least-specific first, so ANN's runs before this one), not just the
;; most specific, and redoing the SAME thing here unconditionally used to
;; make it run TWICE when NAME was given : ANN's call succeeds and binds
;; e.g. PINSON-SOM, then this one ran again, saw that name now already
;; bound, and gensymed PINSON-SOM-129 instead -- silently leaving (NAME
;; self) wrong (though the original global, still correctly bound by ANN's
;; call, kept working) for every SOM ever given an explicit :NAME. Only the
;; anonymous case still needs this method's own naming : left to ANN alone,
;; an anonymous SOM would end up called ANN-n rather than SOM-n.
(defmethod initialize-instance :after ((self som) &key name (size 16) (input 8))
  (unless name
    (let ((n (make-new-symbol 'som)))
      (setf (slot-value self 'name) n
	    (symbol-value n) self)))
  (init self :size size :input input))

(defmethod print-object ((self som) stream)
  (format stream "<SOM ~S>" (name self) ))

(defmethod init ((self som) &key (size 16) (input 8))
   ;; NB : garder (cdr (topology self)) -- la dimension de grille (2 par
   ;; defaut) et tout descripteur ulterieur -- au lieu d'ecraser toute la
   ;; liste : LEARN lit (cadr (topology self)) pour construire le
   ;; voisinage (voir plus bas), et le perdre ici le faisait echouer des
   ;; qu'on rappelait INIT avec une taille explicite (ce que fait
   ;; systematiquement tout code appelant, cf. les exemples et les tests).
   (if size
      (setf (topology self) (list* size (cdr (topology self))))
     (setf size (car (topology self))))
  (setf (net self) (list))
  (dotimes (k size (setf (net self) (nreverse (net self))))
    (push (make-instance 'neuron
			 :id (list k 0)
			 :age 0
			 :nn (name self))
	  (net self)))
  (setf (epoch self) 0)
  (dotimes (i size)
    (let ((nr (eval (nth i (net self)))))
      (dotimes (j input (setf (net nr) (nreverse (net nr))))
	(push (list (list j i)
		    (- .25 (random .5)) 0) (net nr)))))
  (setf (winner self) (random size)
	(max-error self) 0.0     ; nouveaux poids : l'echelle des erreurs repart de zero
	(input self) (coerce (make-list input :initial-element 0) 'vector)
	(output self) (make-list input :initial-element 0))
  (values self))

(defgeneric activation (self &key n)
  (:documentation "Calcul l'activation de 'self'."))

(defmethod activation ((ann som) &key (n nil))
  (let ((input (input ann))  ;; descendre la variable au niveau neuron
	(temp (temp ann))
	(net (net ann)))
    (when (not temp) (setf temp .0))
    (if n
        (setf (output (nth n net))
              (let ((nnt (nth n net)))
                (loop for k from 0 to (1- (length input))
                      collect
                      (+ (* (elt input k) (cadr (nth k (net nnt))))
                         (- (/ temp 2)
                            (if (zerop temp) 0 (random temp)))))))
	(loop for n from 0 to (1- (length net))
	      collect
	      (activation ann :n n)))))

(defgeneric update-activation (self)
  (:documentation "Met a jour l'activation de 'self', sans retourner les valeurs."))

(defmethod update-activation ((self neuron))
  (let ((nn (eval (nn self))) input temp net)
    (setf input (input nn)
	  temp (if (temp self) (temp self) (temp nn))
	  net (net self))
    (dotimes (k (length input))
      (setf (caddr (nth k net))
	    (+ (* (elt input k) (cadr (nth k net)))
	       (- (/ temp 2)
		  (if (zerop temp) 0 (random temp))))))
    (values)))

(defmethod update-activation ((self som))
  (dolist (n (net self))
    (update-activation n))
  (values))

;;*******************************************
;;********* distance entree / neurone *******
;;*******************************************

(defgeneric neuron-weights (ann n)
  (:documentation "Poids du neurone N de ANN, tels que compares a l'entree par
(DISTANCE ANN) : bruites par (TEMP ANN) comme l'etait l'activation. Range aussi
le resultat dans (OUTPUT neurone), que lisent le visualiseur (gui.lisp) et
TRACE-ACTIVATION (read-write.lisp)."))

(defmethod neuron-weights ((ann som) n)
  (let ((temp (or (temp ann) 0.0))
	(nnt (nth n (net ann))))
    (setf (output nnt)
	  (loop for synapse in (net nnt)
		for k from 0 below (length (input ann))
		collect (+ (cadr synapse)
			   (- (/ temp 2)
			      (if (zerop temp) 0 (random temp))))))))

(defun neuron-distance (ann n)
  "Distance, au sens de (DISTANCE ANN), entre (INPUT ANN) et les poids du
neurone N (cf. NEURON-WEIGHTS). Seul endroit ou FIND-WINNER et LEARN mesurent
l'ecart entre l'entree et un neurone."
  (funcall (distance ann) (input ann) (neuron-weights ann n)))

(defmethod find-winner ((ann som) &key (inf #'< ) (equality #'= ))
  (let ((win '((nil 696969)) ))
    (loop for k from 0 to (1- (length (net ann)))
          do
          (let ((dist (neuron-distance ann k)))
	    (setf (distance (nth k (net ann))) dist)
            (if (not (funcall inf dist (cadar win)))
            	(when (funcall equality dist (cadar win))
		  (setf win (append win (list (list k dist)) )) )
		(setf win (list (list k dist))))))
    ;; En cas d'egalite exacte entre plusieurs neurones (WIN a plus d'un
    ;; element), tire le gagnant au hasard parmi eux plutot que de toujours
    ;; prendre le premier trouve -- c'etait deja l'intention (l'appel a
    ;; RANDOM ci-dessous), mais son resultat n'etait jamais reinjecte dans
    ;; la valeur de retour, et en prime (CADAR (NTH W WIN)) faisait un CAR
    ;; de trop : (NTH W WIN) est deja une paire (indice distance), pas une
    ;; liste de paires, donc (CADAR ...) appliquait CAR a un entier (l'indice
    ;; du neurone) -- exactement le "N is not of type LIST" que ce genre
    ;; d'egalite provoquait, rarissime donc invisible avant des dizaines de
    ;; milliers d'appels (cf. train-pinson-som).
    (let ((chosen (if (> (length win) 1)
                       (nth (random (length win)) win)
                       (car win))))
      (list (nth (car chosen) (net ann)) (cadr chosen)))))

;;*******************************************
;;********* apprentissage *******************
;;*******************************************

(defun neighbourhood-width (ann error)
  "Largeur de la gaussienne de voisinage pour un neurone d'erreur ERROR, selon
(ERROR-SCALING ANN) -- cf. la documentation de ce slot. En :NORMALIZED, tant
que MAX-ERROR vaut 0 (rien encore d'appris), la largeur est RADIUS entier."
  (ecase (error-scaling ann)
    (:raw error)
    (:normalized
     (let ((rho (max-error ann)))
       (* (radius ann)
	  (if (plusp rho) (min 1 (/ error rho)) 1))))))

(defun neighbourhood-correction (learn width grid-distance)
  "Taux de correction (GAUSSIAN-HAT) d'un neurone a GRID-DISTANCE cases du
gagnant, pour une gaussienne de largeur WIDTH. Une largeur nulle (ou
negligeable) ne corrige que le gagnant lui-meme, au taux LEARN : avant, une
erreur nulle laissait la correction a NIL (ou divisait 0 par 0 pour le
gagnant) et faisait echouer LEARN."
  (if (< width 1e-6)
      (if (zerop grid-distance) learn 0)
      (gaussian-hat learn width grid-distance)))

(defmethod learn ((ann som))
  (let ((input (input ann))
	(n (length (net ann)))
	(winner (find-winner ann))
	(radius (radius ann))
	(learn (learn-fact ann))
	(topos (topology ann))
	coord-w
	voisins
	f h)
		;correction du gagnant
    (setf f #'2d ;(read-from-string (format nil "~Sd" (cadr topos)))
	  h #'d2 ;(read-from-string (format nil "d~S" (cadr topos)))
	  coord-w (funcall f (car (id winner)) n)
	  voisins (funcall (neighbourhood ann) coord-w radius (floor (expt n (/ 1 (cadr topos))))))
    (when (verbose ann)
      (format t "~%win : ~S ~S" winner coord-w))
    ;; l'erreur du gagnant fixe l'echelle de NEIGHBOURHOOD-WIDTH (:NORMALIZED)
    (setf (max-error ann) (max (max-error ann) (cadr winner)))
    (loop for voisin in voisins ;; winner inclu
	  do
	  (let* ((k (funcall h (car voisin) (cadr voisin) n))
		 (vn (nth k (net ann)))
		 (error (neuron-distance ann k))
		 (correction (neighbourhood-correction ;chapeau mexicain
			      learn
			      (neighbourhood-width ann error)
			      ;; distance SUR LA GRILLE (slot GRID-DISTANCE), pas
			      ;; dans l'espace des entrees (slot DISTANCE)
			      (funcall (grid-distance ann) voisin coord-w))))
            (setf (distance vn) error)
	    (dotimes (i (length input))
	      (let ((synapse (nth i (net vn))))
		(setf (cadr synapse)
		      (+ (cadr synapse) (* correction (- (elt input i) (cadr synapse)))))))))
    (setf (epoch ann) (1+ (epoch ann)))
    (values ann)))  ;; le som appris, pour pouvoir le reinjecter dans une fonction

#|
(defmethod nnsave ((self som) &optional (path "saved-som.lisp"))
  (let ((slots (structure-slot-names (type-of self))))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :append
			    :if-does-not-exist :create)
      (format t "~&(make-instance 'som")
      (loop for s in slots
	    do
	    (let ((val (funcall s self)))
	    (case val
	      (functionp
	    (format t "~%:~S '~S"
		    s
		    (funcall s self)
		    ))
      (format t ")~%")
      t))))))
|#

;;; exemple
;(defvar SOM)
;(setf SOM (make-instance 'som :name 'tata :input 45 :topology (list 100 2)))
;(init SOM :size 100 :input 45)
;(setf (topology SOM) (list 100 2))
; (inspect SOM)
; (setf (input SOM) (coerce (loop for i from 0 to 44 collect (random 1.0)) 'simple-vector))
; (activation SOM :n (car (id (car (find-winner SOM)))))
; (2d (car (id (car (find-winner SOM)))) (length (net SOM)))
;
;(setf (learn-fact SOM) .6)
;(setf (temp SOM) .0)
;(learn SOM)
