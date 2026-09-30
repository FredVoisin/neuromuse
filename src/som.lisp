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
   (distance ;; distance input > memoire, pour ne pas etre recalculee
    :initform 'euclidian :initarg :distance :accessor distance :type symbol)
   (topology ;; '(taille-du-net nombredimension autresdescripteurs) une fois
    ;; INIT passe (toujours le cas : INITIALIZE-INSTANCE :AFTER l'appelle) --
    ;; le premier element devient le nombre total de neurones, le second la
    ;; dimension de la grille pour 2D/D2 (VOISINS, LEARN). L'initform
    ;; ci-dessous ('euclidian 2) ne survit donc jamais telle quelle : seul son
    ;; second element (la dimension) est repris par INIT.
    :initform '(euclidian 2) :initarg :topology :accessor topology :type list)
   (temp
    :initform 0.0 :initarg :temp :accessor temp :type number))
  (:documentation "som"))

(defmethod initialize-instance :after ((self som) &key name (size 16) (input 8))
  (let ((name-of-som (if name
                          (make-new-symbol name)
                          (make-new-symbol 'som))))
     (setf (slot-value self 'name) name-of-som
	   (symbol-value name-of-som) self)
     (init self :size size :input input) ))

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

(defmethod find-winner ((ann som) &key (inf #'< ) (equality #'= ))
  (let ((vector (input ann))
  	(win '((nil 696969)) ))
    (loop for k from 0 to (1- (length (net ann)))
          do
          (let ((dist (funcall (distance ann) vector (activation ann :n k))))
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
    (loop for voisin in voisins ;; winner inclu
	  do
	  (let* ((k (funcall h (car voisin) (cadr voisin) n))
		 (vn (nth k (net ann)))
		 (error (funcall (distance ann) input (activation ann :n k)))
		 correction)
            (setf (distance vn) error)
            (when (not (zerop error))
              (setf correction (gaussian-hat learn error (funcall (distance ann) voisin coord-w))))
 ;chapeau mexicain
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
