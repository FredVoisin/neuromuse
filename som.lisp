;; neuromuse version 2.0 beta 3
;; LISP code to simulate artificial neural networks

;; (C) Frederic Voisin 2000-2008
;; <fredvoisin@neuromuse.org>, <www.neuromuse.org>

;This program is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 2 of the License, or
;(at your option) any later version.

;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.

;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;The GNU Public Licence can be found in the file COPYING
;------------------------------------------------------------------
;;; code for Self-Organizing maps

(format t "Loading SOM...~&")

(defclass som (ANN)
  ((radius
    :initform 1 :initarg :radius :accessor radius :type number)
   (neighbourhood
    :initform #'voisins :initarg :neighbourhood :accessor neighbourhood)
   (winner
    :initform 0 :initarg :winner :accessor winner :type integer)
   (distance ;; distance input > memoire, pour ne pas etre recalculee
    :initform 'euclidian :initarg :distance :accessor distance :type symbol)
   (topology ;; '(espace nombredimension autresdescripteurs)
    :initform '(euclidian 2) :initarg :topology :accessor topology :type list)
   (temp
    :initform 0.0 :initarg :temp :accessor temp :type number))
  (:documentation "som"))

(defmethod initialize-instance :after ((self som) &key name)
  (let ((name-of-som (if name 
                          (make-new-symbol name)
                          (make-new-symbol 'som))))
     (setf (slot-value self 'name) name-of-som
	   (symbol-value name-of-som) self)
     (init self) ))

(defmethod print-object ((self som) stream)
  (format stream "<SOM ~S>" (name self) ))

(defmethod init ((self som) &key (size 16) (input 8))
   (if size
      (setf (topology self) (list size))
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
  (:documentation "Cacul l'activation de 'self'."))

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
    (if (> (length win) 1)
    	(let ((w (random (length win))))
	  (list (nth w (net ann))) (cadar (nth w win))))
    (list (nth (caar win) (net ann)) (cadar win))))

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
	f g h)
		;correction du gagnant
    (setf f #'2d ;(read-from-string (format nil "~Sd" (cadr topos)))
	  h #'d2 ;(read-from-string (format nil "d~S" (cadr topos)))
	  coord-w (funcall f (car (id winner)) n)
	  voisins (funcall (neighbourhood ann) coord-w radius (floor (expt n (/ 1 (cadr topos)))))
	  g (car topos))
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
    (values)))

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




