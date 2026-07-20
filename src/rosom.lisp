;; neuromuse version 2.0 beta 2
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

(in-package :neuromuse)

(format t "ROSOM...~&")
;; portage de MCL a opemMCL (SBCL a suivre)

(defclass rosom (SOM)
  ((input-context :initform nil :initarg :input-context :accessor input-context :type list)
   (content :initform nil :initarg :content :accessor content :type list)
   (context :initform nil :initarg :context :accessor context :type list))
   (:documentation "rosom: recurrent oscillatory self-organising map")
   )

(defmethod initialize-instance :after ((self rosom) &key name)
  (let ((name-of-rosom (if name 
                          (make-new-symbol name)
                          (make-new-symbol 'rosom))))
     (setf (slot-value self 'name) name-of-rosom
	      (symbol-value name-of-rosom) self)
	name-of-rosom))

(defmethod print-object ((self rosom) stream)
  (format stream "<ROSOM ~S>" (name self) ))

(defmethod init ((self rosom) &key (input 8) (size 16))
   (if input (setf (car (topology self)) input)
     (setf input (car (topology self))))
   (if size (setf (cadr (topology self)) size)
     (setf size (cadr (topology self))))
   (setf (net self) (list 'nil 'nil))
   (dotimes (k size (setf (car (net self)) (nreverse (car (net self))))) ;; neurones som
     (push (make-instance 'neuron
			  :id (list k 0)
			  :age 0
			  :nn (name self))
	   (car (net self))))
   (dotimes (i size)
     (let ((nr (eval (nth i (car (net self))))))
       (dotimes (j input (setf (net nr) (nreverse (net nr)))) 
	 (push (list (list j i) (- .1 (random .2)) 0)
	       (net nr)))))
   (dotimes (k size (setf (cadr (net self)) (nreverse (cadr (net self))))) ;; neurones contexte
     (push (make-instance 'neuron
			  :id (list k 0)
			  :age 0
			  :nn (name self))
	   (cadr (net self))))
   (dotimes (i size)
     (let ((nr (eval (nth i (cadr (net self))))))
       (dotimes (j size (setf (net nr) (nreverse (net nr))))
	 (push (list (list j i) (- .1 (random .2)) 0)
	       (net nr)))))
   (setf (winner self) (random size)
	 (input self) (coerce (make-list input :initial-element 0) 'vector)
	 (output self) (make-list input :initial-element 0)
	 (input-context self) (loop for i from 0 to (1- size) collect (list 1 1 1))
	 (epoch self) 0)
  (values self))

(defun conteur (winner rosom)
  (let ((size (length (car (net rosom)))))
    (setf (input-context rosom)
	  (loop for i from 0 to (1- size)
		collect
		(let ((n (nth i (input-context rosom))))
		  (if (= winner i)
		      (list 1 (cadr n) (/ 1 (car n)))
		    (list (1+ (car n)) (cadr n)
			  (funcall #'min 1 (/ (1+ (car n)) (cadr n))))))))) )  ;;; A, Z, A / Z

; (setq titi (make-instance 'rosom :name 'titi :topology (list 5 36)))
; (init titi)
; (net titi)
; (mapcar #'length (net titi))
; (input-context titi)
; (conteur 13 titi)

(defmethod activation ((self rosom) &key (n nil))
  (let ((input (input self))  ;; descendre la variable au niveau neuron
	(temp (temp self))
	(net (car (net self))))  ;; attention on prend seulement som du rosom
    (if n
	(let ((neuron (nth n net)))
	  (loop for k from 0 to (1- (car (topology self)))
		collect
		(+ (* (elt input k) (cadr (nth k (net neuron))))
		   (- (/ temp 2)
		      (if (zerop temp) 0 (random temp))))))
	(loop for o from 0 to (1- (length net))
	      collect
	      (activation self :n o)))))

;(cadr (nth 0 (net (nth 0 (car (net titi))))))
;(elt (input titi) 0)
;(temp titi)
; (setf (input titi) #(.1 .7 .2 .6 .5))
; (activation titi :n 8)
; (activation titi)


(defmethod find-winner ((self rosom) &key (equality #'=) (inf #'<))
  ;; loop bound is the neuron count (CADR of TOPOLOGY, as everywhere else
  ;; in this file), not the input size (CAR) -- using CAR here walked past
  ;; the end of the neuron list into NIL neurons.
  (let ((win '((-1 696969))) (som (car (net self))) (vector (input self)))
    (loop for k from 0 to (1- (cadr (topology self)))
          do
          (let ((dist (euclidian vector (activation self :n k))))
            (if (funcall inf dist (cadar win))
              (setf win (list (list k dist)))
              (when (or (funcall equality dist (cadar win))
                    (funcall inf dist (cadar win)))
                (setf win (append win (list (list k dist)) ))
                ))))
    (if (> (length win) 1)
      (nth (random (length win)) win)
      (car win))))

; (find-winner titi)

(defun contentn (net n)
  ;; on peut mettre une temperature la...
  (coerce (mapcar #'cadr (net (nth n net)))
	  'vector))

; (mapcar #'cadr (net (nth 0 (car (net titi)))))

(defun rosom-learn (input rosom radius learn entrainement-rate temp-som temp-rosom &key (content-on 1) (context-on 1) (verbose t))
  (let ((netrosom (net rosom))
	(n (cadr (topology rosom))) ;(n (array-dimension (car netrosom) 0))
	distances
	w-som  ;; W
	context  ;; S
	w-rosom ;; V
	(winner '(-1 0))
	content-response
	context-response
	response
					;(winner ;(winner input netrosom))
					;(coord-w (2d (car winner) n))
	voisins
	flat-voisins ;; VOISINS's 2D coordinate pairs, converted to flat neuron indices via D2
	)
    (setf w-som (car netrosom)
	  w-rosom (cadr netrosom)
	  context (coerce
		   (loop for c in (input-context rosom) collect (third c))
		   'vector))
    (loop for n from 0 to (1- n)
          do
          (setf  
           content-response (- 1 (+ (euclidian (contentn w-som n) input )
                                          (- (/ temp-som 2) (rand temp-som))))
           context-response (- 1 (+ (euclidian (contentn w-rosom n) context )
                                          (- (/ temp-rosom 2) (rand temp-rosom))))
           response (* (expt content-response content-on)
		       (expt context-response context-on)
		       (- 1 (/ 1 (car (elt (input-context rosom) n))))))
                  (when (> response (cadr winner))
                      (setf winner (list n response))))
       (when (minusp (car winner)) (setf winner (list (rand n) (cadr winner))))
       (conteur (car winner) rosom)  ;;<---
       ;;; voila, on a fixe ici les frequences et phases du rosom
       ;; VOISINS wants the grid width (not the neuron count N) as its third
       ;; argument -- matching how SOM's own LEARN method (src/som.lisp)
       ;; calls it -- and returns 2D coordinate pairs, which must be
       ;; converted back to flat neuron indices via D2 before use (again,
       ;; matching SOM's LEARN). W-SOM/W-ROSOM are lists of NEURON instances
       ;; (per INIT/CONTENTN), not arrays, so weight updates walk each
       ;; neuron's own synapse list instead of using AREF.
       (setf voisins (voisins (2d (car winner) n) radius (floor (sqrt n)))
             flat-voisins (mapcar #'(lambda (v) (d2 (car v) (cadr v) n)) voisins))
       (when verbose (format t "~%win : ~S voisins : ~S" winner voisins))
       ;;correction du gagnant du SOM
       (when (not (zerop learn))
	 (let ((wn (nth (car winner) w-som)))
	   (loop for i from 0 to (1- (length input))
		 do
		 (let ((synapse (nth i (net wn))))
		   (setf (cadr synapse)
			 (+ (cadr synapse) (* learn (- (elt input i) (cadr synapse)))))))
	   ;;correction des neurones voisins du SOM
	   (loop for flat in flat-voisins for p from 0 to (1- (length flat-voisins))
		 do
		 (setf distances (append distances
					 (list (euclidian (2d (car winner) n)
							   (2d flat n)))))
		 (let* ((correction
			 (* learn
			    (exp (/ (- (expt (nth p distances) 2))
				    (expt (* 2 radius) 2)))))  ;;;  changer ici eventuellement dynamiquement en fonction de l erreur
			(nn (nth flat w-som)))
		   (loop for i from 0 to (1- (length input))
			 do
			 (let ((synapse (nth i (net nn))))
			   (setf (cadr synapse)
				 (+ (cadr synapse) (* correction (- (elt input i) (cadr synapse)))))))))
	   ;;correction du gagnant du ROSOM
	   (let ((rn (nth (car winner) w-rosom)))
	     (loop for i from 0 to (1- n)
		   do
		   (let ((synapse (nth i (net rn))))
		     (setf (cadr synapse)
			   (+ (cadr synapse) (* learn (- (elt context i) (cadr synapse))))))))
	   ;;correction des neurones voisins du ROSOM
	   (loop for flat in flat-voisins for p from 0 to (1- (length flat-voisins))
		 do
		 (let* ((correction
			 (* learn
			    (exp (/ (- (expt (nth p distances) 2))
				    (expt (* 2 radius) 2)))))  ;;;  changer ici eventuellement dynamiquement en fonction de l erreur
			(nn (nth flat w-rosom)))
		   (loop for i from 0 to (1- n)
			 do
			 (let ((synapse (nth i (net nn))))
			   (setf (cadr synapse)
				 (+ (cadr synapse) (* correction (- (elt context i) (cadr synapse)))))))))))
         ;; jamais deux sans trois : maintenant, on tend a synchroniser les gagnants...
         ;;SYNCHRO du ROSOM
       (when (not (zerop entrainement-rate))
	 (loop for i from 0 to (1- n)
	       do
	       (setf (cadr (nth i (input-context rosom)))
		     (+ (cadr (nth i (input-context rosom)))
			entrainement-rate)))
	 ;;SYNCHRO des neurones voisins du ROSOM
	 (loop for flat in flat-voisins for p from 0 to (1- (length flat-voisins))
	       do
	       (let ((correction
		      (* entrainement-rate
			 (exp (/ (- (expt (nth p distances) 2))
				 (expt (* 2 radius) 2))))))  ;;;  changer ici eventuellement dynamiquement en fonction de l erreur
		 (setf (cadr (nth flat (input-context rosom)))
		       (+ (cadr (nth flat (input-context rosom)))
			  (* correction (- (cadr (nth (car winner) (input-context rosom))) (cadr (nth flat (input-context rosom))))))))))
       ;; voila
       (setf (epoch rosom) (1+ (epoch rosom)))
       (when verbose (format t "~%epoch : ~S~%"  (epoch rosom)))
       (let* ((wn (nth (car winner) w-som))
	      (winner-rep  (loop for i from 0 to (1- (length input))
				 collect
				 (cadr (nth i (net wn)))))
	      (max (apply #'max winner-rep)))
	 (format t "~2D : ~S~%"
		 (car winner)
		 (mapcar #'(lambda (x) (if (< x max) 0 1)) winner-rep)))
       ))
