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
;;; perceptron, unfinished ?, see mlp

(defclass perceptron (ANN)
  ((name
    :initform 'perceptron
    :initarg :name
    :reader name
    :accessor name
    :type symbol)
   (in-size
    :initform 2
    :initarg :in-size
    :reader in-size
    :accessor in-size
    :type integer)
   (out-size
    :initform 1
    :initarg :out-size
    :reader out-size
    :accessor out-size
    :type integer)
   (previous
    :initform '()
    :initarg :previous
    :reader previous
    :accessor previous
    :type list)
   ))

(defmethod print-object ((self perceptron) stream)
  (format stream
          "<perceptron ~S ~D input~:P ~D output~:P>" 
          (string (name self))
          (length (net self))
          (length (car (net self))))
  (values))

(defmethod init-perceptron-net ((in-size integer) (out-size integer) &key (range .4))
  "initialisation random du reseau du perceptron."
  (let (net)
    (dotimes (i in-size net)
      (let (to-out)
	(dotimes (j out-size (push to-out net))
	  (push (- (/ range 2) (random range)) to-out))))))

(defmethod init-perceptron-net ((in-size null) (out-size null) &key (range .4))
  "initialisation random d'un net."
  (setf in-size 2 out-size 1)
  (init-perceptron-net in-size out-size :range range))

(defmacro make-perceptron (name in out &key (range .4))
  (cond ((not (boundp name))
         (let ((net
                (make-instance 'perceptron
                  :name name
                  :in-size in
                  :out-size out 
                  :net (init-perceptron-net in out :range .4)
                  :creation-date (get-universal-time))))
           `(defvar ,name ,net)
           ))
        ((ann-p (symbol-value name))
         (warning-msg (format nil
			  "~S already exists !~S"
                          name
                          (type-of (symbol-value name))
                          name)))
        (t
         (let ((net
                (make-instance 'perceptron
                  :name name
                  :in-size in
                  :out-size out 
                  :net (init-perceptron-net in out :range range)
                  :creation-date (get-time))))
           `(setf ,name ,net)))))

;; terminer perceptron
