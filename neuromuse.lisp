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
;; neuromuse, main

(in-package :cl-user)

(defvar *lisp-version* (lisp-implementation-type))
;(setf neuromuse-dir "/home/fred/lisp/neuro/neuromuse-lisp/neuromuse/")
#+sbcl (require 'sb-bsd-sockets)
;; #+sbcl (require 'sb-thread)
;; #+(or sbcl cmucl) (load (concatenate 'string neuromuse-dir "bordeaux-mp/bordeaux-mp.lisp") ) ;; obsolete									    
;; #+sbcl  (load (concatenate 'string neuromuse-dir "bordeaux-mp/bordeaux-mp.lisp") )

;; (setq *gc-verbose* nil)

(format t "Loading neuromuse v.2b2 for ~S on ~S with cpu ~S...~%"
	(if *lisp-version* *lisp-version* 'unknown)
	(machine-instance) (machine-version))

(format t "Auteur :  Frederic Voisin, Paris - Nice, 2000 - 2006~&")

#+mcl (defvar *current-directory* (current-directory))

(defun get-time ()
  (multiple-value-bind (second minute hour date month year
                               day-of-week daylight-saving-time-p time-zone)
                       (get-decoded-time)
    (declare (ignore day-of-week daylight-saving-time-p))
    (list year month date hour minute second time-zone)))

(defun print-time (&optional (stream t))
  (let ((time (get-time)))
    (format stream "~& -- ~S ~S ~S, ~S h ~S mn --~&~&"
	    (nth 2 time) (nth 1 time) (car time) (nth 3 time) (nth 4 time))))

(print-time)

(defun make-new-symbol (name &optional content)
  (let ((sym (intern (string (if (boundp (read-from-string (string name)))
                               (gensym (format nil "~S-" name))
                               name)))))
    (setf (symbol-value sym) content)
    sym))

;;regrouper la les arguments des fonctions learn, backpro...
(defclass neuron ()
  ((name
    :initform nil :initarg :name :accessor name)
   (id
    :initform (list 0 0) :initarg :id :accessor id)
   (nn
    :initform nil :initarg :nn :accessor nn)
   (age
    :initform 0 :initarg :age :accessor age)
   (net
    :initform '() :initarg :net :accessor net) ;;((neuron weigth lastactiv)...)
   (fun
    :initform '() :initarg :fun :accessor fun) ;;(function &rest args)
   (slope
     :initform 1.0 :initarg :slope :accessor slope)  ;; = premier arg de fun si nec
   (temp
     :initform 0.0 :initarg :temp :accessor temp)
   (bias
    :initform 0.0 :initarg :bias :accessor bias)
   (fact
     :initform 1.0 :initarg :fact :accessor fact)
   (learn-fact
     :initform nil :initarg :learn-fact :accessor learn-fact)
   (output
     :initform nil :initarg :output :accessor output)
   (distance
     :initform 1 :initarg :distance :accessor distance)
    ))

(defmethod initialize-instance :after ((self neuron) &key name)
  (let ((neuron (if name 
		    (make-new-symbol name)
		    (make-new-symbol 'n))))
    (setf (slot-value self 'name) neuron
          (symbol-value neuron) self)
    neuron))

(defmethod print-object ((self neuron) stream)
  (format stream "neuron <~S>" (name self) )
  (values))

(defclass ANN ()
  ((name
    :initform 'ann :initarg :name :accessor name :type symbol)
   (id
    :initform 'nil :initarg :id :accessor id :type list)
   (superdaemon
   :initform '() :initarg :superdaemon :accessor superdaemon)
   (daemons
    :initform '() :initarg :daemons :accessor daemons :type list)
   (udplist
    :initform '() :initarg :udplist :accessor udplist :type list) ;; ((port (slot host-list))...)
   (iplist
    :initform '() :initarg :iplist :accessor iplist :type list)
   (net
    :initform '() :initarg :net :accessor net :type list)
   (creation-date
    :initform nil :initarg :creation-date :accessor creation-date :type number)
   (modification-date
    :initform '() :initarg :modification-date :accessor modification-date :type list)
   (epoch
    :initform 0 :initarg :epoch :accessor epoch)
   (input
    :initform nil :initarg :input :accessor input)
   (output
    :initform nil :initarg :output :accessor output)
   (topology
    :initform nil :initarg :topology :accessor topology :type list)
   (latence
    :initform nil :initarg :latence :accessor latence)
   (history-error
    :initform  '() :initarg :history-error :accessor history-error :type list)
   (current-error
    :initform 1000 :initarg :current-error :accessor current-error :type number)
   (last-error
    :initform 1000 :initarg :last-error :accessor last-error :type number)
   (last-stop
    :initform '(0 nil) :initarg :last-stop :accessor last-stop :type list)
   (history
    :initform '() :initarg :history :accessor history)
   (temp
    :initform 0.0 :initarg :temp :accessor temp :type float)
   (learn-fact
    :initform 0.0 :initarg :learn-fact :accessor learn-fact :type float)
   (attention
    :initform '() :initarg :attention :accessor attention)
   (properties
    :initform 'nil :initarg :properties :accessor properties :type list)
   (verbose
    :initform '() :initarg :verbose :accessor verbose)
   ))

;; topology = (size &rest space-args)

(defmethod initialize-instance :after ((self ann) &key name)
  (let ((ann (if name 
		 (make-new-symbol name)
		 (make-new-symbol 'ann))))
    (setf (slot-value self 'name) ann
          (symbol-value ann) self)
    ann))

(defmethod print-object ((self ann) stream)
  (format stream
          "<~S>" (name self) )
  (values))

(defgeneric ann-p (self)
  (:documentation "Test si <self> est un reseau de neurones"))

(defmethod ann-p ((self t))
  nil)

(defmethod ann-p ((self ann))
  t)

(defgeneric net (self)
  (:documentation "Donne la partie constituant le reseau (ou coeur) de l'objet,
 neurones et/ou leurs connections elon l'objet auquel s'applique la fonction 'net'."))

(defmethod net ((self symbol))
  (net (eval self)))

(defmethod net ((self list))
  self)

(defgeneric id (self)
  (:documentation "Donne le numero d'identification de l'objet 'self'.
Dans le cas d'une liste, l'ID est celui du premier element de la liste."))

(defmethod id ((self list))
	(id (car self)))

(defmethod id ((self null))
  0)

(defun warning-msg (string &optional (port t))
  (format port (concatenate 'string "~% WARNING : " string)))

(defun structure-slot-names (s-name)
  "Given a STRUCTURE-NAME, returns a list of the slots in the
  structure."
  #+allegro (class-slot-names s-name)
  #+lispworks (structure:structure-class-slot-names
	       (find-class s-name))
  #+sbcl (mapcar #'sb-pcl::slot-definition-name
		 (sb-pcl:class-slots
		  (find-class s-name))) ;;sbcl 0.9.6.55, fv
  #+cmu (mapcar
	 #'pcl::slot-definition-name
	 (pcl:class-slots (pcl:find-class s-name)))
  #+scl
  (loop as
	s across
	(kernel::layout-slots
	 (slot-value (find-class s-name) 'clos::layout))
	collecting s)
  #+mcl (let* ((sd (gethash s-name ccl::%defstructs%))
               (slots (if sd (ccl::sd-slots sd))))
          (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #-(or allegro lispworks cmu sbcl scl mcl)
  (error "structure-slot-names is not defined for this lisp dialect,
 some features won't work (as save...)")  )

(defgeneric init (self &key size input)
  (:documentation "Initialise un objet - ann, som, agent...- work in progress"))

(defgeneric nnsave (self &optional path)
  (:documentation "Sauvegarde l'instance de l'objet 'self' dans un fichier 'path'."))

(defmethod nnsave ((self ann) &optional (path "ann.lisp"))
  (declare (ignore path))
  (let ((slots (structure-slot-names (type-of self))))
    (dolist (slot slots)
      (print slot))))
;;do (save (eval `(,slot self)) path))))

(defmethod nnsave ((self neuron) &optional (path "ann.lisp"))
  (let ((slots (structure-slot-names (type-of self))))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :append
			    :if-does-not-exist :create)
      (format stream "~&(make-instance 'neuron")
      (loop for s in slots
	    do
	    (format stream " :~S \'~S" s (funcall s self)))
      (format stream ")~%"))))

(defmethod nnsave ((self number) &optional (path "ann.lisp"))
   (with-open-file (stream path
			    :direction :output
			    :if-exists :append
			    :if-does-not-exist :create)
     (format stream "~S" self)))

(defmethod nnsave ((self list) &optional (path "ann.lisp"))
  (dolist (elt self)
    (nnsave elt path)))

(defmethod nnsave ((self t) &optional (path "ann.lisp"))
  (dolist (elt self)
    (nnsave elt path)))

;**************** Threads stuff ****************

#+mcl (defun mk-process (name process &key)
	(when (not (stringp name)) (string name))
	(let ((proc (make-process name  :priority -7)))
	  (process-preset proc process)
	  (process-enable proc)))

#+mcl (defun suspend-process (process)
	(process-suspend process))

#+mcl (defun resume-process (process)
	(process-resume process))

#+mcl (defun kill-process (process)
	(process-kill process))

#+mcl (defun list-processes ()
	(all-processes))

#+mcl (format t "Warning ! Processes *ticks-per-second* = ~S"  *TICKS-PER-SECOND*)

#+sbcl (defun mk-process (name process &key)
	 (sb-thread:make-thread process :name name) )



;; #-(or sbcl mcl) (format t "Threads not yet implemented in this Lisp environnement !~%")

;;;; neuromuse suite...
;(load "maths&misc.lisp")
;(load "mlp.lisp")
;(load "som.lisp") 
;(load "udp.lisp")
;(load "rosom.lisp)
;(format t "Ready !~&")

;;;; for testing ****************

#|
(listenudp 42000 128) ;; attend et imprime un (seul) paquet...

(sendudp "udp_from_lisp"  "127.0.0.1" 42000)

|#

;EOF
