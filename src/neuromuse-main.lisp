;;;; neuromuse version 2.2
;;;; LISP code to simulate artificial neural networks
;;;; (C) Frederic Voisin 2000-2021
;;;; <fred@fredvoisin.com>, <www.neuromuse.net>
;;;;
;;;; neuromuse.lisp
;;;; main

(in-package :neuromuse)

(defvar *lisp-version* (lisp-implementation-type))

(format t "Neuromuse v. 2.2 for ~S on ~S with cpu ~S !~%"
	(if *lisp-version* *lisp-version* 'unknown)
	(machine-instance) (machine-version))

;(format t "Loading:~%")
;(format t "neuromuse-main.lisp...~%")

(format t "Auteur :  Frederic Voisin, 2000-2021~&")

;; #+sbcl (require 'sb-thread)

(defun package-internal-symbols (package)
  (let ((rslt nil))
    (do-symbols (s package)
      (when (eq (second
		 (multiple-value-list
		  (find-symbol (symbol-name s) package)))
		:internal)
	(push s rslt)))
    rslt))

(defun make-new-symbol (name &optional content)
  (let ((sym (intern (string (if (boundp (read-from-string (string name)))
                               (gensym (format nil "~S-" name))
                               name)))))
    (setf (symbol-value sym) content)
    sym))

;; work in progress : to apply separately control values of a neural net to neuron instances
;; when the net of a ann will be a list of neurons in place of a list of list of synaptic values
;; (since a list of synaptic values is a neuron !) - future project, for instance to play with
;; inhibited neurons after learn.
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
   (net-temp
    :initform 0.0 :initarg :net-temp :accessor net-temp :type float)
   (bias
    :initform 0.0 :initarg :bias :accessor bias)
   (fact
     :initform 1.0 :initarg :fact :accessor fact)
   (learn-fact
     :initform nil :initarg :learn-fact :accessor learn-fact)
   (output
     :initform nil :initarg :output :accessor output)
   (distance
     :initform 1 :initarg :distance :accessor distance)))

(defgeneric weights (self)
  (:documentation "Poids synaptiques de SELF, sans bruit, dans l'ordre des entrees."))

(defmethod weights ((self neuron))
  ;; (net neurone) = ((source poids derniere-activation) ...)
  (mapcar #'cadr (net self)))

(defgeneric edges (net)
  (:documentation "NET vu comme un graphe pondere : liste de (SOURCE CIBLE POIDS).
Une methode par representation concrete de (net ANN) -- liste de neurones
(som/rosom), liste de matrices (mlp/rmlp/perceptron), tableau 2D
(hopfield/auto-assoc) -- puisque c'est la seule chose qui varie reellement ;
NODES s'en deduit mecaniquement (cf. plus bas), pas de methode a ecrire
classe par classe pour NODES."))

(defgeneric nodes (net)
  (:documentation "Noeuds de NET vu comme un graphe : par defaut, les
sources et cibles distinctes de (EDGES NET). Pas de methode a specialiser
par classe -- NODES se deduit toujours de EDGES, qui lui porte la
connaissance specifique a chaque representation de net."))

(defmethod nodes (net)
  (remove-duplicates
   (loop for (source target) in (edges net) append (list source target))
   :test #'equal))

(defmethod edges ((self neuron))
  "Les arcs entrants de SELF, un par synapse de (net self) : (SOURCE SELF
POIDS). SOURCE est (:input j), j l'indice de la dimension d'entree portee
par cette synapse -- le second element du marqueur stocke dans la synapse,
(j i), n'est que l'indice de SELF lui-meme (cf. INIT dans som.lisp), pas un
identifiant partage entre neurones ; on ne garde donc que j. (Un ROSOM
retague les synapses de ses neurones de contexte en (:context j) -- cf.
EDGES sur ROSOM dans rosom.lisp -- puisque ce sont deux espaces de
dimensions distincts.)"
  (mapcar (lambda (synapse) (list (list :input (first (first synapse))) self (second synapse)))
          (net self)))

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
    :initform nil :initarg :creation-date :accessor creation-date)
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
    :initform 9999 :initarg :current-error :accessor current-error :type number)
   (last-error
    :initform 9999 :initarg :last-error :accessor last-error :type number)
   (last-stop
    :initform '(0 nil) :initarg :last-stop :accessor last-stop :type list)
   (history
    :initform '() :initarg :history :accessor history)
   (temp
    :initform 0.0 :initarg :temp :accessor temp :type float)
   (net-temp
    :initform 0.0 :initarg :net-temp :accessor net-temp :type float)
   (learn-fact
    :initform 0.0 :initarg :learn-fact :accessor learn-fact :type float)
   (learn-fact-schedule
    ;; NIL (defaut) : pas de politique, LEARN-FACT reste une valeur statique,
    ;; modifiee a la main comme aujourd'hui. Sinon, une fonction d'un
    ;; argument (self) : appelee pour EFFET -- elle fait elle-meme
    ;; (setf (learn-fact self) ...) -- jamais par LEARN lui-meme, seulement
    ;; par qui decide du rythme (boucle TRAIN-*, demon UDP, cron, REPL) ;
    ;; remplacable a tout instant par (setf (learn-fact-schedule self) ...),
    ;; y compris en situ. Meme principe que RADIUS-SCHEDULE sur SOM.
    :initform nil :initarg :learn-fact-schedule :accessor learn-fact-schedule)
   (attention
    :initform '() :initarg :attention :accessor attention)
   (properties
    :initform 'nil :initarg :properties :accessor properties :type list)
   (verbose
    :initform '() :initarg :verbose :accessor verbose)))

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

(defgeneric temperature (self)
  (:documentation "Temperature of an ANN or neuron, i.e randomnes of its output (synonyme of 'temp')."))

(defmethod temperature ((self neuron))
  (temp self))

(defmethod temperature ((self ann))
  (temp self))

(defgeneric net-temperature (self)
  (:documentation "Temperature of the synatic weights of an ANN or of a neuron (synonyme : 'net-temp')."))

(defmethod net-temperature ((self neuron))
  (temp self))

(defmethod net-temperature ((self ann))
  (temp self))

(defgeneric ann-p (self)
  (:documentation "Tests if <self> is a neural net."))

(defmethod ann-p ((self t))
  nil)

(defmethod ann-p ((self ann))
  t)

(defmethod net ((self symbol))
  (net (eval self)))

(defmethod net ((self list))
  self)

(defmethod id ((self list))
	(id (car self)))

(defmethod id ((self null))
  0)

(defgeneric activation (self &key neuron)
  (:documentation "To compute activation of 'self'."))

(defun warning-msg (string &optional (port t))
  (format port (concatenate 'string "~% WARNING : " string)))

(defgeneric init (self &key size input)
  (:documentation "To initialize some object - ann, som, etc.- work in progress"))

;; STRUCTURE-SLOT-NAMES and SAVE (generic + its NEURON/LIST/T/MLP methods)
;; moved to src/read-write.lisp, with TRACE-ACTIVATION and the other
;; read/write-to-file utilities -- structure-slot-names had no other caller,
;; and SAVE's methods (bar this file's own NEURON one) already lived
;; elsewhere (src/mlp.lisp), so nothing but this comment stays here.

;**************** Threading ****************

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

;; #-(or sbcl mcl) (format t "Are threads implemented in this Lisp environnement !~%")

#|
;;;; neuromuse suite...
; ai cas ou
; (load "perceptron.lisp")
(load "maths&misc.lisp")
(load "mlp.lisp")
(load "som.lisp")
(load "rosom.lisp")
(load "udp.lisp")
(format t "Ready !~&")
|#

;EOF
