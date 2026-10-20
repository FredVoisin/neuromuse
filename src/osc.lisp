;; OSC in lisp
;;
;; Open Sound Control (OSC 1.0), meme structure que udp.lisp :
;; un interrupteur (*osc* / osc), envoi et reception (send-osc, receive-osc),
;; et les trois serveurs d'un reseau (osc-input-server, osc-control-server,
;; osc-output-server). Codage et decodage des paquets ecrits ici, sans
;; bibliotheque externe : utilisable tel quel avec Pure Data
;; ([oscparse], [oscformat]), Max ou SuperCollider.
;;
;; Un message est une liste (adresse arg1 arg2 ...), par exemple
;;   ("/neuromuse/note" 60 100)
;; Types : entier -> i (int32), flottant -> f (float32), chaine -> s ;
;; en reception, aussi T, F, N (vrai, faux, nil). Pas de bundles.
;;
;;   (send-osc '("/neuromuse/note" 60 100) "127.0.0.1" 9001)
;;   (receive-osc 9002)                  ; affiche ce qui arrive, (osc) arrete

(in-package :neuromuse)

;(format t "Loading OSC...~%")

#+sbcl (require :sb-bsd-sockets)

;; comme *udp* : interrupteur des boucles de reception (receive-osc)
(defvar *osc* t)

(defun osc () (if *osc* (setf *osc* nil) (setf *osc* t)))

;;; Codage / decodage des paquets (portable, sans sockets)

(defun osc-string-bytes (string)
  "STRING en octets, suivi de 1 a 4 octets nuls (longueur multiple de 4)."
  (let ((bytes (map 'list #'char-code string)))
    (append bytes (make-list (- 4 (mod (length bytes) 4)) :initial-element 0))))

(defun osc-int32-bytes (n)
  "Entier N en 4 octets, big-endian (complement a deux)."
  (loop for shift from 24 downto 0 by 8
        collect (ldb (byte 8 shift) n)))

(defun float32-bits (x)
  "Bits IEEE 754 simple precision de X (zero pour les tres petits nombres
denormalises)."
  (let ((x (float x 1.0)))
    (if (zerop x)
        0
        (multiple-value-bind (mantissa exponent sign) (integer-decode-float x)
          (let ((biased (+ exponent 23 127)))
            (if (<= biased 0)
                0
                (logior (if (minusp sign) #x80000000 0)
                        (ash (min biased 255) 23)
                        (logand mantissa #x7fffff))))))))

(defun bits-float32 (bits)
  "Inverse de FLOAT32-BITS."
  (let* ((sign (if (logbitp 31 bits) -1 1))
         (biased (ldb (byte 8 23) bits))
         (fraction (ldb (byte 23 0) bits)))
    (* sign (if (zerop biased)
                (scale-float (float fraction 1.0) -149)
                (scale-float (float (+ fraction #x800000) 1.0) (- biased 150))))))

(defun osc-type-tag (argument)
  (etypecase argument
    (integer #\i)
    (real #\f)
    (string #\s)))

(defun osc-encode (message)
  "MESSAGE = (adresse . arguments) -> paquet OSC, vecteur d'octets."
  (destructuring-bind (address &rest arguments) message
    (coerce (append (osc-string-bytes address)
                    (osc-string-bytes
                     (concatenate 'string "," (map 'string #'osc-type-tag arguments)))
                    (loop for a in arguments
                          append (etypecase a
                                   (integer (osc-int32-bytes a))
                                   (real (osc-int32-bytes (float32-bits a)))
                                   (string (osc-string-bytes a)))))
            '(vector (unsigned-byte 8)))))

(defun osc-decode (buffer &optional (length (length buffer)))
  "Paquet OSC (vecteur d'octets, LENGTH premiers octets) -> (adresse . arguments).
BUFFER peut aussi etre une chaine (un caractere par octet)."
  (let ((buffer (if (stringp buffer) (map 'vector #'char-code buffer) buffer))
        (pos 0))
    (labels ((read-string ()
               (let* ((end (position 0 buffer :start pos :end length))
                      (s (map 'string #'code-char (subseq buffer pos end))))
                 (setf pos (* 4 (ceiling (1+ end) 4)))
                 s))
             (read-int32 ()
               (let ((n (loop for i from 0 below 4
                              sum (ash (aref buffer (+ pos i)) (* 8 (- 3 i))))))
                 (incf pos 4)
                 n)))
      (let ((address (read-string))
            (tags (if (< pos length) (read-string) ",")))
        (cons address
              (loop for tag across (subseq tags 1)
                    collect (ecase tag
                              (#\i (let ((n (read-int32)))
                                     (if (logbitp 31 n) (- n (ash 1 32)) n)))
                              (#\f (bits-float32 (read-int32)))
                              (#\s (read-string))
                              (#\T t)
                              (#\F nil)
                              (#\N nil))))))))

(defun osc-numbers (message)
  "Les arguments numeriques de MESSAGE, en liste."
  (remove-if-not #'numberp (rest message)))

;;; Envoi, reception

#+sbcl (defun send-osc (message host port)
	 "Envoie MESSAGE = (adresse . arguments) a HOST:PORT, par exemple
(send-osc '(\"/neuromuse/note\" 60 100) \"127.0.0.1\" 9001)."
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket
				 :type :datagram :protocol :udp))
	       (packet (osc-encode message)))
	   (sb-bsd-sockets:socket-connect s (sb-bsd-sockets:make-inet-address host) port)
	   (sb-bsd-sockets:socket-send s packet (length packet))
	   (sb-bsd-sockets:socket-close s)))

#+sbcl (defun receive-osc (port &optional (lenght 1024))
	 "Affiche les messages OSC recus sur PORT, tant que *OSC* est vrai."
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
	   (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
	   (loop while *osc* do
		 (multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght
										       :element-type '(unsigned-byte 8))
		   (declare (ignore address port))
		   (print (osc-decode buf len))))
	   (sb-bsd-sockets:socket-close s)))

;;; Serveurs : comme input-server, control-server et output-server
;;; (udp.lisp), mais en OSC. Meme pilotage : ils tournent tant que
;;; (superdaemon ann) est vrai, a lancer chacun dans son thread (mk-process).

#+sbcl (defgeneric osc-input-server (ann port lenght)
	 (:documentation "input server for neural nets, OSC : les arguments
numeriques de chaque message recu deviennent l'entree du reseau"))

#+sbcl (defmethod osc-input-server ((ann som) (port number) (lenght number))
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
	   (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
	   (loop while (superdaemon ann) do
		 (multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght
										       :element-type '(unsigned-byte 8))
		   (let ((message (osc-decode buf len)))
		     (when (verbose ann)
		       (format t "Received ~A from ~A:~A~%" message address port))
		     (setf (input ann) (coerce (osc-numbers message) 'vector)
			   (winner ann) (find-winner ann)
			   (output ann) (activation ann :n (car (id (winner ann))))))
		   (sleep (attention ann))))
	   (sb-bsd-sockets:socket-close s)))

#+sbcl (defmethod osc-input-server ((ann mlp) (port number) (lenght number))
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
	   (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
	   (loop while (superdaemon ann) do
		 (multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght
										       :element-type '(unsigned-byte 8))
		   (let ((message (osc-decode buf len)))
		     (when (verbose ann)
		       (format t "Received ~A from ~A:~A~%" message address port))
		     (setf (input ann) (osc-numbers message)))
		   (run-mlp ann)
		   (sleep (latence ann))))
	   (sb-bsd-sockets:socket-close s)))

;; control : le nom (slot ou action) est le dernier segment du chemin de
;; l'adresse (convention OSC habituelle -- la plupart des controleurs/patches
;; construisent l'adresse ainsi), par exemple
;;   ("/neuromuse/control/learn-fact" 0.1)  ->  (setf (learn-fact ann) 0.1)
;; ou, a defaut (quand ce segment ne nomme pas un accesseur existant),
;; l'ancienne convention, nom en premier argument :
;;   ("/neuromuse/control" "learn-fact" 0.1)  ->  (setf (learn-fact ann) 0.1)
;;
;; Si le nom resolu n'est pas un accesseur SETF-able mais une fonction
;; ordinaire -- typiquement une methode de generation specialisee sur ANN,
;; p. ex. GENERATE dans fred/transformer.lisp -- elle est appelee directement
;; avec ANN et les arguments restants, plutot que de forcer un SETF :
;;   ("/neuromuse/control/generate" 32)     ->  (generate ann 32)
;;   ("/neuromuse/control" "generate" 32)    ->  (generate ann 32)
;; meme serveur, meme thread (osc-control-server) : "control" couvre donc a
;; la fois le reglage continu d'un parametre et le declenchement ponctuel
;; d'une action, selon ce que le nom designe.

(defun osc-control (ann message)
  "Applique a ANN le message de controle MESSAGE (voir ci-dessus) : regle un
parametre (SETF sur un accesseur) ou declenche une action (appel direct
d'une fonction ordinaire, p. ex. une generation), selon ce que nomme
l'adresse."
  (labels ((apply-named (name args)
             "Si NAME nomme, dans :NEUROMUSE, un accesseur SETF-able, regle ce
slot de ANN avec (first args) ; sinon, si NAME nomme une fonction ordinaire,
l'appelle avec ANN et ARGS. Renvoie T si NAME a resolu a l'un des deux."
             (let ((symbol (find-symbol (string-upcase name) :neuromuse)))
               (cond
                 ((and symbol (fboundp (list 'setf symbol)))
                  (funcall (fdefinition (list 'setf symbol)) (first args) ann)
                  t)
                 ((and symbol (fboundp symbol))
                  (apply symbol ann args)
                  t)))))
    (let* ((address (first message))
           (args (rest message))
           (slash (position #\/ address :from-end t))
           (path-name (and slash (< (1+ slash) (length address)) (subseq address (1+ slash)))))
      (unless (and path-name (apply-named path-name args))
        (let ((name (and (stringp (first args)) (first args))))
          (cond
            ((null name)
             (warn "OSC control : impossible de determiner le nom (accesseur ou fonction) pour ~S" message))
            ((not (apply-named name (rest args)))
             (warn "OSC control : pas d'accesseur ni de fonction ~S" name))))))))

#+sbcl (defgeneric osc-control-server (ann port lenght)
	 (:documentation "control server for neural nets, OSC"))

#+sbcl (defmethod osc-control-server ((ann ann) (port number) (lenght number))
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
	   (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
	   (loop while (superdaemon ann) do
		 (multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght
										       :element-type '(unsigned-byte 8))
		   (let ((message (osc-decode buf len)))
		     (when (verbose ann)
		       (format t "Received ~A from ~A:~A~%" message address port))
		     (osc-control ann message))))
	   (sb-bsd-sockets:socket-close s)))

#+sbcl (defgeneric osc-output-server (ann ip port &optional address)
	 (:documentation "output server for neural nets, OSC : send activation to
ip port, as the arguments of an OSC message at ADDRESS"))

#+sbcl (defmethod osc-output-server ((ann som) (ip string) (port number)
				     &optional (address "/neuromuse/output"))
	 (loop while (superdaemon ann) do
	      (send-osc (cons address (mapcar (lambda (x) (float x 1.0)) (coerce (output ann) 'list)))
			ip port)
	      (sleep (latence ann))))

#+sbcl (defmethod osc-output-server ((ann mlp) (ip string) (port number)
				     &optional (address "/neuromuse/output"))
	 (loop while (superdaemon ann) do
	      (send-osc (cons address (mapcar (lambda (x) (float x 1.0)) (coerce (output ann) 'list)))
			ip port)
	      (sleep (latence ann))))

;eof
