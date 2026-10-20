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
;;; (superdaemon ann) est vrai, a lancer chacun dans son thread (mk-process) --
;;; piege classique : si SUPERDAEMON est encore NIL au lancement, le thread se
;;; termine tout de suite, sans la moindre erreur (rien n'a ete ecoute ni
;;; envoye). WARN-UNLESS-SUPERDAEMON, appele en tete de chacun des cinq
;;; serveurs ci-dessous, le signale au lieu de laisser ce silence passer
;;; inapercu.

(defun warn-unless-superdaemon (ann)
  "Avertit si (superdaemon ANN) est NIL : lance maintenant, ce serveur OSC se
terminerait immediatement -- la boucle teste (superdaemon ann) avant de rien
ecouter ni envoyer, sans la moindre erreur sinon. (setf (superdaemon ann) t)
avant de (re)lancer le thread (mk-process)."
  (unless (superdaemon ann)
    (warn "~S : SUPERDAEMON est NIL -- ce serveur OSC va se terminer ~
immediatement, sans rien ecouter ni envoyer. (setf (superdaemon ann) t) ~
avant de (re)lancer ce thread." ann)))

#+sbcl (defgeneric osc-input-server (ann port lenght)
	 (:documentation "input server for neural nets, OSC : les arguments
numeriques de chaque message recu deviennent l'entree du reseau"))

#+sbcl (defmethod osc-input-server ((ann som) (port number) (lenght number))
	 (warn-unless-superdaemon ann)
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
	   ;; UNWIND-PROTECT : SOCKET-CLOSE doit s'executer meme si ce thread est
	   ;; interrompu par SB-THREAD:TERMINATE-THREAD pendant qu'il est bloque
	   ;; dans SOCKET-RECEIVE -- sinon le socket fuit (un fd est partage par
	   ;; tout le process sous Linux, pas par thread : le port reste occupe
	   ;; indefiniment, sans qu'aucun thread n'y soit plus associe ni visible
	   ;; dans SB-THREAD:LIST-ALL-THREADS).
	   (unwind-protect
		(progn
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
			  (sleep (attention ann)))))
	     (sb-bsd-sockets:socket-close s))))

#+sbcl (defmethod osc-input-server ((ann mlp) (port number) (lenght number))
	 (warn-unless-superdaemon ann)
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
	   (unwind-protect
		(progn
		  (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
		  (loop while (superdaemon ann) do
			(multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght
											       :element-type '(unsigned-byte 8))
			  (let ((message (osc-decode buf len)))
			    (when (verbose ann)
			      (format t "Received ~A from ~A:~A~%" message address port))
			    (setf (input ann) (osc-numbers message)))
			  (run-mlp ann)
			  (sleep (latence ann)))))
	     (sb-bsd-sockets:socket-close s))))

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
	 (warn-unless-superdaemon ann)
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
	   (unwind-protect
		(progn
		  (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
		  (loop while (superdaemon ann) do
			(multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght
											       :element-type '(unsigned-byte 8))
			  (let ((message (osc-decode buf len)))
			    (when (verbose ann)
			      (format t "Received ~A from ~A:~A~%" message address port))
			    (osc-control ann message)))))
	     (sb-bsd-sockets:socket-close s))))

#+sbcl (defgeneric osc-output-server (ann ip port &optional address)
	 (:documentation "output server for neural nets, OSC : send activation to
ip port, as the arguments of an OSC message at ADDRESS"))

#+sbcl (defmethod osc-output-server ((ann som) (ip string) (port number)
				     &optional (address "/neuromuse/output"))
	 (warn-unless-superdaemon ann)
	 (loop while (superdaemon ann) do
	      (send-osc (cons address (mapcar (lambda (x) (float x 1.0)) (coerce (output ann) 'list)))
			ip port)
	      (sleep (latence ann))))

#+sbcl (defmethod osc-output-server ((ann mlp) (ip string) (port number)
				     &optional (address "/neuromuse/output"))
	 (warn-unless-superdaemon ann)
	 (loop while (superdaemon ann) do
	      (send-osc (cons address (mapcar (lambda (x) (float x 1.0)) (coerce (output ann) 'list)))
			ip port)
	      (sleep (latence ann))))

;;; ------------------------------------------------------------------
;;; Lecteur temps reel generique : un reseau (TRANSFORMER ou RMLP) ou une
;;; partition (:version, sans reseau) joue(e) note par note vers HOST:PORT,
;;; au tempo. Promu depuis fred/acdll/examples/acdll-transformer.lisp --
;;; generalise a n'importe quel reseau/partition (plus seulement
;;; acdll-transformer/acdll-rmlp vers *hote*/*port*) : cf.
;;; fred/lambda-calcul-neuromuse.md pour l'historique.
;;;
;;; SCORE/SECTIONS : deux listes paralleles -- SCORE un evenement par temps
;;; (liste de (hauteur duree), ou NIL si rien n'attaque), SECTIONS la
;;; valeur du neurone de section au meme temps -- cf. VERSION-EVENTS/
;;; VERSION-SECTIONS (fred/acdll/examples/acdll-data.lisp) ou tout
;;; equivalent : ce fichier ne connait aucune convention de partition
;;; particuliere, seulement cette forme generique.
;;;
;;; DECODE/ENCODE (vecteur <-> (accord section)) : propres au vocabulaire
;;; d'entrainement du reseau (ENCODE-CHORD/DECODE-CHORD et leur TONIC pour
;;; acdll, DUKAS-ENCODE-CHORD/DUKAS-DECODE-CHORD pour Dukas, etc.) -- aucun
;;; defaut ici, ce fichier ne connait aucun codage particulier non plus.

(defvar *osc-stop* nil
  "Mis a T par STOP-OSC : la lecture s'arrete au temps suivant.")

(defvar *osc-player* nil
  "Le thread de la lecture en cours, s'il y en a une.")

(defun wait-until (time)
  "Attend jusqu'a TIME, en unites de GET-INTERNAL-REAL-TIME."
  (let ((delay (/ (- time (get-internal-real-time))
                  internal-time-units-per-second)))
    (when (plusp delay) (sleep delay))))

(defun play-events-osc (next-event host port &key (tempo 92) (events 64) (velocity 90)
                                        (verbose t)
                                        (note-address "/neuromuse/note")
                                        (section-address "/neuromuse/section"))
  "Joue EVENTS evenements fournis par NEXT-EVENT (un generateur -- cf.
TRANSFORMER-GENERATOR/RMLP-GENERATOR/VERSION-GENERATOR, ou toute fonction
sans argument renvoyant (accord section) a chaque appel), un par temps, a
TEMPO noires par minute, envoyes en OSC a HOST:PORT. Chaque note (hauteur
duree) recoit un note-on (NOTE-ADDRESS) a son temps d'attaque et un
note-off DUREE temps plus tard (ou avant, si la meme hauteur est rejouee
entre-temps) ; un changement de section envoie SECTION-ADDRESS. L'evenement
suivant est calcule juste apres l'envoi du precedent, pendant le temps qui
les separe ; les echeances sont absolues (pas de derive si un calcul prend
du temps)."
  (let* ((beat (round (* internal-time-units-per-second (/ 60 tempo))))
         (start (+ (get-internal-real-time) beat))
         (active '())                  ; (hauteur . temps restants)
         (section nil)
         (event (funcall next-event)))
    (setf *osc-stop* nil)
    (unwind-protect
         (dotimes (k events)
           (when *osc-stop* (return))
           (wait-until (+ start (* k beat)))
           (destructuring-bind (chord sec) event
             ;; notes qui finissent, et notes rejouees : note-off d'abord
             (setf active (loop for (pitch . left) in active
                                if (or (<= left 1) (assoc pitch chord))
                                  do (send-osc (list note-address pitch 0) host port)
                                else collect (cons pitch (1- left))))
             (dolist (note chord)
               (send-osc (list note-address (first note) velocity) host port)
               (push (cons (first note) (second note)) active))
             (unless (eql sec section)
               (setf section sec)
               (send-osc (list section-address (float sec)) host port))
             (when verbose
               (format t "~&~3D  section ~4,2F  ~S" k sec chord)
               (force-output)))
           (setf event (funcall next-event)))
      ;; fin ou interruption : laisser sonner les notes restantes, puis arret
      (unless *osc-stop*
        (wait-until (+ start (* (+ events (reduce #'max active :key #'cdr
                                                          :initial-value 1)
                                   -1)
                                beat))))
      (dolist (a active)
        (send-osc (list note-address (car a) 0) host port))))
  (values))

;;; Generateurs : chacun est une fonction sans argument qui renvoie, a
;;; chaque appel, l'evenement suivant sous la forme (accord section).

(defun version-generator (score sections &key (cycle t))
  "Rejoue SCORE/SECTIONS tels quels (aucun reseau), en boucle si CYCLE."
  (let ((i 0) (len (length score)))
    (lambda ()
      (when (and cycle (>= i len)) (setf i 0))
      (prog1 (if (< i len)
                 (list (nth i score) (nth i sections))
                 (list nil (car (last sections))))
        (incf i)))))

(defun transformer-generator (net score sections &key (cycle t) decode encode)
  "NET (une instance TRANSFORMER) en generation libre a partir du premier
evenement de SCORE/SECTIONS : chaque sortie est decodee (DECODE), reencodee
(ENCODE) et ajoutee a la sequence (fenetre glissante de (window net)
trames). Si CYCLE, repart du debut toutes les (length SCORE) evenements ;
sinon continue indefiniment -- au-dela de la fenetre, le reseau voit des
positions decalees par rapport a l'apprentissage, et invente."
  (let* ((start (list (first score) (first sections)))
         (len (length score))
         (frames '())
         (i 0))
    (lambda ()
      (when (and cycle (zerop (mod i len)))
        (setf frames '()))
      (incf i)
      (let ((event (if frames
                       (funcall decode (run-transformer net :in (last frames (window net))))
                       start)))
        (setf frames (append (last frames (1- (window net)))
                             (list (funcall encode (first event) (second event)))))
        event))))

(defun rmlp-generator (net score sections &key (cycle t) decode encode)
  "NET (une instance RMLP) en generation libre a partir du premier evenement
de SCORE/SECTIONS : chaque prediction est reinjectee (memoire recurrente
comprise). Si CYCLE, repart du debut (memoire a zero) toutes les
(length SCORE) evenements ; sinon continue indefiniment. Pas de :IN pour
RUN-MLP, qui court-circuiterait la memoire (le contexte ne serait pas
ajoute a l'entree) -- (input net) est mis a jour par SETF, puis RUN-MLP
appele sans argument."
  (let* ((start (list (first score) (first sections)))
         (len (length score))
         (current nil)
         (i 0))
    (lambda ()
      (when (or (null current) (and cycle (zerop (mod i len))))
        (setf (recurrent-layer-activation net)
              (make-list (nth (recurrent-layer net) (hidden-size net)) :initial-element 0))
        (setf current nil))
      (incf i)
      (setf current
            (if current
                (destructuring-bind (event section) current
                  (setf (input net) (funcall encode event section))
                  (funcall decode (run-mlp net)))
                start)))))

(defun play-osc (net score sections &key host port (tempo 92) events (velocity 90)
                                          (cycle t) (thread t) (verbose t)
                                          decode encode
                                          (note-address "/neuromuse/note")
                                          (section-address "/neuromuse/section"))
  "Joue en temps reel, vers HOST:PORT, la generation de NET a partir du
premier element de SCORE/SECTIONS (cf. ci-dessus) :
  une instance TRANSFORMER   -> TRANSFORMER-GENERATOR
  une instance RMLP          -> RMLP-GENERATOR
  :version                   -> VERSION-GENERATOR (rejoue SCORE/SECTIONS
                                 tels quels, sans reseau -- pour regler Pd)
DECODE/ENCODE : requis pour un reseau (ignores pour :version), propres a
son vocabulaire d'entrainement -- cf. l'en-tete de ce bloc. EVENTS : nombre
de temps joues, par defaut (length SCORE). CYCLE : voir les generateurs.
THREAD (vrai par defaut) : lecture dans son propre thread, la main revient
tout de suite ; (stop-osc) l'arrete."
  (let* ((n (or events (length score)))
         (generator (etypecase net
                      (transformer (transformer-generator net score sections
                                                           :cycle cycle :decode decode :encode encode))
                      (rmlp (rmlp-generator net score sections
                                            :cycle cycle :decode decode :encode encode))
                      (symbol (if (eq net :version)
                                  (version-generator score sections :cycle cycle)
                                  (error "PLAY-OSC : NET ~S non reconnu (TRANSFORMER, RMLP, ou :VERSION)." net)))))
         (job (lambda ()
                (play-events-osc generator host port :tempo tempo :events n
                                                      :velocity velocity :verbose verbose
                                                      :note-address note-address
                                                      :section-address section-address))))
    (stop-osc)
    (if thread
        (setf *osc-player*
              (mk-process (format nil "osc-player-~(~A~)" (if (symbolp net) net (name net))) job))
        (funcall job))))

(defun stop-osc ()
  "Arrete la lecture en cours (notes coupees) et attend la fin du thread."
  (when (and *osc-player* (sb-thread:thread-alive-p *osc-player*))
    (setf *osc-stop* t)
    (sb-thread:join-thread *osc-player* :default nil))
  (setf *osc-player* nil)
  (values))

;eof
