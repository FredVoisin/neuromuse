;;;; midi.lisp -- Lecture de fichiers MIDI standard (format 0/1, division en
;;;; ticks par noire) et conversion vers le format de partition utilise par
;;;; fred/acdll/examples/acdll-data.lisp : une SECTION est une liste
;;;; d'evenements, un par temps (noire) ; chaque evenement est une liste de
;;;; (hauteur-MIDI duree-en-temps), du grave a l'aigu, ou NIL si rien
;;;; n'attaque sur ce temps.
;;;;
;;;; Usage, apres (asdf:load-system :neuromuse) :
;;;;   (midi-file-to-acdll "data/Dukas_Apprenti_sorcier.mid")
;;;;
;;;; Pas de bibliotheque externe : le format est lu directement (chunks
;;;; MThd/MTrk, quantites a longueur variable, running status -- cf. la
;;;; specification MIDI 1.0 / Standard MIDI Files 1.0).
;;;;
;;;; Limites volontaires (cf. docstrings) : pas de decoupage automatique en
;;;; sections (une seule section par fichier, a redecouper/combiner a la
;;;; main si besoin), pas de detection de tonique (:TONIC reste a la charge
;;;; de l'appelant, cf. acdll-data.lisp), code temporel SMPTE refuse
;;;; (seul ticks-par-noire est gere), chevauchement de deux notes de meme
;;;; hauteur sur le meme canal resolu en pile (LIFO) plutot qu'en file.

(in-package :neuromuse)

;;; ------------------------------------------------------------------
;;; 1. Lecture bas niveau (entiers big-endian, quantite a longueur variable)

(defun read-u16 (stream)
  (+ (ash (read-byte stream) 8) (read-byte stream)))

(defun read-u32 (stream)
  (loop with n = 0
        repeat 4
        do (setf n (+ (ash n 8) (read-byte stream)))
        finally (return n)))

(defun read-chunk-type (stream)
  "Les 4 octets ASCII d'un en-tete de chunk (\"MThd\", \"MTrk\"...)."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence buf stream)
    (map 'string #'code-char buf)))

(defun read-vlq (stream)
  "Quantite a longueur variable MIDI : 7 bits utiles par octet lu, le bit de
poids fort indiquant s'il faut lire un octet de plus."
  (loop with n = 0
        for byte = (read-byte stream)
        do (setf n (logior (ash n 7) (logand byte #x7f)))
        while (logbitp 7 byte)
        finally (return n)))

;;; ------------------------------------------------------------------
;;; 2. Evenements d'une piste -> notes (hauteur on off canal), en ticks

(defun channel-message-data-count (kind)
  "Nombre d'octets de donnee d'un message de canal, KIND = les 4 bits de
poids fort du statut (8 = note off, 9 = note on, A = pression polyphonique,
B = controleur, C = changement de programme, D = pression de canal,
E = pitch bend)."
  (case kind
    ((#x8 #x9 #xA #xB #xE) 2)
    ((#xC #xD) 1)
    (t 0)))

(defun read-midi-track-notes (stream end-position)
  "Notes de la piste MTrk commencant a la position courante de STREAM, qui
doit s'arreter exactement a END-POSITION (position en octets, juste apres
la piste -- longueur deja lue par l'appelant). Renvoie une liste de
(HAUTEUR ON-TICK OFF-TICK CANAL) ; les notes jamais relachees avant la fin
de la piste sont ignorees. RUNNING STATUS gere (cf. spec) ; les evenements
meta (FF) et exclusifs (F0/F7) sont sautes sans modifier le statut courant
-- seuls les messages de canal (80-EF) le font."
  (let ((tick 0) (running-status nil) (open (make-hash-table :test 'equal))
        (notes '()))
    (flet ((note-on (channel pitch velocity)
             (if (zerop velocity)
                 (let* ((key (cons channel pitch)) (on (pop (gethash key open))))
                   (when on (push (list pitch on tick channel) notes)))
                 (push tick (gethash (cons channel pitch) open))))
           (note-off (channel pitch)
             (let* ((key (cons channel pitch)) (on (pop (gethash key open))))
               (when on (push (list pitch on tick channel) notes)))))
      (loop while (< (file-position stream) end-position)
            do (let* ((delta (read-vlq stream))
                      (byte (read-byte stream))
                      (status (if (>= byte #x80) byte running-status))
                      (data1 (unless (>= byte #x80) byte)))
                 (incf tick delta)
                 (when (and (>= byte #x80) (<= byte #xef))
                   (setf running-status byte))
                 (cond
                   ((= status #xff)                     ; meta
                    (let ((len (progn (read-byte stream) (read-vlq stream))))
                      (dotimes (i len) (read-byte stream))))
                   ((or (= status #xf0) (= status #xf7)) ; exclusif (sysex)
                    (let ((len (read-vlq stream)))
                      (dotimes (i len) (read-byte stream))))
                   ((<= #x80 status #xef)                ; message de canal
                    (let* ((channel (logand status #x0f))
                           (kind (ash status -4))
                           (d1 (or data1 (read-byte stream)))
                           (d2 (when (= 2 (channel-message-data-count kind))
                                 (read-byte stream))))
                      (case kind
                        (#x8 (note-off channel d1))
                        (#x9 (note-on channel d1 d2))
                        (t nil))))
                   (t nil)))))        ; systeme temps-reel (rares en fichier)
    (nreverse notes)))

;;; ------------------------------------------------------------------
;;; 3. Fichier complet -> notes fusionnees, triees

(defun read-midi-file (path)
  "Lit le fichier MIDI standard PATH (format 0 ou 1, division en
ticks-par-noire). Renvoie en valeurs : (1) la liste de toutes les notes, de
toutes les pistes, fusionnees et triees par attaque -- (HAUTEUR ON-TICK
OFF-TICK CANAL) ; (2) DIVISION, le nombre de ticks par noire."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (unless (string= (read-chunk-type in) "MThd")
      (error "~S : pas un fichier MIDI standard (pas de chunk MThd en tete)." path))
    (let* ((header-length (read-u32 in))
           (header-end (+ (file-position in) header-length)))
      (read-u16 in)                    ; format (0 ou 1), pas utilise ici
      (let ((ntrks (read-u16 in))
            (division (read-u16 in)))
        (when (logbitp 15 division)
          (error "~S : code temporel SMPTE (division ~D) -- seul le format ~
ticks-par-noire est gere." path division))
        (file-position in header-end)  ; au cas ou le chunk MThd aurait plus
        (let ((notes '()))             ; de 6 octets (extension future)
          (dotimes (i ntrks)
            (let* ((chunk-type (read-chunk-type in))
                   (chunk-length (read-u32 in))
                   (chunk-end (+ (file-position in) chunk-length)))
              (when (string= chunk-type "MTrk")
                (setf notes (append notes (read-midi-track-notes in chunk-end))))
              (file-position in chunk-end)))
          (values (sort notes #'< :key #'second) division))))))

;;; ------------------------------------------------------------------
;;; 4. Notes en ticks -> evenements acdll (un par temps)

(defun quantize-to-beats (notes beat-ticks)
  "NOTES (voir READ-MIDI-FILE) -> une liste d'evenements, un par temps de
BEAT-TICKS ticks, du premier temps (0) au dernier couvert par une note :
chaque evenement une liste de (HAUTEUR DUREE-EN-TEMPS), du grave a l'aigu,
ou NIL si rien n'attaque ce temps-la -- meme convention que acdll-data.lisp.
Attaque et duree arrondies au temps le plus proche (duree jamais inferieure
a 1, une note plus courte qu'un temps occupant quand meme son temps).

Si BEAT-TICKS est trop grossier pour le fichier (plusieurs notes dont les
attaques reelles sont clairement distinctes tombent sur le meme temps),
WARN plutot que de les fondre silencieusement en un accord qu'elles ne
forment pas -- reessayer avec un BEAT-TICKS plus fin (DIVISION/2, /4...)."
  (let* ((steps (mapcar (lambda (n)
                           (destructuring-bind (pitch on off channel) n
                             (declare (ignore channel))
                             (list pitch on (round on beat-ticks)
                                   (max 1 (round (- off on) beat-ticks)))))
                         notes))
         (last-step (reduce #'max steps :key (lambda (s) (+ (third s) (fourth s)))
                             :initial-value 0)))
    (loop for i from 0 below last-step
          collect (let ((here (loop for (pitch on beat dur) in steps
                                     when (= beat i) collect (list pitch on dur))))
                    (when (> (length here) 1)
                      (let ((spread (- (reduce #'max here :key #'second)
                                        (reduce #'min here :key #'second))))
                        (when (> spread (max 1 (floor beat-ticks 8)))
                          (warn "Temps ~D : ~D notes fondues en un accord alors que leurs ~
attaques reelles s'etalent sur ~D ticks (BEAT-TICKS = ~D) -- probablement ~
des notes successives, pas un accord ; essayer un BEAT-TICKS plus fin ~
(p. ex. ~D)." i (length here) spread beat-ticks (floor beat-ticks 2)))))
                    (when here
                      (sort (mapcar (lambda (e) (list (first e) (third e))) here)
                            #'< :key #'first))))))

(defun midi-file-to-acdll (path &key beat-ticks)
  "Lit PATH et renvoie une liste a un seul element, une SECTION au format de
fred/acdll/examples/acdll-data.lisp (liste d'evenements, voir
QUANTIZE-TO-BEATS) -- directement utilisable comme :FORM d'une entree de
*ACDLL-VERSIONS*, par exemple :
  (list :name \"...\" :tonic <a determiner> :form (midi-file-to-acdll \"data/x.mid\"))
Pas de decoupage automatique en plusieurs sections (AABA...) ni de
detection de tonique : a faire a la main sur le resultat si besoin --
rien dans un fichier MIDI generique ne donne la forme musicale, et la
tonique depend d'une analyse harmonique que ceci ne fait pas.
BEAT-TICKS : ticks par temps, par defaut DIVISION du fichier (une noire) --
mauvais defaut si la piece est ecrite plus fin (croches, doubles...) : dans
ce cas QUANTIZE-TO-BEATS avertit (notes fondues en accord alors que leurs
attaques sont distinctes) ; reessayer avec un BEAT-TICKS plus petit."
  (multiple-value-bind (notes division) (read-midi-file path)
    (list (quantize-to-beats notes (or beat-ticks division)))))

;;; (read-midi-file "data/Dukas_Apprenti_sorcier.mid")
;;; ;; DIVISION = 480 (une noire) ; theme en doubles-croches -> BEAT-TICKS
;;; ;; par defaut (480) trop grossier, QUANTIZE-TO-BEATS avertit (notes
;;; ;; fondues en accord). Avec :BEAT-TICKS 120 (une double-croche) : melodie
;;; ;; propre, un seul avertissement residuel (une note decalee de 60 ticks).
;;; (midi-file-to-acdll "data/Dukas_Apprenti_sorcier.mid" :beat-ticks 120)

; eof
