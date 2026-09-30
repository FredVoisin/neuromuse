;;;; pinson_som.lisp - Carte auto-organisatrice (SOM) 12x12 pour les vecteurs
;;;; de chant de pinson (Fringilla coelebs) extraits dans pinson_vecteurs.lisp :
;;;; 144 neurones en grille carree, 18 entrees (une par bande FFT, voir
;;;; *pinson-freqs*).
;;;;
;;;; Usage, package :neuromuse courant :
;;;;   (load "examples/pinson_vecteurs.lisp")   ; *pinson-freqs*, *pinson-chants*
;;;;   (load "examples/pinson_som.lisp")        ; construit pinson-som
;;;;   (train-pinson-som)                       ; quelques epoques d'apprentissage
;;;;   (pinson-som-winner (first (first *pinson-chants*)))  ; teste un vecteur
;;;;
(in-package :neuromuse)

(unless (boundp '*pinson-chants*)
  (load (merge-pathnames "pinson_vecteurs.lisp"
                         (or *load-pathname* #P"examples/"))))

(defparameter *pinson-som-side* 12 "cote de la grille carree.")
(defparameter *pinson-som-size* (* *pinson-som-side* *pinson-som-side*) "144 neurones.")
(defparameter *pinson-som-input* (length *pinson-freqs*) "18 bandes FFT.")

(unless (and (boundp 'pinson-som) (ann-p pinson-som))
  (make-instance 'som :name 'pinson-som
                       :size *pinson-som-size* :input *pinson-som-input*)
  (setf (learn-fact pinson-som) .3
        (radius pinson-som) 2
        (temp pinson-som) 0.0
	(net-temp pinson-som) 0.05))

(defun all-pinson-vectors ()
  "Les vecteurs des 3 chants de *PINSON-CHANTS*, mis bout a bout."
  (apply #'append *pinson-chants*))

(defun shuffled-indices (n)
  (let ((v (make-array n)))
    (dotimes (i n) (setf (aref v i) i))
    (loop for i from (1- n) downto 1
          do (rotatef (aref v i) (aref v (random (1+ i)))))
    (coerce v 'list)))

(defun train-pinson-som (&key (epochs 20) (net 'pinson-som)
                               (vectors (all-pinson-vectors)) (verbose nil))
  "Presente chaque vecteur de VECTORS, EPOCHS fois, dans un ordre tire au
hasard a chaque epoque (un ordre fixe repete a l'identique favorise des
solutions degenerees -- meme constat que pour le XOR, voir EXAMPLES.md)."
  (let* ((som (if (symbolp net) (symbol-value net) net))
         (n (length vectors)))
    (dotimes (e epochs (values som))
      (dolist (i (shuffled-indices n))
        (setf (input som) (coerce (nth i vectors) 'vector))
        (learn som))
      (when verbose (format t "~&epoch ~D/~D~%" (1+ e) epochs)))))

(defun pinson-som-winner (vector &optional (net 'pinson-som))
  "Coordonnees (colonne ligne) sur la grille du neurone gagnant de NET pour
VECTOR (un vecteur d'intensite a *PINSON-SOM-INPUT* bandes)."
  (let ((som (if (symbolp net) (symbol-value net) net)))
    (setf (input som) (coerce vector 'vector))
    (2d (car (id (car (find-winner som)))) (length (net som)))))

;; petit test de sanite : le premier vecteur de chaque chant, avant et apres
;; apprentissage -- pas cense converger vers une carte topologiquement fine
;; en si peu d'epoques, juste montrer que la boucle tourne et que
;; PINSON-SOM-WINNER repond une case de la grille 12x12 a chaque fois.
;;
;;   (dolist (chant *pinson-chants*)
;;     (format t "~&gagnant (avant) : ~S~%" (pinson-som-winner (first chant))))
;;   (train-pinson-som :epochs 20)
;;   (dolist (chant *pinson-chants*)
;;     (format t "~&gagnant (apres) : ~S~%" (pinson-som-winner (first chant))))

;(init pinson-som :input 18 :size 144)
					;(net pinson-som)

#|
(setf *w-chant1*
      (let ((w ))
	(dolist (frame (first *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))
      *w-chant2*
      (let ((w ))
	(dolist (frame (second *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))      
      *w-chant3*
      (let ((w ))
	(dolist (frame (third *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))
      )
(describe pinson-som)
(train-pinson-som :epochs 100)

(setf *w100-chant1*
      (let ((w ))
	(dolist (frame (first *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))
      *w100-chant2*
      (let ((w ))
	(dolist (frame (second *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))      
      *w100-chant3*
      (let ((w ))
	(dolist (frame (third *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))
      )

|#

;;; ANALYSE

;; INVENTAIRE et INVENTAIRE-H vivent maintenant dans src/maths.lisp
;; (bibliotheque coeur) : deja disponibles ici sans rien redefinir.


;(sort (inventaire l) #'> :key #'second)

(sort (inventaire *w-chant1*) #'> :key #'second)
(sort (inventaire *w100-chant1*) #'> :key #'second)

(sort (inventaire *w-chant2*) #'> :key #'second)
(sort (inventaire *w100-chant2*) #'> :key #'second)

(sort (inventaire *w-chant3*) #'> :key #'second)
(sort (inventaire *w100-chant3*) #'> :key #'second)
)

(length (intersection (inventaire *w-chant1*)  (inventaire *w-chant2*)
:test #'(lambda (x y) (equalp (car x) (car y)))))

(length (intersection (inventaire *w100-chant1*)  (inventaire *w100-chant2*)
:test #'(lambda (x y) (equalp (car x) (car y)))))


(describe pinson-som)
(setf (learn-fact pinson-som) .0)
(train-pinson-som :epochs 200)

(epoch pinson-som)

(setf *w200-chant1*
      (let ((w ))
	(dolist (frame (first *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))
      *w200-chant2*
      (let ((w ))
	(dolist (frame (second *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))      
      *w200-chant3*
      (let ((w ))
	(dolist (frame (third *pinson-chants*) (reverse w))
	  (push (pinson-som-winner frame) w)))
)


(- (length (inventaire *w-chant1*))
(length (intersection (inventaire *w-chant1*)  (inventaire *w-chant2*)
		      :test #'(lambda (x y) (equalp (car x) (car y))))))

(- (length (inventaire *w-chant1*))
(length (intersection (inventaire *w-chant1*)  (inventaire *w-chant3*)
		      :test #'(lambda (x y) (equalp (car x) (car y))))))

(- (length (inventaire *w-chant1*))
(length (intersection (inventaire *w-chant2*)  (inventaire *w-chant3*)
		      :test #'(lambda (x y) (equalp (car x) (car y))))))



(- (length (inventaire *w-chant1*))
(length (intersection (inventaire *w200-chant1*)  (inventaire *w200-chant2*)
		      :test #'(lambda (x y) (equalp (car x) (car y))))))

(- (length (inventaire *w200-chant1*))
(length (intersection (inventaire *w200-chant1*)  (inventaire *w200-chant3*)
		      :test #'(lambda (x y) (equalp (car x) (car y))))))

(- (length (inventaire *w200-chant1*))
(length (intersection (inventaire *w200-chant2*)  (inventaire *w200-chant3*)
		      :test #'(lambda (x y) (equalp (car x) (car y))))))
;;=> ~65


|#

;; EOF
