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

;;; Comparaison des trajectoires : matrices de transition case -> case
;;
;; Plutot que comparer les gagnants successifs frame a frame (les 3 chants
;; n'ont pas la meme longueur ni le meme tempo, cf. *PINSON-CHANTS*), ou
;; qu'ignorer completement l'ordre comme INVENTAIRE/INVENTAIRE-H (qui ne
;; comptent que les cases visitees, pas les enchainements), on compte ici
;; les transitions d'une case a l'autre d'une frame a la suivante : la
;; "grammaire" du trajet sur la carte plutot que sa position absolue dans le
;; temps. Un DTW sur les sequences de gagnants resterait la methode a
;; essayer si cette comparaison-ci ne suffit pas a distinguer les chants.

(defun winner-sequence (chant &optional (net 'pinson-som))
  "Indice (0..N-1, PAS les coordonnees (colonne ligne) de PINSON-SOM-WINNER)
du neurone gagnant de NET pour chaque frame de CHANT, dans l'ordre -- la
trajectoire brute avant toute conversion en coordonnees de grille."
  (let ((som (if (symbolp net) (symbol-value net) net)))
    (mapcar (lambda (frame)
              (setf (input som) (coerce frame 'vector))
              (car (id (car (find-winner som)))))
            chant)))

(defun transition-matrix (winners &optional (size *pinson-som-size*))
  "Matrice SIZE x SIZE des comptes de transition : (aref m a b) = nombre de
fois ou le gagnant est passe de la case A a la case B d'une frame a la
suivante, dans WINNERS (une sequence d'indices de neurones, cf.
WINNER-SEQUENCE)."
  (let ((m (make-array (list size size) :initial-element 0)))
    (loop for (a b) on winners while b
          do (incf (aref m a b)))
    m))

(defun compare-transitions (m1 m2)
  "Similarite cosinus entre deux matrices de transition de meme taille,
vues comme deux vecteurs aplatis -- insensible au nombre total de
transitions (donc a la longueur des chants compares), contrairement a une
comparaison directe des comptes bruts. 1.0 = memes proportions de
transitions, 0.0 = aucune transition en commun."
  (let ((dims (array-dimensions m1))
        (dot 0) (n1 0) (n2 0))
    (dotimes (i (first dims))
      (dotimes (j (second dims))
        (let ((a (aref m1 i j)) (b (aref m2 i j)))
          (incf dot (* a b))
          (incf n1 (* a a))
          (incf n2 (* b b)))))
    (if (or (zerop n1) (zerop n2))
        0.0
        (float (/ dot (sqrt (* n1 n2)))))))

(defun compare-pinson-chants-transitions (&optional (net 'pinson-som))
  "Similarite cosinus (matrices de transition, cf. COMPARE-TRANSITIONS) entre
chaque paire des 3 chants de *PINSON-CHANTS*, sur l'etat courant de NET (pas
de reentrainement declenche ici) : ((0 1) sim01) ((0 2) sim02) ((1 2) sim12)."
  (let* ((seqs (mapcar (lambda (chant) (winner-sequence chant net))
		       *pinson-chants*))
         (mats (mapcar #'transition-matrix seqs)))
    (loop for i from 0 below (length mats)
          append (loop for j from (1+ i) below (length mats)
                       collect (list (list i j)
                                     (compare-transitions (nth i mats) (nth j mats)))))))

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

#|


(compare-pinson-chants-transitions)

;(time 
(train-pinson-som :epochs 200)
;; 146 seconds i7-6700HQ CPU @ 2.60GHz
;)					

(setf (learn-fact pinson-som) .3)
(train-pinson-som :epochs 300)
(setf (learn-fact pinson-som) .2)
(train-pinson-som :epochs 300)

;; chants originaux
(print 
(compare-pinson-chants-transitions)
; Il manque une ligne de base pour savoir ce qui est "proche" ou "différent" en absolu
 ; => (((0 1) 0.9249053) ((0 2) 0.9022291) ((1 2) 0.96922594)) ;
)

|#

;(defvar *stop* NIL)
(neuromuse-gui:gui 'pinson-som)

;;; Trace de l'activation, SANS apprentissage : un fichier par chant

(defun trace-pinson-chants (&key (net 'pinson-som) (prefix "pinson-trace-chant"))
  "Parcourt chacun des 3 chants de *PINSON-CHANTS*, SANS apprentissage
(FIND-WINNER seulement, pas LEARN -- NET reste tel quel), et journalise
l'activation de NET a chaque frame via TRACE-ACTIVATION (src/read-write.lisp) :
un fichier par chant, PREFIX1.lisp, PREFIX2.lisp, PREFIX3.lisp -- chacun
ecrase au debut de l'appel (pas de :append d'un appel sur l'autre), puis
rempli frame par frame. Relire un fichier avec READ-ACTIVATION-TRACE."
  (let ((som (if (symbolp net) (symbol-value net) net)))
    (loop for chant in *pinson-chants*
          for i from 1
          do (let ((path (format nil "~A~D.lisp" prefix i)))
               (ignore-errors (delete-file path))
               (dolist (frame chant)
                 (setf (input som) (coerce frame 'vector))
                 (find-winner som)
                 (trace-activation som path))))
    (values som)))

; (trace-pinson-chants)


;; EOF
