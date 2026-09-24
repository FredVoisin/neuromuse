;;; Perceptron, exemple 1 : reconnaissance des chiffres 1 à 0 sur une rétine de 5x6
;;;
;;; D'après run_perceptron.lisp, Frédéric Voisin, IRCAM, mars 1999
;;; (original conservé sous legacy/).
;;;
;;; (asdf:load-system :neuromuse)
;;; (in-package :neuromuse)
;;; puis évaluer les formes une par une (Emacs/SLIME).

(in-package :neuromuse)

;; stimuli : les chiffres 1, 2, ..., 9, 0 dessinés sur une rétine de 5 x 6 cellules
(defvar *chiffres*
  '(#(
    0 0 1 0 0
    0 0 1 0 0
    0 0 1 0 0
    0 0 1 0 0
    0 0 1 0 0
    0 0 1 0 0 )
    
    #(
    1 1 1 1 1
    0 0 0 0 1
    1 1 1 1 1
    1 0 0 0 0
    1 0 0 0 0
    1 1 1 1 1 )
    
    #(
    1 1 1 1 1
    0 0 0 0 1
    1 1 1 1 1
    0 0 0 0 1
    0 0 0 0 1
    1 1 1 1 1  )
    
    #(
    1 0 0 0 0
    1 0 0 0 0
    1 0 0 0 0
    1 0 0 1 0
    1 1 1 1 1
    0 0 0 1 0 )
    
    #(
    1 1 1 1 1
    1 0 0 0 0
    1 0 0 0 0
    1 1 1 1 1
    0 0 0 0 1
    1 1 1 1 1 )
    
    #(
    1 1 1 1 1
    1 0 0 0 0
    1 0 0 0 0
    1 1 1 1 1
    1 0 0 0 1
    1 1 1 1 1 )
    
    #(
    1 1 1 1 0
    0 0 0 1 0
    0 0 0 1 0
    0 0 1 1 1
    0 0 0 1 0
    0 0 0 1 0 )
    
    #(
    1 1 1 1 1
    1 0 0 0 1
    1 1 1 1 1
    1 0 0 0 1
    1 0 0 0 1
    1 1 1 1 1 )
    
    #(
    1 1 1 1 1
    1 0 0 0 1
    1 1 1 1 1
    0 0 0 0 1
    0 0 0 0 1
    1 1 1 1 1  )
    
    #(
    1 1 1 1 1
    1 0 0 0 1
    1 0 0 0 1
    1 0 0 0 1
    1 0 0 0 1
    1 1 1 1 1 )))

;; buts : une cellule de sortie par chiffre
(defvar *chiffres-buts*
  '(#(1 0 0 0 0 0 0 0 0 0)   ;; output goals
      #(0 1 0 0 0 0 0 0 0 0)
      #(0 0 1 0 0 0 0 0 0 0)
      #(0 0 0 1 0 0 0 0 0 0)
      #(0 0 0 0 1 0 0 0 0 0)
      #(0 0 0 0 0 1 0 0 0 0)
      #(0 0 0 0 0 0 1 0 0 0)
      #(0 0 0 0 0 0 0 1 0 0)
      #(0 0 0 0 0 0 0 0 1 0)
      #(0 0 0 0 0 0 0 0 0 1)))

;; un perceptron de 30 entrées (la rétine) et 10 sorties
(make-perceptron chiffres 30 10)

(setf (learn-fact chiffres) 0.01)

;; apprentissage, jusqu'à ce qu'une époque soit sans erreur
(train-perceptron chiffres *chiffres* *chiffres-buts* :verbose t)

;; test
(dolist (s *chiffres*)
  (format t "~&~S" (run-perceptron chiffres :in s)))

;; un « 1 » déformé : quelles cellules s'allument ? (généralisation non garantie :
;; un perceptron sans couche cachée ne sait que séparer linéairement)
(run-perceptron chiffres :in #(0 1 1 0 0
                               0 0 1 0 0
                               0 0 1 0 0
                               0 0 1 0 0
                               0 0 1 0 0
                               0 0 1 1 0))

;;; Perceptron probabiliste « à la Boltzmann » (run-perceptron-B en 1999) :
;;; la fonction de transfert devient stochastique, TEMP est la température (> 0).

(make-perceptron chiffres-b 30 10)

;; en 1999 : température 3.0 ; l'apprentissage converge alors lentement
;; (souvent plus de 1000 époques, cf. (stop chiffres-b)). Un taux d'erreur nul
;; n'étant jamais garanti avec une sortie stochastique, on tolère ici 10 %.
(setf (out-fun chiffres-b) #'boltzmann
      (temp chiffres-b) 1.0
      (learn-fact chiffres-b) 0.01
      (threshold chiffres-b) 0.1)

(train-perceptron chiffres-b *chiffres* *chiffres-buts* :verbose t)

;; à température plus basse, la réponse est plus stable
(setf (temp chiffres-b) 0.2)

(run-perceptron chiffres-b :in #(0 1 1 1 1
                                 0 0 0 0 0
                                 1 1 1 1 1
                                 0 0 0 0 1
                                 0 0 0 0 0
                                 1 1 1 1 0))
;;; Biais : une entrée supplémentaire toujours à 1, dont les poids sont appris.
;;; Sans biais, un perceptron ne peut pas apprendre le ET logique (la sortie
;;; ne peut s'allumer pour (1 1) sans s'allumer aussi pour (1 0) ou (0 1)).

(defvar *logique* '((0 0) (0 1) (1 0) (1 1)))

(make-perceptron et 2 1 :bias t)
(train-perceptron et *logique* '((0) (0) (0) (1)) :verbose t)
(mapcar #'(lambda (e) (run-perceptron et :in e)) *logique*)   ; => ((0) (0) (0) (1))
(net et)   ; la dernière ligne est le poids de biais, négatif : un seuil

;; le OU EXCLUSIF reste hors de portée, biais ou non : il faut une couche
;; cachée (cf. examples/mlp-test.lisp)
(make-perceptron ou-x 2 1 :bias t)
(setf (stop ou-x) 100)
(train-perceptron ou-x *logique* '((0) (1) (1) (0)) :verbose t)   ; interrompu
