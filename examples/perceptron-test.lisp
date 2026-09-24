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
