(defpackage neuromuse-test
  (:use :cl
        :neuromuse
        :prove))
(in-package :neuromuse-test)

;; NOTE: To run this test file, execute `(asdf:test-system :neuromuse)' in your Lisp.

(plan nil)

(subtest "maths: transfer functions"
  (is (logistic 0) 0.5)
  (is (binary 1 :thresh 0) 1)
  (is (binary -1 :thresh 0) 0)
  (is (sign 1 :thresh 0) 1)
  (is (sign -1 :thresh 0) -1)
  (is (linear 2 :slope 3 :thresh 1) 5))

(subtest "maths: distance & error"
  (is (euclidian (list 0 0) (list 3 4)) 5.0)
  (is (euclidian-fast (list 0 0) (list 3 4)) 25)
  (is (neuromuse-distance (list 3 4) (list 0 0)) 5.0 :test #'=
      "poids nuls : activation nulle, distance = norme de l'entree")
  (is (neuromuse-distance (list 3 4) (list 1 1)) 0.0 :test #'=
      "poids a 1 : distance nulle quelle que soit l'entree")
  (is (chebyshev (list 0 0) (list 3 4)) 4 "Tchebychev : plus grand ecart par axe")
  (is (manhattan (list 0 0) (list 3 4)) 7 "Manhattan : somme des ecarts par axe")
  (is (check-error (vector 1.0 2.0) (vector 1.0 2.0) 0.01) 0
      "identical vectors have zero error")
  (is (check-error (vector 0.0 0.0) (vector 1.0 1.0) 0.01) 2.0
      "error sums per-element absolute differences above tolerance")
  (is (compare-vectors (list 1 2 3) (list 1 2 3)) (list 0 0 0))
  (is (compare-vectors (list 1 2 3) (list 1 9 3)) (list 0 1 0)))

(subtest "maths: clip, scale, normalize, noise"
  (is (clip 5) 1)
  (is (clip -1) 0)
  (is (clip 0.5) 0.5)
  (is (scale 5 0 10 0 1) 0.5)
  (is (normalize (list 0 5 10)) (list 0.0 0.5 1.0) :test #'equal)
  (is (noise 5.0 0.0) 5.0 "zero noise leaves a number unchanged")
  (is (noise 5 nil) 5 "nil p is a no-op"))

(subtest "maths: SOM topology helpers"
  (let ((coords (2d 4 25)))
    (is (d2 (first coords) (second coords) 25) 4
        "2d/d2 round-trip on a perfect-square grid"))
  (is (find (list 2) (voisins (list 2) 1 5) :test #'equal) (list 2)
      "voisins includes the origin position itself" :test #'equal)
  (is (length (voisins (list 2) 1 5)) 3
      "voisins with radius 1 covers position, -1 and +1"))

(subtest "maths: matrix/vector algebra"
  (is (multiply-2-vectors (list 1 2 3) (list 4 5 6)) (list 4 10 18) :test #'equal)
  (is (substract-2-vectors (list 5 5 5) (list 1 2 3)) (list 4 3 2) :test #'equal)
  (is (hadamar-product (list (list 1 2) (list 3 4)) (list (list 1 1) (list 1 1)))
      (list (list 1 2) (list 3 4)) :test #'equal))

(subtest "misc: string/wire formatting"
  (is (split "a b  c") (list "a" "b" "c") :test #'equal)
  (is (st2list "1 2 3") (list 1 2 3) :test #'equal))

;;; à ajouter dans tests/neuromuse.lisp (après les subtests existants, avant (finalize))

(subtest "perceptron"
  (let ((p (make-instance 'perceptron :in-size 3 :out-size 2
                          :net (list (list 1 -1) (list 1 -1) (list -3 1)))))
    (is (perceptron-activity p :in '(1 1 0)) '(2 -2)
        "activité = somme pondérée des entrées, par cellule de sortie")
    (is (run-perceptron p :in '(1 1 0)) '(1 0)
        "sortie binaire : 1 si activité > 0")
    (is (run-perceptron p :in #(1 0 1)) '(0 0)
        "les stimuli peuvent être des vecteurs (comme en 1999)"))
  ;; exemple de perceptron.lisp (IRCAM, mars 1999) : 4 rétines de 30 cellules,
  ;; linéairement séparables -> convergence garantie (théorème du perceptron)
  (let* ((retines (list (loop for i below 30 collect (if (zerop (mod i 5)) 1 0))
                        (loop for i below 30 collect (if (= 2 (mod i 5)) 1 0))
                        (make-list 30 :initial-element 0)
                        (make-list 30 :initial-element 1)))
         (buts '((1 0 0) (0 1 0) (0 0 0) (0 0 1)))
         (p (make-instance 'perceptron :in-size 30 :out-size 3
                           :net (init-perceptron-net 30 3))))
    (train-perceptron p retines buts)
    (is (second (last-stop p)) 'completed "l'apprentissage converge")
    (is (mapcar #'(lambda (r) (run-perceptron p :in r)) retines) buts
        "chaque rétine donne sa sortie apprise"))
  ;; biais : le ET logique n'est apprenable qu'avec un biais ; le OU EXCLUSIF
  ;; ne l'est jamais (non linéairement séparable, Minsky & Papert 1969)
  (let ((entrees '((0 0) (0 1) (1 0) (1 1)))
        (p (make-instance 'perceptron :in-size 2 :out-size 1 :bias t
                          :net (list (list 1) (list 1) (list -1.5)))))
    (is (length (net p)) 3 "le biais ajoute une ligne au réseau")
    (is (mapcar #'(lambda (e) (run-perceptron p :in e)) entrees)
        '((0) (0) (0) (1))
        "le biais est une entrée à 1 ajoutée au stimulus")
    (flet ((apprend (buts bias)
             (let ((q (make-instance 'perceptron :in-size 2 :out-size 1 :bias bias
                                     :net (init-perceptron-net 2 1 :bias bias))))
               (train-perceptron q entrees buts)
               (second (last-stop q)))))
      (is (apprend '((0) (0) (0) (1)) t) 'completed "ET appris avec biais")
      (is (apprend '((0) (0) (0) (1)) nil) 'interrupted "ET inapprenable sans biais")
      (is (apprend '((0) (1) (1) (0)) t) 'interrupted "OU exclusif inapprenable"))))

(subtest "mlp: construction and shape"
  (make-mlp mlp-shape-test 3 2 4)
  (is (in-size mlp-shape-test) 3)
  (is (out-size mlp-shape-test) 2)
  (is (hidden-size mlp-shape-test) (list 4) :test #'equal)
  (is (length (net mlp-shape-test)) 2
      "one weight matrix input->hidden, one hidden->output")
  (setf (input mlp-shape-test) (list 0.1 0.2 0.3))
  (let ((out (run-mlp mlp-shape-test)))
    (is (length out) 2 "run-mlp produces one output per out-size")))

(subtest "mlp: backpropagation reduces XOR error"
  ;; deterministic weight init and pattern order so this is not flaky
  (setf *random-state* (sb-ext:seed-random-state 42))
  (make-mlp mlp-xor-test 2 1 2)
  (setf (learn-fact mlp-xor-test) 0.4
        (threshold mlp-xor-test) 0.01)
  (setf (input mlp-xor-test) (list 0 0) (goal mlp-xor-test) (list 0))
  (learn mlp-xor-test)  ;; renvoie mlp-xor-test ; l'erreur du pas est dans current-error
  (let ((e0 (current-error mlp-xor-test)))
    (dotimes (i 2000)
      (dolist (pattern (list (list (list 0 0) (list 0))
                              (list (list 1 0) (list 1))
                              (list (list 0 1) (list 1))
                              (list (list 1 1) (list 0))))
        (setf (input mlp-xor-test) (first pattern)
              (goal mlp-xor-test) (second pattern))
        (learn mlp-xor-test)))
    (setf (input mlp-xor-test) (list 0 0) (goal mlp-xor-test) (list 0))
    (learn mlp-xor-test)
    (let ((e1 (current-error mlp-xor-test)))
      (ok (< e1 e0) "error on (0 0) after training is lower than before training"))))

(subtest "rmlp: construction and run"
  (make-rmlp rmlp-test 2 1 0 3)
  (is (recurrent-layer rmlp-test) 0)
  (setf (input rmlp-test) (list 0.1 0.2))
  (let ((out (run-mlp rmlp-test)))
    (is (length out) 1)))

(subtest "som: construction, activation, winner"
  (let ((s (make-instance 'som :name 'som-shape-test)))
    (init s :size 4 :input 3)
    (is (length (net s)) 4 "net has one neuron per grid cell")
    (setf (input s) (coerce (list 0.1 0.5 0.9) 'vector))
    (let ((a (activation s)))
      (is (length a) 4 "one activation vector per neuron")
      (is (length (first a)) 3 "each activation vector has one value per input"))
    (let ((winner (find-winner s)))
      (is (length winner) 2 "find-winner returns (neuron distance)")
      (is (type-of (first winner)) 'neuron :test #'eq))))

(subtest "som: distance euclidienne par defaut, distance neuromuse en option"
  (let ((s (make-instance 'som :name 'som-metric-test :size 4 :input 3)))
    (is (distance s) 'euclidian "distance euclidienne d(x, w) par defaut")
    (is (error-scaling s) :normalized "pilotage par l'erreur normalise par defaut")
    ;; le neurone 2 recoit exactement l'entree comme poids
    (loop for synapse in (net (nth 2 (net s)))
          for v in '(0.1 0.5 0.9)
          do (setf (cadr synapse) v))
    (setf (input s) (coerce (list 0.1 0.5 0.9) 'vector))
    (let ((w (find-winner s)))
      (is (car (id (first w))) 2 "le neurone dont les poids valent l'entree gagne")
      (is (second w) 0.0 "a distance nulle"))
    (is (output (nth 2 (net s))) (list 0.1 0.5 0.9)
        "(output neurone) = ce qui a ete compare a l'entree, ici les poids")
    ;; reglage historique : distance a l'activation x*w
    (setf (distance s) 'neuromuse-distance)
    (let* ((w (find-winner s))
           (k (car (id (first w)))))
      (is (second w) (euclidian (input s) (activation s :n k)) :test #'=
          "NEUROMUSE-DISTANCE mesure d(x, x*w), comme avant"))))

(subtest "som: learn, voisinage normalise, erreur nulle"
  (let ((s (make-instance 'som :name 'som-learn-test :size 9 :input 3)))
    (setf (learn-fact s) 0.5
          (radius s) 1
          (input s) (coerce (list 0.2 0.4 0.6) 'vector))
    (let ((before (second (find-winner s))))
      (learn s)
      (ok (< (second (find-winner s)) before) "le gagnant se rapproche de l'entree")
      (is (max-error s) before "MAX-ERROR retient l'erreur du gagnant"))
    (is (neighbourhood-width s (max-error s)) 1 :test #'=
        "a erreur maximale, la largeur vaut RADIUS")
    (is (neighbourhood-width s (/ (max-error s) 2)) 0.5 :test #'=
        "a mi-erreur, la moitie de RADIUS")
    ;; entree egale aux poids d'un neurone : erreur nulle, LEARN ne doit pas echouer
    (setf (input s) (coerce (mapcar #'cadr (net (nth 4 (net s)))) 'vector))
    (ok (learn s) "LEARN passe avec une erreur nulle")
    (setf (distance s) 'neuromuse-distance
          (error-scaling s) :raw)
    (ok (learn s) "le reglage historique (NEUROMUSE-DISTANCE, :RAW) tourne toujours")))

(subtest "som: distance sur la grille"
  (let ((s (make-instance 'som :name 'som-grid-test :size 9 :input 3)))
    (is (grid-distance s) 'euclidian "grille euclidienne par defaut")
    ;; carte 3x3, gagnant au centre (1 1), rayon 1, largeur pleine : avec
    ;; Tchebychev, coin (0 0) et bord (1 0) sont a la meme distance, donc
    ;; recoivent la meme correction ; en euclidien, le coin en recoit moins
    (flet ((correction (fn voisin)
             (neighbourhood-correction 1.0 1 (funcall fn voisin (list 1 1)))))
      (is (correction 'chebyshev (list 0 0)) (correction 'chebyshev (list 1 0))
          "Tchebychev : coin et bord a egalite" :test #'=)
      (ok (< (correction 'euclidian (list 0 0)) (correction 'euclidian (list 1 0)))
          "euclidien : le coin est moins corrige que le bord"))
    (setf (grid-distance s) 'chebyshev
          (learn-fact s) 0.5
          (radius s) 1
          (input s) (coerce (list 0.2 0.4 0.6) 'vector))
    (ok (learn s) "LEARN tourne avec une grille de Tchebychev")))

(subtest "rosom: construction and activation"
  (let ((r (make-instance 'rosom :name 'rosom-shape-test)))
    (init r :size 4 :input 3)
    (is (length (first (net r))) 4 "content net has one neuron per cell")
    (is (length (second (net r))) 4 "context net has one neuron per cell")
    (setf (input r) (coerce (list 0.1 0.5 0.9) 'vector))
    (is (length (activation r)) 4 "one activation value per content neuron")))

(finalize)
