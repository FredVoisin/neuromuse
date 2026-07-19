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
  (let ((e0 (backpropagate mlp-xor-test)))
    (dotimes (i 2000)
      (dolist (pattern (list (list (list 0 0) (list 0))
                              (list (list 1 0) (list 1))
                              (list (list 0 1) (list 1))
                              (list (list 1 1) (list 0))))
        (setf (input mlp-xor-test) (first pattern)
              (goal mlp-xor-test) (second pattern))
        (backpropagate mlp-xor-test)))
    (setf (input mlp-xor-test) (list 0 0) (goal mlp-xor-test) (list 0))
    (let ((e1 (backpropagate mlp-xor-test)))
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

(subtest "rosom: construction and activation"
  (let ((r (make-instance 'rosom :name 'rosom-shape-test)))
    (init r :size 4 :input 3)
    (is (length (first (net r))) 4 "content net has one neuron per cell")
    (is (length (second (net r))) 4 "context net has one neuron per cell")
    (setf (input r) (coerce (list 0.1 0.5 0.9) 'vector))
    (is (length (activation r)) 4 "one activation value per content neuron")))

(finalize)
