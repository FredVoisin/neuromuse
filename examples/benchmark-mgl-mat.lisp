;;; Spike: benchmark mgl-mat (CPU-BLAS / CUDA) against neuromuse's
;;; existing list-based matrix/vector code.
;;;
;;; Standalone script, not wired into the :neuromuse or :neuromuse-test
;;; ASDF systems and touches no file under src/. Run from SLIME/SBCL
;;; after loading the system:
;;;
;;;   (asdf:load-system :neuromuse)
;;;   (load "examples/benchmark-mgl-mat.lisp")
;;;
;;; Compares, for a few net shapes (the two shapes actually used in
;;; examples/mlp-test.lisp and examples/mlp-test2.lisp, plus one much
;;; larger shape for contrast):
;;;   - neuromuse's own MULTIPLY-MATRIX-AND-VECTOR (list-of-lists, as
;;;     called by RUN-MLP: a fresh result list consed on every call),
;;;   - mgl-mat's GEMM! on the CPU/BLAS backend,
;;;   - mgl-mat's GEMM! on the CUDA/cuBLAS backend, if a CUDA device is
;;;     actually reachable (skipped with a message otherwise).

(ql:quickload :mgl-mat)

(in-package :neuromuse)

(defparameter *bench-sizes*
  '((:name "XOR (2-2-1, real usage, examples/mlp-test.lisp)"
     :in 2 :hidden 2 :out 1 :repeats 20000)
    (:name "Accel (6-4-1, real usage, examples/mlp-test2.lisp)"
     :in 6 :hidden 4 :out 1 :repeats 20000)
    (:name "Large contrast (200-100-50, not a real neuromuse net)"
     ;; MULTIPLY-MATRIX-AND-VECTOR indexes matrix rows with NTH, an
     ;; O(n) walk, so one list-based forward pass here is already
     ;; O(rows*cols^2) -- far fewer repeats needed to get a stable
     ;; timing than the tiny real-usage shapes above.
     :in 200 :hidden 100 :out 50 :repeats 200)))

(defparameter *bench-repeats* 20000
  "Default forward passes timed per backend/size when a size doesn't
specify its own :repeats. Small nets need many repeats for the timing
to rise above scheduler/GC noise.")

(defun bench-random-matrix (rows cols)
  (loop repeat rows collect (loop repeat cols collect (- (random 2.0) 1.0))))

(defun bench-random-vector (n)
  (loop repeat n collect (- (random 2.0) 1.0)))

(defun bench-elapsed-ms (start)
  (/ (* 1000.0 (- (get-internal-real-time) start))
     internal-time-units-per-second))

(defun bench-list-based (w1 w2 input repeats)
  "As RUN-MLP actually calls it: no :result buffer reuse, a fresh
list is consed by MULTIPLY-MATRIX-AND-VECTOR on every call."
  (let ((start (get-internal-real-time)))
    (dotimes (i repeats)
      (multiply-matrix-and-vector w2 (multiply-matrix-and-vector w1 input)))
    (bench-elapsed-ms start)))

(defun bench-mgl-mat (w1 w2 input repeats &key cuda)
  "Idiomatic mgl-mat usage: MAT buffers are allocated once and reused
across calls, as they would be for persistent GPU-resident weights."
  (mgl-mat:with-cuda* (:enabled cuda)
    (let* ((in-n (length input))
           (hid-n (length w1))
           (out-n (length w2))
           (m-w1 (mgl-mat:make-mat (list hid-n in-n) :initial-contents w1))
           (m-w2 (mgl-mat:make-mat (list out-n hid-n) :initial-contents w2))
           (m-in (mgl-mat:make-mat (list in-n 1)
                                    :initial-contents (mapcar #'list input)))
           (m-h (mgl-mat:make-mat (list hid-n 1)))
           (m-o (mgl-mat:make-mat (list out-n 1))))
      (let ((start (get-internal-real-time)))
        (dotimes (i repeats)
          (mgl-mat:gemm! 1 m-w1 m-in 0 m-h)
          (mgl-mat:gemm! 1 m-w2 m-h 0 m-o))
        (bench-elapsed-ms start)))))

(defun run-benchmark ()
  (format t "~&~%mgl-mat spike benchmark -- ~a forward passes per backend/size~%"
          *bench-repeats*)
  (format t "(total ms for all repeats; lower is better)~%")
  (dolist (spec *bench-sizes*)
    (destructuring-bind (&key name in hidden out (repeats *bench-repeats*)) spec
      (let* ((w1 (bench-random-matrix hidden in))
             (w2 (bench-random-matrix out hidden))
             (input (bench-random-vector in)))
        (format t "~%--- ~a (~a repeats) ---~%" name repeats)
        (format t "  list-based (neuromuse, as-is): ~,2f ms~%"
                (bench-list-based w1 w2 input repeats))
        (format t "  mgl-mat CPU/BLAS:               ~,2f ms~%"
                (bench-mgl-mat w1 w2 input repeats :cuda nil))
        (cond
          ((not (mgl-mat:cuda-available-p))
           (format t "  mgl-mat CUDA/cuBLAS:            skipped (no CUDA device reachable)~%"))
          (t
           (handler-case
               (format t "  mgl-mat CUDA/cuBLAS:            ~,2f ms~%"
                       (bench-mgl-mat w1 w2 input repeats :cuda t))
             (error (e)
               (format t "  mgl-mat CUDA/cuBLAS:            failed: ~a~%" e))))))))
  (values))

(run-benchmark)
