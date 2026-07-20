;;; rosom-sine-demo.lisp
;;;
;;; Demo/test of a rosom (recurrent oscillatory SOM, src/rosom.lisp) learning
;;; a periodic signal: a sine wave, sampled 64 times per period and encoded as
;;; a length-64 one-hot bit vector per sample (a single 1 at the bin matching
;;; the sample's amplitude, quantized into 64 levels). Stacking consecutive
;;; vectors traces the sine curve through the position of the 1 bit -- see
;;; the printed samples below.
;;;
;;; A rosom's job is to phase-lock its internal oscillator (its
;;; INPUT-CONTEXT/context net) to a periodic input. We test that here by
;;; training on several repeated periods, then replaying the same period
;;; twice with learning frozen and checking how consistently each phase step
;;; maps to the same winning neuron across the two replays -- a stable,
;;; repeatable phase->neuron mapping is what "prediction" means for this
;;; architecture: the network has learned the cycle's structure, not just
;;; reacted to individual samples.
;;;
;;; Run after (asdf:load-system :neuromuse) and (in-package :neuromuse):
;;;   (load "examples/rosom-sine-demo.lisp")

;;; ---------------------------------------------------------------------
;;; 1. Sine -> one-hot bit vector encoding
;;; ---------------------------------------------------------------------

(defparameter *n-bits* 64
  "Length of each input vector = number of amplitude bins.")

(defparameter *n-samples* 64
  "Time samples per sine period.")

(defun sine-bitvector (i n-samples n-bits &optional (periods 1))
  "A length N-BITS bit vector, all zero except one bit set: the amplitude of
sin(2*pi*i*periods/n-samples), scaled from [-1,1] into a bin in [0,n-bits-1]."
  (let* ((phase (/ (* 2 pi i periods) n-samples))
         (amplitude (sin (float phase 1.0d0)))
         (bin (round (* (/ (1+ amplitude) 2) (1- n-bits))))
         (v (make-array n-bits :element-type 'bit :initial-element 0)))
    (setf (aref v bin) 1)
    v))

(defun print-bitvector (v &optional (stream t))
  (map nil (lambda (b) (princ (if (= b 1) #\1 #\.) stream)) v))

(defun one-period (&optional (n-samples *n-samples*) (n-bits *n-bits*))
  (loop for i from 0 below n-samples collect (sine-bitvector i n-samples n-bits)))

(format t "~&First 12 samples of one sine period (n-samples=~a, n-bits=~a):~%~%"
        *n-samples* *n-bits*)
(dotimes (i 12)
  (let ((v (sine-bitvector i *n-samples* *n-bits*)))
    (format t "i=~2,'0D  sin=~6,3F  bit=~2,'0D  " i (sin (/ (* 2 pi i) *n-samples*)) (position 1 v))
    (print-bitvector v)
    (terpri)))

;;; ---------------------------------------------------------------------
;;; 2. Build the rosom
;;; ---------------------------------------------------------------------

(defparameter *rosom-size* 36
  "Number of SOM/context neurons (a perfect square: 2D/VOISINS lay them out
on a sqrt(*rosom-size*) x sqrt(*rosom-size*) grid).")

(when (boundp 'sine-rosom) (makunbound 'sine-rosom))
(make-instance 'rosom :name 'sine-rosom)
(init sine-rosom :input *n-bits* :size *rosom-size*)

(format t "~&~%Built ~S: ~a content/context neurons, ~a-bit input.~%"
        sine-rosom *rosom-size* *n-bits*)

;;; ---------------------------------------------------------------------
;;; 3. Train on repeated periods (learning + phase synchronization on)
;;; ---------------------------------------------------------------------

(defparameter *training-periods* 12)
(defparameter *radius* 2)
(defparameter *learn-rate* .1)
(defparameter *entrainement-rate* .1)

(defun train-rosom-on-sequence (rosom sequence &key (radius *radius*)
                                 (learn *learn-rate*)
                                 (entrainement-rate *entrainement-rate*)
                                 (temp-som 0) (temp-rosom 0))
  "One pass over SEQUENCE, learning on. Returns the list of winners (one per
step), and suppresses ROSOM-LEARN's own per-step printing so training stays
readable."
  (let ((null-out (make-broadcast-stream)))
    (loop for v in sequence
          collect
          (progn
            (setf (input rosom) v)
            (let ((*standard-output* null-out))
              (rosom-learn v rosom radius learn entrainement-rate temp-som temp-rosom
                           :verbose nil))
            (car (find-winner rosom))))))

(format t "~&Training on ~a periods (~a steps each, ~a steps total)...~%"
        *training-periods* *n-samples* (* *training-periods* *n-samples*))

(let ((period (one-period)))
  (dotimes (p *training-periods*)
    (train-rosom-on-sequence sine-rosom period)
    (format t "  period ~2D/~a done (epoch ~a)~%" (1+ p) *training-periods* (epoch sine-rosom))))

;;; ---------------------------------------------------------------------
;;; 4. Test: replay two periods with learning frozen, compare winners
;;; ---------------------------------------------------------------------

(format t "~&~%Replaying 2 periods with learning frozen (radius=0, learn=0, ~
entrainement-rate=0)...~%")

(let* ((period (one-period))
       (replay-a (train-rosom-on-sequence sine-rosom period
                                           :radius 0 :learn 0 :entrainement-rate 0))
       (replay-b (train-rosom-on-sequence sine-rosom period
                                           :radius 0 :learn 0 :entrainement-rate 0)))
  (format t "~%phase  sin      winner(A) winner(B)  match?~%")
  (loop for i from 0 below *n-samples*
        for wa in replay-a
        for wb in replay-b
        when (< i 16)
          do (format t "~4D  ~6,3F   ~4D      ~4D       ~a~%"
                      i (sin (/ (* 2 pi i) *n-samples*)) wa wb (if (= wa wb) "yes" "NO")))
  (format t "  ... (~a more phase steps)~%" (- *n-samples* 16))
  (let* ((matches (count t (mapcar #'= replay-a replay-b)))
         (accuracy (/ matches (float *n-samples*))))
    (format t "~%Phase-locking consistency (replay A vs. replay B, same phase): ~
~a/~a = ~,1F%~%" matches *n-samples* (* 100 accuracy))
    (format t "~a~%"
            (if (> accuracy .8)
                "=> the rosom has learned a stable, repeatable phase->neuron mapping: it reliably recognizes where in the sine cycle it is, cycle after cycle."
                "=> the phase->neuron mapping is not yet stable across replays -- try more *training-periods*, a different *radius*/*learn-rate*, or a larger *rosom-size*."))))
