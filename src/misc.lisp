;;;;; MISC (generic Lisp utilities and UDP wire-format helpers)

(in-package :neuromuse)

;(format t "misc.lisp...~%")

;;********************************************************************************
;;                              Lisp Misc                                        *
;;********************************************************************************

(defun make-listarray (dim0 dim1 &optional initial-element)
  (let (array)
    (dotimes (i dim0 array)
      (push
       (loop for j from 0 to (1- dim1)
	  collect (if initial-element (funcall initial-element) nil))
       array))))

;(make-listarray 2 3)
;(make-listarray 2 4 #'(lambda () (random 1.0)))3

(defun ldlp (l)
  "Test if every element of l is a list of lists."
  (not (member nil (mapcar #'listp l))))

(defun ldvp (l)
  "Test if every element of l is a list of vectors"
  (not (member nil (mapcar #'vectorp l))))

(defun get-time ()
  (multiple-value-bind (second minute hour date month year
                               day-of-week daylight-saving-time-p time-zone)
                       (get-decoded-time)
    (declare (ignore day-of-week daylight-saving-time-p))
    (list year month date hour minute second time-zone)))

(defgeneric round1 (value &optional dec)
  (:documentation
   "Arrondi 'value' (nombre, liste ou vecteur, au nombre de decimales 'dec'."))

(defmethod round1 ((value number) &optional (dec 0))
  (let* ((int (floor (* value (expt 10 dec))))
         (f (- (* value (expt 10 dec)) int)))
    (float (/ (+ int (round f)) (expt 10 dec)))))

(defmethod round1 ((value list) &optional (dec 0))
  (mapcar #'(lambda (x) (round1 x dec)) value))

(defmethod round1 ((value vector) &optional (dec 0))
  (dotimes (n (length value) value)
    (setf (elt value n) (round1 (elt value n) dec))))

(defun test-t (list test)
  (if (member test list :test #'equalp)
    list nil))

;; make-new-symbol is defined in neuromuse-main.lisp (loaded before this
;; file); it used to be duplicated here too, which just produced an SBCL
;; "redefining" warning on every load for an identical definition.

;; formatage

(defun st2v (string)
  (coerce (mapcar #'read-from-string (split string)) 'vector))

(defun st2list (string)
  (mapcar #'read-from-string (split string)))

(defun vector2string (vector)
  (apply #'concatenatef
	 (loop for i from 0 to (1- (length vector))
	       collect (format nil "~S" (aref vector i)))))

(defun buf2string (buf)
  (setf buf (subseq buf 0 (- (position 0 buf) 2)))
  (let ((st (make-string (length buf))))
    (dotimes (n (length buf))
      (setf (elt st n) (code-char (elt buf n))))
    st))

(defun list2string (list)
  (apply #'concatenatef
	  (loop for i from 0 to (1- (length list))
	       collect (format nil "~S" (nth i list)))))

(defun v2st (vector) (vector2string vector))

(defun concatenatef (&rest args)
  (let ((st ""))
    (loop for arg in args
	  do
	  (setf st (concatenate 'string st " " arg)))
    (subseq st 1)))

;;; This courtesy of Pierre Mai in comp.lang.lisp 08 Jan 1999 00:51:44 +0100
;;; Message-ID: <87lnjebq0f.fsf@orion.dent.isdn.cs.tu-berlin.de>

(defun split (string &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
The whitespace is elided from the result.  The whole string will be
split, unless `max' is a non-negative integer, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (loop for start = (position-if-not #'is-ws string)
          then (position-if-not #'is-ws string :start index)
          for index = (and start
                           (if (and max (= (1+ word-count) max))
                               nil
                             (position-if #'is-ws string :start start)))
          while start
          collect (subseq string start index)
          count 1 into word-count
          while index)))

; eof
