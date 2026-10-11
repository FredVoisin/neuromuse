;;;;; MATHS

(in-package :neuromuse)

;(format t "maths.lisp...~%")

(defun rand (x)
  (if (zerop x) 0 (random x)))

;;; Fonctions de transfert

(defgeneric binary (x &key thresh temp learn slope)
  (:documentation "valeur binaire de x"))

(defmethod binary ((x number) &key (thresh 0) (temp nil) (learn nil) (slope nil))
  (declare (ignore learn temp slope))
  (if (<= x thresh)
    0 1))

(defmethod binary ((x list) &key (thresh 0) (temp nil) (learn nil) (slope nil))
  (declare (ignore learn temp slope))
  (mapcar #'(lambda (a) (binary a :thresh thresh)) x))

(defmethod binary ((x vector) &key (thresh 0) (temp nil) (learn nil) (slope nil))
  (declare (ignore learn temp slope))
  (dotimes (i (length x) x)
    (setf (aref x i) (binary (aref x i) :thresh thresh))))

(defgeneric sign (x &key thresh temp learn slope)
  (:documentation "signe de x"))

(defmethod sign ((x number) &key (thresh 0) (temp nil) (learn nil) (slope nil))
  (declare (ignore learn temp slope))
  (if (<= x thresh)
    -1 1))

(defmethod sign ((x list) &key (thresh 0) (temp nil) (learn nil) (slope nil))
  (declare (ignore learn temp slope))
  (mapcar #'(lambda (a) (sign a :thresh thresh)) x))

(defmethod sign ((x vector) &key (thresh 0) (temp nil) (learn nil) (slope nil))
  (declare (ignore learn temp slope))
  (dotimes (i (length x) x)
    (setf (aref x i) (sign (aref x i) :thresh thresh))))

(defun temp-value (temp)
  (if temp (if (< (random 1.0) temp) t nil) nil))

(defgeneric logistic (x &key thresh temp learn slope)
  (:documentation "Fonction 'logisitic' de x, ie. sigmoide a probabilite gaussienne."))

(defmethod logistic ((x number) &key (thresh 0) (temp nil) (learn nil) (slope 1))
  (declare (ignore learn))
  (if temp
    (let ((temp-v (temp-value temp)))
      (if temp-v
        (random 1.0)
        (/ 1 (+ 1 (exp (- (/ (- x thresh) slope)))))))
    (/ 1 (+ 1 (exp (- (/ (- x thresh) slope)))))))

(defmethod logistic ((x list) &key (thresh 0) (temp 0) (learn nil) (slope 1))
  (mapcar #'(lambda (a) (logistic a :slope slope :thresh thresh :temp temp :learn learn)) x))

(defmethod logistic ((x vector) &key (thresh 0) (temp 0) (learn nil) (slope 1))
  (dotimes (i (length x) x)
    (setf (aref x i) (logistic (aref x i) :slope slope :thresh thresh :temp temp :learn learn))))

(defgeneric sigmoide (x &key thresh temp learn slope)
  (:documentation "Fonction sigmoide de x"))

(defmethod sigmoide ((x number) &key (thresh 0) (temp nil) (learn nil) (slope 1))
  (declare (ignore learn))
  (if (temp-value temp)
      (random 1.0)
      (/ (exp (/ (- x thresh) slope))
	 (+ (exp (/ (- x thresh) slope)) (exp (- (/ (- x thresh) slope)))))))

(defmethod sigmoide ((x list) &key (thresh 0) (temp nil) (learn nil) (slope 1))
  (mapcar #'(lambda (a) (sigmoide a :slope slope :thresh thresh :temp temp :learn learn)) x))

(defmethod sigmoide ((x vector) &key (thresh 0) (temp nil) (learn nil) (slope 1))
  (dotimes (i (length x) x)
    (setf (aref x i) (sigmoide (aref x i) :slope slope :thresh thresh :temp temp :learn learn))))

(defgeneric linear (x &key thresh temp learn slope)
  (:documentation "Valeur lineaire de x = slope . x - thresh."))

(defmethod linear ((x number) &key (thresh 0) (temp nil) (learn nil) (slope 1))
  (declare (ignore learn))
  (if (temp-value temp)
      (random 1.0)
      (- (* slope x) thresh)))

(defmethod linear ((x list) &key (thresh 0) (temp nil) (learn nil) (slope 1))
  (mapcar #'(lambda (a) (linear a :thresh thresh :slope slope :temp temp :learn learn)) x))

(defmethod linear ((x vector) &key (thresh 0) (temp nil) (learn nil) (slope 1))
  (declare (ignore learn))
  (dotimes (i (length x) x)
    (setf (aref x i) (linear (aref x i) :thresh thresh :slope slope))))

(defun boltzmann-distr (x temp &optional (thresh 0))
  (when (= 0 temp) (setf temp 1E-32))
  (/ 1 (+ 1 (exp (/ (- (- x thresh)) temp)))))

(defgeneric boltzmann (x &key thresh temp learn slope)
  (:documentation "Distribution de Boltzmann de probabilite de x."))

(defmethod boltzmann ((x number) &key (thresh 0) (temp 1.0) (learn nil) (slope 1))
  (declare (ignore learn slope))
  (let ((rho-jt (random 1.0))
        (proba-j (boltzmann-distr (- x thresh) temp)))
    (if (<= proba-j rho-jt)
      0 1)))

(defmethod boltzmann ((x vector) &key (thresh 0) (temp 1.0) (learn nil) (slope 1))
  (dotimes (i (length x) x)
    (setf (aref x i) (boltzmann (aref x i) :temp temp :thresh thresh :learn learn :slope slope))))

(defmethod boltzmann ((x list) &key (thresh 0) (temp 1.0) (learn nil) (slope 1))
  (mapcar #'(lambda (a) (boltzmann a :temp temp :thresh thresh :learn learn :slope slope)) x))

(defgeneric softmax (x)
  (:documentation "Distribution de probabilite de X (liste ou vecteur) :
exp(xi) / somme(exp(xj)) -- le maximum de X est soustrait avant l'exponentielle
pour la stabilite numerique (SOFTMAX est invariant par translation, le
resultat est le meme, mais exp ne deborde jamais). Pure (X n'est jamais mute,
contrairement a LINEAR/BOLTZMANN sur vecteur) -- utilisee par
fred/transformer.lisp (attention multi-tetes), pas (encore) par une
architecture de src/."))

(defmethod softmax ((x list))
  (let* ((m (reduce #'max x))
         (e (mapcar #'(lambda (xi) (exp (- xi m))) x))
         (s (apply #'+ e)))
    (mapcar #'(lambda (ei) (/ ei s)) e)))

(defmethod softmax ((x vector))
  (coerce (softmax (coerce x 'list)) 'vector))

;;; apprentissage : Widrow-Hoff rule = delta rule
(defun widrow-hoff (wij tj oj xi l)
  (+ wij (* l (- tj oj) xi)))

;obsolete
(defun cell-output-boltz (cell-activity temperature)
  (let ((rho-jt (random 1.0))
        (proba-j (boltzmann-distr
                  cell-activity
                  temperature)))
    (if (<= proba-j rho-jt)
      0 1)))

(defgeneric check-error (v1 v2 error)
  (:documentation "fonction bizarre et surement utile. en relation avec MLP"))

(defmethod check-error ((v1 vector) (v2 vector) (error number))
  (let ((diff '()))
    (dotimes (n (length v1))
      (let ((e (abs (- (aref v1 n) (aref v2 n)))))
	(if (< e error)
	    (push 0 diff)
	    (push e diff))))
    (values (apply #'+ diff))))

(defmethod check-error ((v1 list) (v2 list) (error number))
  (let ((diff '()))
    (dotimes (n (length v1))
      (let ((e (abs (- (nth n v1) (nth n v2)))))
	(if (< e error)
	    (push 0 diff)
	    (push e diff))))
    (values (apply #'+ diff))))

;(check-error #(0 .3 9 4) #(.1 .3 8.8 4) 0.2)

(defgeneric declip (x &optional thresh)
  (:documentation "Randomises x lorsqu'il depasse le seuil 'thresh'."))

(defmethod declip ((x number) &optional thresh)
  (when (not thresh) (setf thresh 100))
  (if (> (abs x) thresh)
    (setf x (- (random 2.0) 1))
    x))

(defmethod declip ((x vector) &optional thresh)
  (coerce (mapcar #'(lambda (x) (declip x thresh)) (coerce x 'list)) 'vector))

(defmethod declip ((x list) &optional thresh)
  (mapcar #'(lambda (x) (declip x thresh))  x))

(defgeneric clip (value &key min max)
  (:documentation
   "Clips values between bounds :min and :max (default 0 1)."))

(defmethod clip ((value number) &key (min 0) (max 1))
  (if (< value min)
    min (if (> value max) max value)))

(defmethod clip ((value list) &key (min 0) (max 1))
  (mapcar #'(lambda (x) (clip x :min min :max max)) value))

(defmethod clip ((value vector) &key (min 0) (max 1))
  (coerce (clip (coerce value 'list) :min min :max max) 'vector))

(defmethod clip ((value array) &key (min 0) (max 1))
  (let ((clipped-matrix (make-array (list (array-dimension value 0)
                                          (array-dimension value 1)))))
    (dotimes (i (array-dimension value 0) clipped-matrix)
      (dotimes (j (array-dimension value 1))
        (setf (aref clipped-matrix i j) (clip (aref value i j) :min min :max max))))))

(defgeneric noise (data p)
  (:documentation
   "Noise : random variation into a range from plus to minus p for each value of data;
    p must be included between 0 and 1. When data is of type ann, returns a noise variation
    of the network values of the ann (i.e. : (noise (net ann) p))."))

(defmethod noise ((data t) (p null))
  (values data))

(defmethod noise ((data number) (p float))
  (if (zerop p)
      data
      (let ((sig (* p (- (random 2.0) 1))))
	(+ data sig))))

(defmethod noise ((data list) (p float))
  (if (zerop p) data (mapcar #'(lambda (x) (noise x p)) data)))

(defmethod noise ((data vector) (p float))
  (if (zerop p) data (coerce (noise (coerce data 'list) p) 'vector)))

(defmethod noise ((data neuron) (p float))
  (if (zerop p) (net data) (noise (net data) p)))

(defmethod noise ((data ann) (p number))
  (if (zerop p) (net data) (noise (net data) p)))

(defgeneric inhibit-synaps (matrix from &optional to &key factor)
  (:documentation "mettre a jour pour som'."))

(defmethod inhibit-synaps ((matrix array) (from null) &optional (to nil) &key (factor 0))
  (declare (ignore from to factor))
  matrix)

(defmethod inhibit-synaps ((matrix array) (from integer) &optional (to nil) &key (factor 0))
  (when (numberp to) (setf to (list to)))
  (let ((inhibited (make-array (list (array-dimension matrix 0)
                                     (array-dimension matrix 1)))))
    (dotimes (i (array-dimension matrix 1) inhibited)
      (dotimes (j (array-dimension matrix 0))
        (if (and (= i from) (or (when to (member j to)) (not to)))
          (setf (aref inhibited j i) (* factor (aref matrix j i)))
          (setf (aref inhibited j i) (aref matrix j i)))))))

(defmethod inhibit-synaps ((matrix array) (from list) &optional (to nil) &key (factor 0))
  (let ((inhibited (make-array (list (array-dimension matrix 0)
                                     (array-dimension matrix 1)))))
    (cond ((not to)
           (dotimes (i (array-dimension matrix 1) inhibited)
             (dotimes (j (array-dimension matrix 0))
               (if (member i from)
                 (setf (aref inhibited j i) (* factor (aref matrix j i)))
                 (setf (aref inhibited j i) (aref matrix j i))))))
          ((integerp to)
           (dotimes (i (array-dimension matrix 1) inhibited)
             (dotimes (j (array-dimension matrix 0))
               (if (and (member i from) (= j to))
                 (setf (aref inhibited j i) (* factor (aref matrix j i)))
                 (setf (aref inhibited j i) (aref matrix j i))))))
          (t (let ((map (mapcar #'(lambda (x y) (list x y)) from to)))
               (dotimes (i (array-dimension matrix 1) inhibited)
                 (dotimes (j (array-dimension matrix 0))
                   (if (find (list i j) map :test #'equal)
		       (setf (aref inhibited j i) (* factor (aref matrix j i)))
		       (setf (aref inhibited j i) (aref matrix j i))))))))))

(defmethod inhibit-synaps ((matrix array) (from null) &optional (to nil) &key (factor 0))
  (let ((inhibited (make-array (list (array-dimension matrix 0)
                                     (array-dimension matrix 1)))))
    (cond ((not to)
           matrix)
          ((integerp to)
           (dotimes (i (array-dimension matrix 1) inhibited)
             (dotimes (j (array-dimension matrix 0))
               (if (= j to)
                 (setf (aref inhibited j i) (* factor (aref matrix j i)))
                 (setf (aref inhibited j i) (aref matrix j i))))))
          (t (dotimes (i (array-dimension matrix 1) inhibited)
               (dotimes (j (array-dimension matrix 0))
                 (if (member j to)
		     (setf (aref inhibited j i) (* factor (aref matrix j i)))
		     (setf (aref inhibited j i) (aref matrix j i)))))))))

(defun match-i (w list)
  "prend dans les synapses inhibees de net qui match avec le rang de la couche w,
et formate pour <inhibit-synaps>."
  (let ((temp (mapcar #'(lambda (x) (list (cadar x) (cadadr x)))
                      (remove-if-not #'(lambda (a) (= a w)) list :key #'caar))))
    (list (mapcar #'car temp) (mapcar #'cadr temp))))

(defgeneric sort* (x predicat &key key)
  (:documentation "Randomises x lorsqu'il depasse le seuil 'thresh'."))

(defmethod sort* ((synapses list) (predicate symbol) &key key)
  (let ((pos '()))
    (dotimes (n (length synapses))
      (push (list (elt synapses n) n) pos))
    (if key
      (sort pos predicate :key #'(lambda (x) (funcall key (car x))))
      (sort pos predicate :key #'car))))

(defmethod sort* ((synapses vector) (predicate symbol) &key key)
  (let ((pos '()))
    (dotimes (n (length synapses))
      (push (list (elt synapses n) n) pos))
    (if key
      (sort pos predicate :key #'(lambda (x) (funcall key (car x))))
      (sort pos predicate :key #'car))))

(defmethod sort* ((synapses array) (predicate symbol) &key key)
  (let ((pos '()))
    (dotimes (i (array-dimension synapses 1))
      (dotimes (n (array-dimension synapses 0))
        (push (list (aref synapses n i) i n) pos)))
    (if key
      (sort pos predicate :key #'(lambda (x) (funcall key (car x))))
      (sort pos predicate :key #'car))))

;(defmethod sort* ((synapses mlp) (predicate symbol) &key key)
;  (let ((pos '()))
;    (setf synapses (net synapses))
;    (dotimes (w (length synapses))
;      (dotimes (i (array-dimension (nth w synapses) 1))
;        (dotimes (n (array-dimension (nth w synapses) 0))
;          (push (list (aref (nth w synapses) n i) (list w i) (list (1+ w) n)) pos))))
;    (if key
;      (sort pos predicate :key #'(lambda (x) (funcall key (car x))))
;      (sort pos predicate :key #'car))))

;;; MATRICES

(defmethod compare-vectors ((v1 vector) (v2 vector) &optional (error 0))
  (let ((r '()))
    (dotimes (n (length v1) (nreverse r))
      (if (<= (abs (- (aref v1 n) (aref v2 n))) error)
        (push 0 r) (push 1 r)))))

(defmethod compare-vectors ((v1 list) (v2 list) &optional (error 0))
  (let ((r '()))
    (dotimes (n (length v1) (nreverse r))
      (if (<= (abs (- (elt v1 n) (elt v2 n))) error)
        (push 0 r) (push 1 r)))))

;(defmethod compare-vectors ((v1 symbol) (v2 t) &optional (error 0))
;  ;(setf v1 (eval `(,v1)))
;  (let ((r '()))
;    (dotimes (n (length v1) (nreverse r))
;      (if (<= (abs (- (aref v1 n) (aref v2 n))) error)
;        (push 0 r) (push 1 r)))))

;(defmethod compare-vectors ((v1 t) (v2 symbol) &optional (error 0))
  ;(setf v2 (eval `(,v2)))
 ; (let ((r '()))
 ;   (dotimes (n (length v1) (nreverse r))
 ;     (if (<= (abs (- (aref v1 n) (aref v2 n))) error)
 ;       (push 0 r) (push 1 r)))))

;(defmethod compare-vectors ((v1 symbol) (v2 symbol) &optional (error 0))
 ; (setf v1 (eval `(,v1))
 ;       v2 (eval `(,v2)))
 ; (let ((r '()))
 ;   (dotimes (n (length v1) (nreverse r))
 ;     (if (<= (abs (- (aref v1 n) (aref v2 n))) error)
 ;       (push 0 r) (push 1 r)))))

(defmethod multiply-2-vectors ((v1 list) (v2 list))
  (mapcar #'* v1 v2))

(defmethod multiply-2-vectors ((v1 vector) (v2 vector))
  (let ((result '()))
    (dotimes (n (length v1) (apply #'vector (nreverse result)))
      (push (* (aref v1 n) (aref v2 n)) result))))

(defmethod vector-sum ((vector list))
  (apply #'+ vector))

(defmethod vector-sum ((vector t))
  (let ((result '()))
    (dotimes (n (length vector) (apply #'+ result))
      (push (aref vector n) result))))

(defun dot-product (a b)
  "Produit scalaire de A et B (listes ou vecteurs, cf. MULTIPLY-2-VECTORS)."
  (vector-sum (multiply-2-vectors a b)))

(defun multiply-two-matrices (a-matrix b-matrix
			      &key (result
				    (make-array
				     (list (nth 0 (array-dimensions a-matrix))
					   (nth 1 (array-dimensions b-matrix))))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-matrix (required)
       ==> another 2d matrix, with dimensions such that
           the product of a-matrix and b-matrix is defined
   [3] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain product of two matrices
returns
   [1] product of two matrices (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions b-matrix)))
        (common (nth 0 (array-dimensions b-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
        (setf (aref result i j) 0.0)
        (dotimes (k common)
          (incf (aref result i j)
                (* (aref a-matrix i k) (aref b-matrix k j))))))))

;(multiply-two-matrices #2a((0 0 1) (0 1 0) (1 0 0)) #2a((10 9) (8 7) (6 5)))
;==> #2a((6.0 5.0) (8.0 7.0) (10.0 9.0))

(defun multiply-matrix-and-vector (a-matrix b-vector
				   &key (result (make-list (length a-matrix))))
  "version list : given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-vector (required)
       ==> a list, with dimensions such that
           the product of a-matrix and b-vector is defined
   [3] result (keyword; new vector of appropriate size)
       <== a list to contain product of a-matrix and b-vector
returns
   [1] product of a-matrix and b-vector (placed in result)"
  (let ((m (length a-matrix))
        (n (length b-vector)))
    (dotimes (i m result)
      (setf (elt result i) 0.0)
      (dotimes (j n)
        (setf (elt result i)
              (+ (elt result i)
		 (* (nth j (elt a-matrix i)) (elt b-vector j))))))))

(defun matrix-vector (matrix x)
  "A-MATRIX . X -- meme resultat que MULTIPLY-MATRIX-AND-VECTOR, mais en un
seul parcours de chaque ligne de MATRIX (DOT-PRODUCT) plutot que par NTH/ELT
indexes dans une boucle imbriquee -- O(lignes x colonnes) au lieu de
O(lignes x colonnes^2) quand MATRIX est une liste de listes. X peut etre une
liste ou un vecteur (DOT-PRODUCT), toujours converti en liste. A utiliser a
la place de MULTIPLY-MATRIX-AND-VECTOR des que la matrice est grande ou
l'appel frequent (boucle d'apprentissage) ; MULTIPLY-MATRIX-AND-VECTOR reste
tel quel, rien ne l'oblige a changer."
  (let ((x (coerce x 'list)))
    (mapcar #'(lambda (row) (dot-product row x)) matrix)))

(defun retropropagate-signal (matrix signal)
  "Signal d'erreur ramene a l'entree de MATRIX : transposee(MATRIX) . SIGNAL.
Meme resultat que (hidden-error-estimation (error-retropropagation matrix
signal)) (mlp.lisp), en O(lignes x colonnes) au lieu de O(lignes x
colonnes^2) -- memes raisons que MATRIX-VECTOR ; HIDDEN-ERROR-ESTIMATION/
ERROR-RETROPROPAGATION restent definies telles quelles."
  (let ((acc (make-list (length (car matrix)) :initial-element 0.0)))
    (loop for row in matrix
          for s in signal
          unless (zerop s)
            do (setf acc (mapcar #'(lambda (a w) (+ a (* s w))) acc row)))
    acc))

(defun accumulate-weights (matrix signals inputs learn &key (radius 0))
  "MATRIX + LEARN . somme sur les positions de SIGNAL (x) INPUT -- la
correction de UPDATE-HIDDEN-WEIGHTS/UPDATE-OUTPUT-WEIGHTS (mlp.lisp),
generalisee a plusieurs positions (une sequence, pas un seul stimulus) et
cumulee avant d'etre ajoutee a MATRIX. Tous les signaux doivent avoir ete
calcules avant, avec les poids d'origine (SIGNALS/INPUTS ne referencent
jamais MATRIX en cours de calcul). RADIUS > 0 (defaut 0, sans effet) repartit
en plus la correction cumulee sur les neurones voisins d'indice (DIFFUSE-ROWS,
meme falloff gaussien que le voisinage d'un SOM) -- cf. (radius ann)."
  (let* ((inputs (mapcar #'(lambda (x) (coerce x 'list)) inputs))
         (correction
           (loop for row in matrix
                 for i from 0
                 collect (let ((row (make-list (length row) :initial-element 0.0)))
                           (loop for s in signals
                                 for x in inputs
                                 for si = (* learn (nth i s))
                                 unless (zerop si)
                                   do (setf row (mapcar #'(lambda (c xj) (+ c (* si xj))) row x)))
                           row))))
    (add-2-matrices matrix (diffuse-rows correction radius))))

(defun add-2-matrices (a-matrix b-matrix
		       &key (result (make-listarray (length a-matrix)
						    (length (car a-matrix)))))
  (let ((m (length a-matrix))
        (n (length (car a-matrix)))
	)
    (dotimes (i m result)
      (dotimes (j n)
        (setf (nth j (nth i result))
              (+ (nth j (nth i a-matrix)) (nth j (nth i b-matrix)))) ))))

(defun subtract-two-matrices
       (a-matrix
        b-matrix
        &key
        (result
         (make-array (array-dimensions a-matrix))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-matrix (required)
       ==> a 2d matrix, with dimensions the same
           as a-matrix
   [3] result (keyword; new vector of appropriate size)
       <== a matrix to contain result of subtracting
           b-matrix from a-matrix
returns
   [1] a-matrix minus b-matrix (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions a-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
        (setf (aref result i j)
              (- (aref a-matrix i j) (aref b-matrix i j)))))))

(defun const*matrix (const matrix)
  (let ((m (nth 0 (array-dimensions matrix)))
        (n (nth 1 (array-dimensions matrix)))
        result)
    (setq result (make-array (list m n)))
    (dotimes (o m result)
      (dotimes (p n)
        (setf (aref result o p) (* const (aref matrix o p)))))))

(defun transpose
       (a-matrix
        &key
        (result
         (make-array
          (reverse (array-dimensions a-matrix)))))
  "given
   [1] A (required)
       ==> a 2d matrix
   [2] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain transpose of a-matrix
returns
   [1] transpose of a-matrix (placed in result)"
  (let ((list-of-two-integers (array-dimensions a-matrix)))
    (dotimes (i (nth 0 list-of-two-integers) result)
      (dotimes (j (nth 1 list-of-two-integers))
        (setf (aref result j i)
              (aref a-matrix i j))))))

(defun hadamar-product (a-matrix b-matrix) ; version list
  "hadamar-product of two matrix of same size."
  (let* ((m (length a-matrix))
	 (n (length (car a-matrix)))
         (result (make-listarray m n #'(lambda () 0))) )
    (dotimes (i m result)
      (dotimes (j n)
        (setf (nth j (nth i result))
              (* (nth j (nth i a-matrix)) (nth j (nth i b-matrix))))))))

;(hadamar-product '((1 2 3) (4 5 6)) '((-1 -2 -3) (-4 -5 -6)))

(defun kronecker-product
       (a-matrix
        b-matrix)
  "kronecker-product of two matrices."
  (let* ((i (nth 0 (array-dimensions a-matrix)))
         (j (nth 1 (array-dimensions a-matrix)))
         (k (nth 0 (array-dimensions b-matrix)))
         (l (nth 1 (array-dimensions b-matrix)))
         (result (make-array (list (* k i) (* j l)))))
    (dotimes (a i)
      (let ((o 0)
            (p 0))
        (dotimes (b k)
          (setf (aref result o p)
                (* (aref a-matrix a o) (aref b-matrix i j))))))))

(defmethod substract-2-vectors ((v1 array) (v2 array))
  (let ((e '()))
    (dotimes (i (length v1) (apply #'vector (reverse e)))
      (push (- (aref v1 i)
               (aref v2 i))
            e))))

(defmethod substract-2-vectors ((v1 list) (v2 list))
  (let ((e '()))
    (dotimes (i (length v1) (reverse e))
      (push (- (elt v1 i)
               (elt v2 i))
            e))))

(defgeneric add-two-vectors (v1 v2)
  (:documentation "Somme terme a terme de V1 et V2 (listes ou vecteurs)."))

(defmethod add-two-vectors ((v1 list) (v2 list))
  (mapcar #'+ v1 v2))

(defmethod add-two-vectors ((v1 t) (v2 t))
  (let ((e '()))
    (dotimes (i (length v1) (apply #'vector (reverse e)))
      (push (+ (aref v1 i)
               (aref v2 i))
            e))))

(defgeneric multiply-vector (f v)
  (:documentation "V (liste ou vecteur) multiplie par le scalaire F."))

(defmethod multiply-vector (f (v list))
  (mapcar #'(lambda (x) (* f x)) v))

(defmethod multiply-vector (f (v t))
  (let ((e '()))
    (dotimes (i (length v) (apply #'vector (reverse e)))
      (push (* f
               (aref v i))
            e))))

(defun ldl->lvectors (ldl)
  (mapcar #'(lambda (x) (apply #'vector x)) ldl))

(defun vector->list (vector)
  (coerce vector 'list))

(defun lvectors->ldl (lvectors)
  (mapcar #'(lambda (x) (vector->list x)) lvectors))

;;; SCALING and data SAMPLING

(defun scale-input (input output)
  (let ((max (apply #'max output)))
    (mapcar #'(lambda (x)
                (float (/ x max)))
            input)))

(defun echant (list n)
  "Does a resampling of list by n samples."
  (let ((l (length list))
        (r '()))
    (if (>= n l)
      (dotimes (i n (reverse r))
        (push (nth (floor (* i (/ l n))) list) r))
      (let ((newn (- n 2)))
        (dotimes (i newn (append (list (car list)) (reverse r) (last list)))
          (push (nth (round (* (1+ i) (/ (- l 2) (if (evenp newn)
                                                   (1+ newn)
                                                   (if (= 1 newn)
                                                     2 newn))))) list) r))))))

(defun counter (n &key (start 0) (step 1))
  (let ((r (list start)))
    (dotimes (i n (reverse r))
      (push (+ (car r) step) r))))

(defgeneric number->bin (x form)
  (:documentation
   "Converti x (numbre ou liste) en representation ''binaire'' a base 'form'"))

(defmethod number->bin ((x number) (form number))
  (let ((r '())
        (value (format nil
                       (concatenate 'string "~" (format nil "~DB" form))
                       x)))
    (dotimes (n (length value) (reverse r))
      (push (read-from-string (string (if (eq #\Space (elt value n)) #\0 (elt value n)))) r))))

(defmethod number->bin ((x list) (form number))
  (mapcar #'(lambda (x) (number->bin x form)) x))

(defgeneric scale (x minin maxin minout maxout)
  (:documentation "Effectue une mise a l'echelle de x borne entre minin et maxin."))

(defmethod scale ((x number) minin maxin minout maxout)
  (let ((ratio (/ (- maxout minout) (- maxin minin))))
    (float (+ minout (* ratio (- x minin))))))

(defmethod scale ((x list) minin maxin minout maxout)
  (mapcar #'(lambda (a) (scale a minin maxin minout maxout)) x))

(defmethod scale ((x vector) minin maxin minout maxout)
  (coerce (scale (coerce x 'list) minin maxin minout maxout) 'vector))

(defgeneric normalize (x )
  (:documentation "normalise les donnees x entre 0 et 1"))

(defmethod normalize ((x list))
  (let ((min (apply #'min x))
        (max (apply #'max x)))
    (mapcar #'(lambda (a) (scale a min max 0.0 1.0)) x)))

(defmethod normalize ((x vector))
  (coerce (normalize (coerce x 'list)) 'vector))

(defgeneric filtre-median (values window)
  (:documentation "Filtre median d'une liste ou d'un vecteur"))

(defmethod filtre-median ((values number) (window integer))
  (assert (oddp window))
  values)

(defmethod filtre-median ((values list) (window integer))
  (assert (oddp window))
  (let ((r '()))
    (dotimes (i (length values) r)
      (let* ((lastp (+ i window))
             (win (subseq values i (if (>= lastp (length values))
                                     (length values) lastp)))
             (p (floor (/ (length win) 2))))
        (setf r (append r (list (elt (sort win '<) p))))))))

(defmethod filtre-median ((values vector) (window integer))
  (assert (oddp window))
  (let ((r '()))
    (dotimes (i (length values) (coerce r 'vector))
      (let* ((lastp (+ i window))
             (win (subseq values i (if (>= lastp (length values))
				       (length values) lastp)))
             (p (floor (/ (length win) 2))))
        (setf r (append r (list (elt (sort win '<) p))))))))

(defun codin (n &rest data)
  (let* ((length (apply #'+ (mapcar #'car data)))
         (ratio (floor (/ n length)))
         (rest (- n (* length ratio)))
         (subrest (floor (/ rest (length data))))
         (result (list)))
    (dolist (d data (setf result (reverse result)))
      (dotimes (n (floor (+ (* (car d) ratio) subrest)))
        (push (float (- (cadr d) (* n (/ (cadr d) (floor (+ (* (car d) ratio) subrest))))))
              result)))

    (if (= n (length result))
      result
      (append result (make-list (- n (length result))
                                :initial-element (car (last result)))))))

;(length (codin 64 '(1 1) '(4 0) '(.4 .7) '(.4 .7)))

(defun scale-th-error (x)
  (if (<= x 0) 0
      (round (* 15 (+ 5 (log (* 1000 x) 5))))))

(defgeneric euclidian (x y &key side lattice boundary)
  (:documentation "Distance euclidienne entre x et y :

    d(x, y) = sqrt( somme_i (x_i - y_i)^2 )

Sans mot-cle, distance ordinaire entre deux listes ou vecteurs (espace des
entrees, comme toujours). Avec les mots-cles de topologie (cf. GRID-METRIC),
X et Y sont des coordonnees de neurones sur la grille d'un SOM : LATTICE
(:square par defaut, ou :hex) et BOUNDARY (:bounded par defaut, ou :torus,
qui demande SIDE, le cote de la grille)."))

(defmethod euclidian :around (x y &key side (lattice :square) (boundary :bounded))
  ;; grille carree bornee (ou pas de mot-cle) : la distance ordinaire,
  ;; calculee par les methodes ci-dessous exactement comme avant
  (if (and (eq lattice :square) (eq boundary :bounded))
      (call-next-method)
      (grid-metric :euclidian x y :side side :lattice lattice :boundary boundary)))

(defmethod euclidian ((a list) (b list) &key side lattice boundary)
  (declare (ignore side lattice boundary))
  (sqrt (apply #'+ (mapcar #'(lambda (x y) (expt (- x y) 2)) a b))))

(defmethod euclidian ((a vector) (b vector) &key side lattice boundary)
  (declare (ignore side lattice boundary))
  (sqrt (apply #'+ (loop for k from 0 to (1- (length a))
			 collect (expt (- (elt a k) (elt b k)) 2)))))

(defmethod euclidian ((a vector) (b list) &key side lattice boundary)
  (declare (ignore side lattice boundary))
  (sqrt (apply #'+ (loop for k from 0 to (1- (length a))
			 collect (expt (- (elt a k) (elt b k)) 2)))))

(defmethod euclidian-fast ((a list) (b list))
  (apply #'+ (mapcar #'(lambda (x y) (expt (- x y) 2)) a b)))

(defmethod euclidian-fast ((a vector) (b vector))
  (apply #'+ (loop for k from 0 to (1- (length a))
		   collect (expt (- (elt a k) (elt b k)) 2))))

(defmethod euclidian-fast ((a vector) (b list))
  (apply #'+ (loop for k from 0 to (1- (length a))
		   collect (expt (- (elt a k) (elt b k)) 2))))

;;*************      AUTRES DISTANCES     **********************************
;;
;; Toutes s'appellent comme EUCLIDIAN, (distance a b), sur deux listes ou
;; vecteurs de meme longueur (melanges possibles), et se donnent par leur nom
;; au slot DISTANCE d'un SOM (espace des entrees : COSINE, NEUROMUSE-DISTANCE)
;; ou a son slot GRID-DISTANCE (coordonnees sur la grille : EUCLIDIAN,
;; CHEBYSHEV, MANHATTAN, qui acceptent les mots-cles de topologie decrits
;; plus bas), par exemple (setf (distance som) 'cosine).
;; Notation : a = (a_1 ... a_d), b = (b_1 ... b_d).

(defun cosine (a b)
  "Distance cosinus : 1 - cos(a, b), soit

    d(a, b) = 1 - (somme_i a_i b_i) / ( sqrt(somme_i a_i^2) sqrt(somme_i b_i^2) )

Vaut 0 pour deux vecteurs de meme direction, 1 pour deux vecteurs orthogonaux,
2 pour deux vecteurs opposes. Ne depend que de la direction, pas de la norme :
dans l'espace des entrees d'un SOM, elle code la FORME d'un spectre plutot que
son intensite -- l'equivalent coherent de ce que NEUROMUSE-DISTANCE fait de
facon detournee. Si l'un des deux vecteurs est nul, la direction n'est pas
definie : la distance vaut alors 1 (aucune ressemblance), et 0 si les deux
sont nuls."
  (let ((ab 0) (aa 0) (bb 0))
    (map nil (lambda (x y)
               (incf ab (* x y)) (incf aa (* x x)) (incf bb (* y y)))
         a b)
    (cond ((and (zerop aa) (zerop bb)) 0)
          ((or (zerop aa) (zerop bb)) 1)
          (t (- 1 (/ ab (sqrt (* aa bb))))))))

;; distances pour la GRILLE d'un SOM (slot GRID-DISTANCE), sur des
;; coordonnees de neurones, (colonne ligne) comme les renvoie 2D. La metrique
;; (EUCLIDIAN, CHEBYSHEV, MANHATTAN) et la topologie de la grille sont
;; independantes : la topologie arrive par les mots-cles SIDE, LATTICE et
;; BOUNDARY, que LEARN (som.lisp) tire du slot TOPOLOGY de la carte, et
;; GRID-METRIC applique la bonne methode pour une meme metrique.

(defun hex-position (p)
  "Position dans le plan du neurone de coordonnees P = (colonne ligne) sur
une grille hexagonale dont les lignes impaires sont decalees d'une demi-case
vers la droite :  x = c + (l mod 2)/2,  y = l * sqrt(3)/2.
Les six voisins directs sont ainsi tous a la distance 1."
  (let ((c (elt p 0)) (l (elt p 1)))
    (list (+ c (/ (mod l 2) 2)) (* l (/ (sqrt 3) 2)))))

(defun hex-steps (a b)
  "Nombre de pas hexagonaux entre A et B, coordonnees (colonne ligne) sur la
meme grille hexagonale que HEX-POSITION. En coordonnees axiales
q = c - floor(l/2), r = l, puis cubiques (q, r, -q-r) :

    pas(a, b) = max( |dq|, |dr|, |dq + dr| )

c'est-a-dire la distance de Tchebychev en coordonnees cubiques."
  (flet ((axial (p)
           (let ((c (elt p 0)) (l (elt p 1)))
             (values (- c (floor l 2)) l))))
    (multiple-value-bind (qa ra) (axial a)
      (multiple-value-bind (qb rb) (axial b)
        (let ((dq (- qa qb)) (dr (- ra rb)))
          (max (abs dq) (abs dr) (abs (+ dq dr))))))))

(defun %plain-metric (metric a b)
  "METRIC (:euclidian, :chebyshev, :manhattan) entre A et B, sans topologie."
  (let ((acc 0))
    (map nil (lambda (x y)
               (let ((d (abs (- x y))))
                 (ecase metric
                   (:euclidian (incf acc (* d d)))
                   (:chebyshev (setf acc (max acc d)))
                   (:manhattan (incf acc d)))))
         a b)
    (if (eq metric :euclidian) (sqrt acc) acc)))

(defun %lattice-metric (metric a b lattice)
  (ecase lattice
    (:square (%plain-metric metric a b))
    (:hex (if (eq metric :euclidian)
              ;; distance dans le plan entre les positions hexagonales
              (%plain-metric :euclidian (hex-position a) (hex-position b))
              ;; Tchebychev et Manhattan : le nombre de pas hexagonaux, la
              ;; metrique naturelle d'un pavage (voisinage en hexagone)
              (hex-steps a b)))))

(defun grid-metric (metric a b &key side (lattice :square) (boundary :bounded))
  "Distance METRIC (:euclidian, :chebyshev ou :manhattan) entre les neurones
de coordonnees A et B, selon la topologie de la grille :

  LATTICE  :square -- grille carree, coordonnees (colonne ligne) telles quelles.
           :hex    -- grille hexagonale, lignes impaires decalees d'une
                      demi-case (cf. HEX-POSITION). :euclidian y mesure la
                      distance dans le plan ; :chebyshev et :manhattan, le
                      nombre de pas hexagonaux (cf. HEX-STEPS).
  BOUNDARY :bounded -- grille bornee.
           :torus   -- bords opposes recolles, grille de cote SIDE :

               d_tore(a, b) = min sur k_i dans {-1, 0, 1} de d(a, b + k * SIDE)

                      la plus courte distance a l'une des copies de B
                      decalees d'un cote dans chaque direction. Sur une grille
                      hexagonale, SIDE doit etre pair pour que l'alternance
                      des lignes decalees se poursuive a travers le bord."
  (ecase boundary
    (:bounded (%lattice-metric metric a b lattice))
    (:torus
     (unless side
       (error "GRID-METRIC : tore sans cote de grille -- passer :SIDE."))
     (when (and (eq lattice :hex) (oddp side))
       (error "GRID-METRIC : tore hexagonal de cote impair (~D) -- l'alternance
des lignes decalees ne se raccorde qu'avec un cote pair." side))
     (let ((best nil)
           (b (coerce b 'list)))
       ;; toutes les copies de B decalees de -SIDE, 0 ou +SIDE sur chaque axe
       (labels ((images (coords)
                  (if (null coords)
                      (list nil)
                      (loop for k in (list (- side) 0 side)
                            append (mapcar (lambda (rest) (cons (+ (car coords) k) rest))
                                           (images (cdr coords)))))))
         (dolist (image (images b) best)
           (let ((d (%lattice-metric metric a image lattice)))
             (when (or (null best) (< d best)) (setf best d)))))))))

(defun chebyshev (a b &key side (lattice :square) (boundary :bounded))
  "Distance de Tchebychev : le plus grand ecart, axe par axe,

    d(a, b) = max_i |a_i - b_i|

Sur la grille carree d'un SOM, c'est exactement la distance que suppose la
fenetre carree de VOISINS : voisinage carre, coins et bords a egalite. Sur une
grille hexagonale, le nombre de pas hexagonaux. Mots-cles de topologie : cf.
GRID-METRIC."
  (grid-metric :chebyshev a b :side side :lattice lattice :boundary boundary))

(defun manhattan (a b &key side (lattice :square) (boundary :bounded))
  "Distance de Manhattan : la somme des ecarts, axe par axe,

    d(a, b) = somme_i |a_i - b_i|

Sur la grille carree d'un SOM : voisinage en losange. Sur une grille
hexagonale, le nombre de pas hexagonaux, comme CHEBYSHEV. Mots-cles de
topologie : cf. GRID-METRIC."
  (grid-metric :manhattan a b :side side :lattice lattice :boundary boundary))

(defun neuromuse-distance (x w)
  "Distance euclidienne entre l'entree X et l'activation X*W (produit
composante par composante) : d(x, x*w) = sqrt(somme xi^2 (1 - wi)^2). C'est
la distance des SOM de neuromuse de 1999 a 2001, ou FIND-WINNER comparait
l'entree a l'activation du neurone plutot qu'a ses poids ; a donner au slot
DISTANCE d'un SOM pour retrouver ce comportement. Proprietes a connaitre : le
classement des neurones ne depend ni de l'amplitude globale de X ni du signe
de ses composantes, une composante nulle de X ne compte pas, et le neurone
ideal pour toute entree est W = (1 1 ... 1) -- alors que LEARN tire W vers X.
X et W : listes ou vecteurs, de meme longueur."
  (sqrt (let ((s 0))
	  (dotimes (k (length x) s)
	    (let ((xk (elt x k)))
	      (incf s (expt (- xk (* xk (elt w k))) 2)))))))

(defun transition-matrix (sequence size)
  "Matrice SIZE x SIZE des comptes de transition : (aref m a b) = nombre de
fois ou SEQUENCE passe de l'etat A a l'etat B d'un indice au suivant --
SEQUENCE une suite d'entiers dans [0, SIZE). Pas specifique aux SOM ni aux
reseaux de neurones ; deplacee depuis examples/pinson_som.lisp, ou SEQUENCE
etait une suite d'indices de neurones gagnants (cf. WINNER-SEQUENCE, restee
sur place, qui la produit)."
  (let ((m (make-array (list size size) :initial-element 0)))
    (loop for (a b) on sequence while b
          do (incf (aref m a b)))
    m))

(defun cos-similarity (m1 m2)
  "Similarite cosinus entre deux matrices 2D de meme taille, vues comme deux
vecteurs aplatis -- insensible a l'echelle absolue de leurs valeurs
(contrairement a une comparaison directe des comptes/valeurs bruts). 1.0 =
memes proportions, 0.0 = rien en commun. Deplacee depuis
examples/pinson_som.lisp (COMPARE-TRANSITIONS), ou elle comparait des
matrices de transition case-a-case d'un SOM, un usage qui n'a rien de
specifique aux reseaux de neurones."
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

;;*************      TOPOLOGIE     **********************************

(defun 2d (x n)
  (let ((k (sqrt n)))
    (list (floor (mod x k)) (floor (/ x k)))))

(if (equalp "ppc" (machine-type))
   (defun d2 (x y n) ;; version ppc
	 (+ x (* y (sqrt n) )))
   (defun d2 (x y n) ;; version x86
     (floor (+ x (* y (sqrt n) )))))

;(2d 4 25)
;(d2 4 0 25)

(defun 3d (x n)
  (let ((k (expt n 1/3)))
    (list (mod x k)
	  (mod (floor (/ x k)) k)
	  (mod (floor (/ x (* k k))) k))))

(defun d3 (x y z n)
  (let ((k (expt n 1/3)))
    (+ x (* y k) (* z k k))))

;;************         VOISINAGE      ********************************

;; todo: developper, generaliser, reduire

(defun %voisins-bornes (pos radius n &optional r )
  ;pos = '(x y z ...) et n = 1- sqrt n
  ;; REMOVE-DUPLICATES compare ici des listes de coordonnees, donc avec
  ;; EQUAL : avec le test par defaut (EQL), jusqu'en octobre 2026, les doublons
  ;; crees par le bornage aux bords restaient (au coin (0 0), rayon 1, 9
  ;; entrees pour 4 cases), et LEARN corrigeait ces neurones plusieurs fois
  ;; par pas -- le gagnant d'un coin, 4 fois.
  (if (not pos)
      (remove-duplicates r :test #'equal)
      (let ((x (pop pos))
	    (o (1- n))
	    tmp)
	(if (not r)
	    (progn
	      (push (list x) r)
	      (loop for i from 1 to radius
		    do
		    (push (list (min (+ x i) o)) r)
		    (push (list (max (- x i) 0)) r))
	      (%voisins-bornes pos radius n r))
	    (progn
	      (loop for a in r
		    do
		    (push (append a (list x)) tmp)
		    (loop for i from 1 to radius
			  do
			  (push (append a (list (min (+ x i) o))) tmp)
			  (push (append a (list (max (- x i) 0))) tmp)))
	      (%voisins-bornes pos radius n tmp))))))

(defun voisins (pos radius n &key (boundary :bounded))
  "Coordonnees des neurones de la fenetre carree de rayon RADIUS centree sur
POS = (colonne ligne ...), sur une grille de cote N, sans doublon. BOUNDARY
:bounded (defaut, comme toujours) borne la fenetre aux bords de la grille ;
:torus la fait revenir par le bord oppose (coordonnees prises modulo N).
Convient aussi a une grille hexagonale : la fenetre carree y contient tous
les neurones a moins de RADIUS pas hexagonaux."
  (ecase boundary
    (:bounded (%voisins-bornes pos radius n))
    (:torus
     (let ((axes (mapcar (lambda (x)
                           (remove-duplicates
                            (loop for i from (- radius) to radius
                                  collect (mod (+ x i) n))))
                         pos)))
       (reduce (lambda (axis acc)
                 (loop for v in axis
                       append (mapcar (lambda (rest) (cons v rest)) acc)))
               axes :from-end t :initial-value (list nil))))))

;le chapeau divergerait, selon Uwe Lammel und Jurgen Cleve Kunstliche Intelligenz, 2001.
;(* learn (exp (/ (- (expt error 2)) (expt (* 2 radius) 2))))

(defun gaussian-hat (learn error distance)
  (* learn (exp (/ (- (expt distance 2)) (expt (* 2 error) 2)))))

(defun diffuse-rows (matrix radius)
  "Repartit chaque colonne de MATRIX (liste de lignes) sur les lignes
voisines, pondere par GAUSSIAN-HAT : la ligne resultat I2 recoit, pour
chaque colonne, la somme sur toutes les lignes I de (gaussian-hat 1 radius
|I-I2|) * MATRIX[I][colonne] -- meme falloff gaussien que la correction de
voisinage d'un SOM (NEIGHBOURHOOD-CORRECTION, som.lisp), ici applique a
l'indice de ligne plutot qu'a une distance de grille. RADIUS proche de 0 :
identite exacte (MATRIX inchangee), meme garde que NEIGHBOURHOOD-CORRECTION
-- une ligne ne recoit alors que sa propre contribution."
  (if (< radius 1e-6)
      matrix
      (let* ((m (length matrix))
             (n (length (car matrix)))
             (result (make-listarray m n #'(lambda () 0.0))))
        (dotimes (i2 m result)
          (dotimes (j n)
            (setf (nth j (nth i2 result))
                  (loop for i from 0 below m
                        sum (* (gaussian-hat 1 radius (abs (- i i2)))
                               (nth j (nth i matrix))))))))))

;;************  RETROPROPAGATION : BRIQUES GENERIQUES  **************
;; Fonctions pures (aucun argument mute, aucun acces a un slot ANN), dont
;; certaines deplacees de mlp.lisp (HIDDEN-SIGNAL-ERROR : derivee de la
;; fonction de transfert, generique malgre son nom, aucune dependance au mlp)
;; et d'autres ajoutees pour fred/transformer.lisp, local -- voir
;; fred/lambda-calcul-neuromuse.md (encodage de position, normalisation de
;; couche et leur retropropagation). Genericite d'architecture : rien ici ne
;; suppose mlp/som/transformer -- place apres MULTIPLY-2-VECTORS/
;; SUBSTRACT-2-VECTORS/DOT-PRODUCT, dont tout ceci depend.

(defun hidden-signal-error (hidden-activation hidden-error-estimation)
  "Signal d'erreur avant une fonction de transfert LOGISTIC, connaissant
HIDDEN-ACTIVATION (sa sortie) et HIDDEN-ERROR-ESTIMATION (le signal apres) :
(1 - a) (e a). Deplace de mlp.lisp tel quel (ou LEARN de mlp/rmlp l'utilise
encore) -- pure, generique, aucune dependance au reste de mlp.lisp."
  (assert (= (length hidden-activation) (length hidden-error-estimation)))
  (multiply-2-vectors
   (substract-2-vectors
    (make-list (length hidden-activation) :initial-element 1)
    hidden-activation)
   (multiply-2-vectors
    hidden-error-estimation
    hidden-activation)))

(defun transfer-signal-error (fun activation error)
  "Signal d'erreur avant la fonction de transfert FUN, connaissant ACTIVATION
(sa sortie) et ERROR (le signal apres). Pour #'logistic, HIDDEN-SIGNAL-ERROR ;
pour #'linear, ERROR tel quel (derivee 1)."
  (cond ((eq fun #'logistic) (hidden-signal-error activation error))
        ((eq fun #'linear) error)
        (t (error "Pas de derivee connue pour la fonction de transfert ~S." fun))))

(defun with-bias (x)
  "X (liste ou vecteur) suivi d'une entree de biais, toujours a 1.0 -- meme
convention que le biais de PERCEPTRON (cf. PERCEPTRON-INPUT, perceptron.lisp),
ici comme fonction pure reutilisable plutot que specifique a une classe."
  (append (coerce x 'list) (list 1.0)))

(defun positional-encoding (pos size)
  "Encodage de position sinusoidal de Vaswani et al. (« Attention Is All You
Need », 2017), un vecteur de SIZE nombres pour la position POS."
  (loop for i from 0 below size
        for freq = (expt 10000.0 (- (/ (* 2 (floor i 2)) size)))
        collect (if (evenp i) (sin (* pos freq)) (cos (* pos freq)))))

(defun layer-norm (x ln &optional (epsilon 1e-5))
  "Normalisation de couche de X (une trame) avec LN = (gains biais) : centre
X, le reduit par son ecart-type, puis applique GAINS/BIAIS terme a terme.
Renvoie trois valeurs : la sortie, X centre-reduit (XHAT) et l'inverse de
l'ecart-type (INV-SIGMA) -- ces deux dernieres valeurs sont ce dont
LAYER-NORM-RETROPROPAGATION a besoin, pas a recalculer depuis la sortie."
  (let* ((n (length x))
         (mean (/ (apply #'+ x) n))
         (centered (mapcar #'(lambda (a) (- a mean)) x))
         (inv-sigma (/ 1.0 (sqrt (+ epsilon (/ (dot-product centered centered) n)))))
         (xhat (multiply-vector inv-sigma centered)))
    (values (add-two-vectors (multiply-2-vectors (first ln) xhat) (second ln))
            xhat
            inv-sigma)))

(defun layer-norm-retropropagation (signal xhat inv-sigma ln)
  "Retropropagation d'un LAYER-NORM : SIGNAL est le signal d'erreur apres la
normalisation, XHAT/INV-SIGMA ceux renvoyes par LAYER-NORM pour la meme trame.
Renvoie trois valeurs : le signal d'erreur a l'entree, et les signaux des
gains et des biais de LN (a cumuler sur toute la sequence par l'appelant,
cf. UPDATE-LAYER-NORM)."
  (let* ((n (length signal))
         (dxhat (multiply-2-vectors (first ln) signal))
         (mean-d (/ (apply #'+ dxhat) n))
         (mean-dx (/ (dot-product dxhat xhat) n)))
    (values (mapcar #'(lambda (d xh) (* inv-sigma (- d mean-d (* xh mean-dx)))) dxhat xhat)
            (multiply-2-vectors signal xhat)
            signal)))

(defun update-layer-norm (ln d-gain d-bias learn)
  "LN = (gains biais) corrige par LEARN . (D-GAIN D-BIAS), les signaux cumules
renvoyes par LAYER-NORM-RETROPROPAGATION -- meme regle que les poids
synaptiques ordinaires (ACCUMULATE-WEIGHTS), sans RADIUS : gains et biais
sont scalaires par dimension, pas une matrice de synapses entre deux couches,
DIFFUSE-ROWS n'a pas de sens ici."
  (list (add-two-vectors (first ln) (multiply-vector learn d-gain))
        (add-two-vectors (second ln) (multiply-vector learn d-bias))))

;;************         DIVERS      ********************************

(defun inventaire (liste &key (test #'equal))
  "Renvoie ((element n) ...) : chaque élément distinct de LISTE et son
nombre d'occurrences, dans l'ordre de première apparition."
  (let ((inv '()))
    (dolist (x liste (nreverse inv))
      (let ((e (assoc x inv :test test)))
        (if e
            (incf (second e))
            (push (list x 1) inv))))))

;; variante hash-table de INVENTAIRE (memes entrees/sorties, O(n) au lieu de
;; O(n^2) sur de grandes listes grace a la table au lieu d'ASSOC) -- deplacee
;; ici en meme temps qu'INVENTAIRE depuis examples/pinson_som.lisp, ou elle
;; servait deja au meme usage (compter les cases gagnantes d'un SOM sur un
;; grand nombre de presentations). A reinjecter dans les methodes
;; d'apprentissage (LEARN/TRAIN-*) le jour ou l'une d'elles a besoin de ce
;; genre de comptage en interne -- pour l'instant aucune n'appelle INVENTAIRE
;; ni INVENTAIRE-H, ce sont des outils d'analyse post-hoc appeles depuis le
;; REPL/les exemples.
(defun inventaire-h (liste &key (test #'equal))
  "Comme INVENTAIRE, via une hash-table : chaque élément distinct de LISTE et
son nombre d'occurrences, dans l'ordre de première apparition."
  (let ((h (make-hash-table :test test)) (ordre '()))
    (dolist (x liste)
      (unless (gethash x h) (push x ordre))
      (incf (gethash x h 0)))
    (mapcar (lambda (x) (list x (gethash x h))) (nreverse ordre))))

; eof
