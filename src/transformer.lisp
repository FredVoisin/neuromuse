;;;; transformer.lisp
;;;; Transformer (Vaswani et al., « Attention Is All You Need », 2017), version
;;;; neuromuse : décodeur seul, auto-attention multi-têtes (causale par défaut),
;;;; normalisation de couche avant chaque sous-couche (« pre-LN »), connexions
;;;; résiduelles, encodage de position sinusoïdal.
;;;;
;;;; Sous-classe de ANN, apparentée à MLP, avec pour constructeur la macro
;;;; MAKE-TRANSFORMER, et les méthodes ACTIVATION, LEARN (backpropagation),
;;;; CLEAR, SAVE. Le réseau du transformer est stocké dans NET en listes de listes.
;;;;
;;;; Ce qui est repris de l'existant, tel quel (rien ici n'est redéfini
;;;; localement : voir fred/lambda-calcul-neuromuse.md §10 pour l'historique
;;;; de cette adaptation à la structure nodes/edges/radius/learn-schedule) :
;;;;   maths.lisp : logistic, linear (fonctions de transfert), softmax (ajoutée
;;;;     avec les autres fonctions de transfert), multiply-2-vectors,
;;;;     vector-sum, substract-2-vectors, add-two-vectors, multiply-vector,
;;;;     make-listarray, noise (température des poids, NET-TEMP), temp-value
;;;;     (température des sorties, TEMP), check-error (mesure d'erreur, comme
;;;;     pour le MLP) ; sample-index (ajoutée) et round1 servent dans
;;;;     examples/transformer-test.lisp. DOT-PRODUCT, MATRIX-VECTOR,
;;;;     RETROPROPAGATE-SIGNAL, TRANSFER-SIGNAL-ERROR, ACCUMULATE-WEIGHTS,
;;;;     WITH-BIAS, POSITIONAL-ENCODING, LAYER-NORM, LAYER-NORM-RETROPROPAGATION,
;;;;     UPDATE-LAYER-NORM : promues dans maths.lisp depuis ce fichier (sauf
;;;;     TRANSFER-SIGNAL-ERROR, déjà là, et HIDDEN-SIGNAL-ERROR, déplacée de
;;;;     mlp.lisp) lors de cette adaptation -- fonctions pures, génériques,
;;;;     réutilisées ici sous leur nom partagé plutôt que redéfinies
;;;;     localement (« reprendre au maximum l'existant »).
;;;;   mlp.lisp : la rétropropagation. La sous-couche « feed-forward » de chaque
;;;;     bloc EST un MLP à une couche cachée (HIDDEN-FUN, #'logistic par défaut) ;
;;;;     LEARN de mlp/rmlp utilise maintenant les mêmes briques rapides
;;;;     (MATRIX-VECTOR, TRANSFER-SIGNAL-ERROR, RETROPROPAGATE-SIGNAL,
;;;;     ACCUMULATE-WEIGHTS) que ce fichier, au lieu des anciennes versions par
;;;;     NTH/ELT en O(n^3) -- UPDATE-HIDDEN-WEIGHTS/UPDATE-OUTPUT-WEIGHTS sont
;;;;     maintenant de simples enveloppes autour d'ACCUMULATE-WEIGHTS.
;;;;     RADIUS (généralisé à ANN, cf. ci-dessous) module la correction en
;;;;     pondérant la diffusion sur les neurones voisins d'indice (DIFFUSE-ROWS,
;;;;     maths.lisp) exactement comme pour le MLP -- câblé plus bas dans
;;;;     BACKPROPAGATE.
;;;;   perceptron.lisp : le biais est une entrée constante à 1 ajoutée au
;;;;     vecteur d'entrée (WITH-BIAS, maintenant dans maths.lisp) ; les
;;;;     matrices qui ont un biais ont une colonne de plus.
;;;;   neuromuse-main.lisp (ANN) : RADIUS et LEARN-SCHEDULE sont hérités, pas
;;;;     redéclarés ici -- RADIUS défaut 0 (sans effet tant que non réglé),
;;;;     LEARN-SCHEDULE nil par défaut (pas de politique ; sinon une fonction
;;;;     de self, appelée pour effet par l'appelant -- jamais par LEARN --
;;;;     qui peut ajuster LEARN-FACT et/ou RADIUS).
;;;;   udp.lisp : input-server, control-server, output-server ont une méthode
;;;;     pour le transformer (une trame par paquet, fenêtre glissante).
;;;;
;;;; Convention de signe, celle de mlp.lisp : un « signal d'erreur » est
;;;; l'opposé du gradient de l'erreur quadratique 1/2 |but - sortie|^2, et
;;;; l'apprentissage AJOUTE learn-fact * signal (x) entrée aux poids.
;;;;
;;;; Une séquence est une liste de trames, une trame une liste (ou un vecteur)
;;;; de IN-SIZE nombres. (input self) est la séquence courante, (goal self) la
;;;; liste des buts, une trame de OUT-SIZE nombres par position.

(in-package :neuromuse)

(defclass transformer (ann)
  ((name :initform 'transformer :initarg :name :accessor name :type symbol)
   (in-size :initform 2 :initarg :in-size :accessor in-size :type integer)
   (out-size :initform 2 :initarg :out-size :accessor out-size :type integer)
   (model-size :initform 16 :initarg :model-size :accessor model-size :type integer)
   (heads :initform 2 :initarg :heads :accessor heads :type integer)
   (ff-size :initform 32 :initarg :ff-size :accessor ff-size :type integer)
   (layers :initform 1 :initarg :layers :accessor layers :type integer)
   (window :initform 16 :initarg :window :accessor window :type integer)
   (causal :initform t :initarg :causal :accessor causal)
   (hidden-fun :initform #'logistic :initarg :hidden-fun :accessor hidden-fun :type function)
   (out-fun :initform #'logistic :initarg :out-fun :accessor out-fun :type function)
   (previous :initform '() :initarg :previous :accessor previous :type list)
   (goal :initform '() :initarg :goal :accessor goal :type list)
   (outputs :initform '() :initarg :outputs :accessor outputs :type list)
   (attention-map :initform '() :initarg :attention-map :accessor attention-map :type list)
   (learn-fact :initform 0.05 :initarg :learn-fact :accessor learn-fact :type float)
   (threshold :initform .001 :initarg :threshold :accessor threshold :type float)
   (stop :initform 1000 :initarg :stop :accessor stop :type integer))
  (:documentation
   "Transformer décodeur (auto-attention multi-têtes, pre-LN).
NET = (W-in blocs LN-finale W-out) :
  W-in   MODEL-SIZE x (IN-SIZE + 1)   projection d'entrée (+ biais)
  blocs  liste de LAYERS blocs (Wq Wk Wv Wo W1 W2 LN1 LN2) :
           Wq Wk Wv Wo  MODEL-SIZE x MODEL-SIZE, sans biais
           W1           FF-SIZE x (MODEL-SIZE + 1)    couche cachée du MLP
           W2           MODEL-SIZE x (FF-SIZE + 1)    sortie du MLP
           LN1 LN2      (gains biais), normalisations de couche
  LN-finale (gains biais)
  W-out  OUT-SIZE x (MODEL-SIZE + 1)
HEADS doit diviser MODEL-SIZE. WINDOW est la longueur maximale de la séquence
vue par le réseau (fenêtre glissante de PUSH-INPUT et GENERATE). Si CAUSAL est
vrai (défaut), chaque position ne voit que les positions précédentes et
elle-même : c'est un modèle de prédiction de la trame suivante.
HIDDEN-FUN (couche cachée des MLP) et OUT-FUN : #'logistic ou #'linear.
(output self) est la sortie de la dernière position, (outputs self) celles de
toutes les positions, (attention-map self) les poids d'attention du dernier
passage, par bloc puis par tête (une ligne par position).
NB : le slot ATTENTION hérité de ANN est un délai des serveurs UDP, rien à voir."))

(defmethod print-object ((self transformer) stream)
  (format stream "<transformer ~S: ~D input~:P, ~D output~:P, d=~D, ~D head~:P, ~D layer~:P>"
          (string (name self)) (in-size self) (out-size self)
          (model-size self) (heads self) (layers self))
  (values))

;;; Initialisation

(defun random-listarray (dim0 dim1)
  "Matrice DIM0 x DIM1 (liste de listes) de poids uniformes dans
[-1/sqrt(DIM1), 1/sqrt(DIM1)[."
  (let ((r (/ 1.0 (sqrt dim1))))
    (make-listarray dim0 dim1 #'(lambda () (- (random (* 2 r)) r)))))

(defun init-layer-norm (size)
  (list (make-list size :initial-element 1.0)
        (make-list size :initial-element 0.0)))

(defun init-transformer-net (in-size out-size &key (model-size 16) (heads 2)
                                                   (ff-size 32) (layers 1))
  "Initialisation aléatoire du réseau d'un transformer (voir la classe)."
  (assert (zerop (mod model-size heads)) ()
          "MODEL-SIZE (~D) doit être un multiple de HEADS (~D)." model-size heads)
  (let ((d model-size))
    (list (random-listarray d (1+ in-size))
          (loop repeat layers
                collect (list (random-listarray d d)
                              (random-listarray d d)
                              (random-listarray d d)
                              (random-listarray d d)
                              (random-listarray ff-size (1+ d))
                              (random-listarray d (1+ ff-size))
                              (init-layer-norm d)
                              (init-layer-norm d)))
          (init-layer-norm d)
          (random-listarray out-size (1+ d)))))

(defmacro make-transformer (name in out &key (model-size 16) (heads 2) (ff-size 32)
                                          (layers 1) (window 16) (causal t))
  ;; comme make-MLP (mlp.lisp) : l'expansion est du CODE qui construit
  ;; l'instance au chargement, pas une instance littérale.
  (let ((form `(make-instance 'transformer
                 :name ',name
                 :in-size ,in
                 :out-size ,out
                 :model-size ,model-size
                 :heads ,heads
                 :ff-size ,ff-size
                 :layers ,layers
                 :window ,window
                 :causal ,causal
                 :net (init-transformer-net ,in ,out
                                            :model-size ,model-size :heads ,heads
                                            :ff-size ,ff-size :layers ,layers)
                 :creation-date (get-universal-time))))
    (cond ((not (boundp name))
           `(defvar ,name ,form))
          ((ann-p (symbol-value name))
           (warning-msg (format nil "~S already exists ! ~S"
                                name (type-of (symbol-value name)))))
          (t `(setf ,name ,form)))))

;;; Briques
;;;
;;; DOT-PRODUCT, MATRIX-VECTOR, RETROPROPAGATE-SIGNAL, TRANSFER-SIGNAL-ERROR,
;;; ACCUMULATE-WEIGHTS, WITH-BIAS, POSITIONAL-ENCODING, LAYER-NORM,
;;; LAYER-NORM-RETROPROPAGATION, UPDATE-LAYER-NORM : promues dans maths.lisp
;;; (pures, generiques, sans rapport avec une architecture precise) --
;;; utilisees ici telles quelles, voir l'en-tete du fichier. Ne restent ici
;;; que les deux briques propres au mecanisme d'attention multi-tetes.

(defun head-slice (v h dk)
  (subseq v (* h dk) (* (1+ h) dk)))

(defun visible-positions (i n causal)
  "Positions que la position I peut regarder."
  (if causal (1+ i) n))

;;; Propagation

(defun transformer-forward (self net frames)
  "Propagation de la séquence FRAMES dans NET. Renvoie la liste des sorties,
une par position, et en second valeur tout ce qu'il faut garder pour la
rétropropagation."
  (destructuring-bind (w-in blocks ln-f w-out) net
    (let* ((n (length frames))
           (d (model-size self))
           (nh (heads self))
           (dk (floor d nh))
           (sqrt-dk (sqrt (float dk)))
           (inputs (mapcar #'with-bias frames))
           (x (loop for in in inputs
                    for pos from 0
                    collect (add-two-vectors (matrix-vector w-in in)
                                         (positional-encoding pos d))))
           (block-caches '())
           (attention '()))
      (assert (<= n (window self)) ()
              "~S : séquence de ~D trames, fenêtre de ~D." self n (window self))
      (dolist (blk blocks)
        (destructuring-bind (wq wk wv wo w1 w2 ln1 ln2) blk
          (let (u xhat1 isig1 q k v a z u2 xhat2 isig2 u2b h hb)
            ;; 1- auto-attention multi-têtes
            (loop for xt in x
                  do (multiple-value-bind (y xh isig) (layer-norm xt ln1)
                       (push y u) (push xh xhat1) (push isig isig1)))
            (setf u (nreverse u) xhat1 (nreverse xhat1) isig1 (nreverse isig1)
                  q (mapcar #'(lambda (ut) (matrix-vector wq ut)) u)
                  k (mapcar #'(lambda (ut) (matrix-vector wk ut)) u)
                  v (mapcar #'(lambda (ut) (matrix-vector wv ut)) u))
            ;; a : par tête, une ligne de poids par position (softmax des scores)
            (setf a (loop for hd below nh
                          collect (loop for i below n
                                        for qi = (head-slice (nth i q) hd dk)
                                        collect (softmax
                                                 (loop for j below (visible-positions i n (causal self))
                                                       collect (/ (dot-product qi (head-slice (nth j k) hd dk))
                                                                  sqrt-dk)))))
                  ;; z : moyenne des valeurs pondérée par l'attention, têtes concaténées
                  z (loop for i below n
                          collect (loop for hd below nh
                                        append (let ((zi (make-list dk :initial-element 0.0)))
                                                 (loop for aij in (nth i (nth hd a))
                                                       for vj in v
                                                       do (setf zi (add-two-vectors zi (multiply-vector aij (head-slice vj hd dk)))))
                                                 zi))))
            (setf x (mapcar #'(lambda (xt zt) (add-two-vectors xt (matrix-vector wo zt))) x z))
            ;; 2- MLP position par position
            (loop for xt in x
                  do (multiple-value-bind (y xh isig) (layer-norm xt ln2)
                       (push y u2) (push xh xhat2) (push isig isig2)))
            (setf u2 (nreverse u2) xhat2 (nreverse xhat2) isig2 (nreverse isig2)
                  u2b (mapcar #'with-bias u2)
                  h (mapcar #'(lambda (ub) (funcall (hidden-fun self) (matrix-vector w1 ub))) u2b)
                  hb (mapcar #'with-bias h)
                  x (mapcar #'(lambda (xt hbt) (add-two-vectors xt (matrix-vector w2 hbt))) x hb))
            (push a attention)
            (push (list :u u :xhat1 xhat1 :isig1 isig1 :q q :k k :v v :a a :z z
                        :u2b u2b :xhat2 xhat2 :isig2 isig2 :h h :hb hb)
                  block-caches))))
      ;; normalisation finale et projection de sortie
      (let (uf xhatf isigf ufb y)
        (loop for xt in x
              do (multiple-value-bind (yy xh isig) (layer-norm xt ln-f)
                   (push yy uf) (push xh xhatf) (push isig isigf)))
        (setf uf (nreverse uf) xhatf (nreverse xhatf) isigf (nreverse isigf)
              ufb (mapcar #'with-bias uf)
              y (mapcar #'(lambda (ub) (funcall (out-fun self) (matrix-vector w-out ub)))
                        ufb))
        (setf (attention-map self) (nreverse attention))
        (values y
                (list :inputs inputs :blocks (nreverse block-caches)
                      :xhatf xhatf :isigf isigf :ufb ufb))))))

(defun transformer-sequence (self in)
  "La séquence à propager : IN, ou (input self), en listes, limitée à WINDOW."
  (let ((frames (mapcar #'(lambda (f) (coerce f 'list)) (or in (input self)))))
    (last frames (window self))))

(defmethod check-shape ((self transformer) &key input goal)
  "INPUT et GOAL sont des SEQUENCES de trames (une par position), pas une
seule trame : chaque trame d'INPUT doit avoir IN-SIZE valeurs, chaque trame
de GOAL, OUT-SIZE -- sinon l'erreur (une multiplication matrice x vecteur
de tailles incompatibles) remonte de loin, a l'interieur de
TRANSFORMER-FORWARD/BACKPROPAGATE, sans dire quelle trame ni pourquoi (c'est
ce qui est arrive avec les poids d'ACDLL-TRANSFORMER restes au vocabulaire
d'accords d'avant l'ajout de la Version 2 -- cf. fred/lambda-calcul-neuromuse.md)."
  (loop for frame in input
        for i from 0
        unless (= (length frame) (in-size self))
          do (error "~S attend des trames d'entree de longueur ~D ; la trame ~
~D de la sequence en a ~D." self (in-size self) i (length frame)))
  (loop for frame in goal
        for i from 0
        unless (= (length frame) (out-size self))
          do (error "~S attend des trames de but de longueur ~D ; la trame ~
~D du but en a ~D." self (out-size self) i (length frame)))
  self)

(defmethod run-transformer ((self transformer) &key in)
  "Sorties du transformer pour la séquence IN (défaut (input self)).
Met à jour (outputs self) et (output self), la sortie de la dernière
position, et renvoie celle-ci."
  (let* ((frames (transformer-sequence self in))
         (net (noise self (net-temp self))))
    (check-shape self :input frames)
  (let* ((y (transformer-forward self net frames)))
    ;; température de sortie, comme logistic ou linear avec :temp dans run-mlp :
    ;; chaque valeur est remplacée par du hasard avec la probabilité TEMP
    (when (and (temp self) (plusp (temp self)))
      (setf y (mapcar #'(lambda (yt)
                          (mapcar #'(lambda (o) (if (temp-value (temp self)) (random 1.0) o)) yt))
                      y)))
    (setf (outputs self) y
          (output self) (car (last y)))
    (values (output self)))))

(defmethod activation ((self transformer) &key neuron n)
  ;; &key n : lambda-liste du defgeneric de som.lisp, qui redéfinit celui
  ;; de neuromuse-main.lisp (&key neuron)
  (declare (ignore neuron n))
  (run-transformer self :in (input self)))

(defmethod push-input ((self transformer) frame)
  "Ajoute FRAME à la fin de la séquence d'entrée (fenêtre glissante de WINDOW
trames) et propage : usage en temps réel, trame par trame (voir udp.lisp)."
  (setf (input self) (last (append (input self) (list (coerce frame 'list)))
                           (window self)))
  (run-transformer self))

;;; Rétropropagation

(defmethod backpropagate ((self transformer) &optional in)
  "Un pas d'apprentissage sur la séquence IN (défaut (input self)) vers les
buts (goal self), une trame de but par position. Renvoie l'erreur, somme des
CHECK-ERROR de chaque position."
  (let* ((frames (transformer-sequence self in))
         (goals (last (goal self) (length frames)))
         (learn (learn-fact self))
         (radius (radius self))
         (net (noise self (* learn (net-temp self))))
         (d (model-size self))
         (nh (heads self))
         (dk (floor d nh))
         (sqrt-dk (sqrt (float dk)))
         (n (length frames)))
    (check-shape self :input frames :goal goals)
    (assert (= (length goals) n) ()
            "~S : ~D trames d'entrée mais ~D buts." self n (length goals))
    (setf (previous self) (copy-tree (net self)))
    (multiple-value-bind (y cache) (transformer-forward self net frames)
      (destructuring-bind (w-in blocks ln-f w-out) net
        (let* (;; sortie : même signal d'erreur que output-signal-error de mlp.lisp
               (dy (mapcar #'(lambda (yt gt)
                               (transfer-signal-error (out-fun self) yt (substract-2-vectors gt yt)))
                           y goals))
               (duf (mapcar #'(lambda (dyt) (butlast (retropropagate-signal w-out dyt))) dy))
               (new-w-out (accumulate-weights w-out dy (getf cache :ufb) learn :radius radius))
               (dx '())
               (d-gain-f (make-list d :initial-element 0.0))
               (d-bias-f (make-list d :initial-element 0.0))
               (new-blocks '()))
          (loop for dut in duf
                for xh in (getf cache :xhatf)
                for isig in (getf cache :isigf)
                do (multiple-value-bind (dxt dg db) (layer-norm-retropropagation dut xh isig ln-f)
                     (push dxt dx)
                     (setf d-gain-f (add-two-vectors d-gain-f dg)
                           d-bias-f (add-two-vectors d-bias-f db))))
          (setf dx (nreverse dx))
          ;; blocs, du dernier au premier
          (loop for blk in (reverse blocks)
                for bc in (reverse (getf cache :blocks))
                do (destructuring-bind (wq wk wv wo w1 w2 ln1 ln2) blk
                     (let* ((d-gain1 (make-list d :initial-element 0.0))
                            (d-bias1 (make-list d :initial-element 0.0))
                            (d-gain2 (make-list d :initial-element 0.0))
                            (d-bias2 (make-list d :initial-element 0.0))
                            ;; 2- MLP : exactement la rétropropagation de mlp.lisp
                            (dh (mapcar #'(lambda (dxt ht)
                                            (transfer-signal-error (hidden-fun self) ht
                                                                   (butlast (retropropagate-signal w2 dxt))))
                                        dx (getf bc :h)))
                            (du2 (mapcar #'(lambda (dht) (butlast (retropropagate-signal w1 dht))) dh))
                            (new-w2 (accumulate-weights w2 dx (getf bc :hb) learn :radius radius))
                            (new-w1 (accumulate-weights w1 dh (getf bc :u2b) learn :radius radius))
                            dq dk-sig dv dz)
                       (setf dx (loop for dxt in dx
                                      for dut in du2
                                      for xh in (getf bc :xhat2)
                                      for isig in (getf bc :isig2)
                                      collect (multiple-value-bind (dxn dg db)
                                                  (layer-norm-retropropagation dut xh isig ln2)
                                                (setf d-gain2 (add-two-vectors d-gain2 dg)
                                                      d-bias2 (add-two-vectors d-bias2 db))
                                                (add-two-vectors dxt dxn))))
                       ;; 1- attention
                       (setf dz (mapcar #'(lambda (dxt) (retropropagate-signal wo dxt)) dx))
                       (let ((q (getf bc :q)) (k (getf bc :k)) (v (getf bc :v)) (a (getf bc :a))
                             (dq-h '()) (dk-h '()) (dv-h '()))
                         (dotimes (hd nh)
                           (let ((dqh (loop repeat n collect (make-list dk :initial-element 0.0)))
                                 (dkh (loop repeat n collect (make-list dk :initial-element 0.0)))
                                 (dvh (loop repeat n collect (make-list dk :initial-element 0.0))))
                             (dotimes (i n)
                               (let* ((ai (nth i (nth hd a)))
                                      (dzi (head-slice (nth i dz) hd dk))
                                      (qi (head-slice (nth i q) hd dk))
                                      ;; signal sur les poids d'attention, puis à travers le softmax
                                      (dai (loop for j below (length ai)
                                                 collect (dot-product dzi (head-slice (nth j v) hd dk))))
                                      (mean (dot-product ai dai))
                                      (dsi (mapcar #'(lambda (aij daij) (/ (* aij (- daij mean)) sqrt-dk))
                                                   ai dai)))
                                 (loop for j from 0
                                       for aij in ai
                                       for dsij in dsi
                                       for kj = (head-slice (nth j k) hd dk)
                                       do (setf (nth j dvh) (add-two-vectors (nth j dvh) (multiply-vector aij dzi))
                                                (nth i dqh) (add-two-vectors (nth i dqh) (multiply-vector dsij kj))
                                                (nth j dkh) (add-two-vectors (nth j dkh) (multiply-vector dsij qi))))))
                             (push dqh dq-h) (push dkh dk-h) (push dvh dv-h)))
                         ;; têtes concaténées, dans l'ordre
                         (flet ((concat-heads (per-head)
                                  (loop for i below n
                                        collect (loop for hs in (reverse per-head) append (nth i hs)))))
                           (setf dq (concat-heads dq-h)
                                 dk-sig (concat-heads dk-h)
                                 dv (concat-heads dv-h))))
                       (let* ((u (getf bc :u))
                              (new-wo (accumulate-weights wo dx (getf bc :z) learn :radius radius))
                              (new-wq (accumulate-weights wq dq u learn :radius radius))
                              (new-wk (accumulate-weights wk dk-sig u learn :radius radius))
                              (new-wv (accumulate-weights wv dv u learn :radius radius)))
                         (setf dx (loop for dxt in dx
                                        for dqt in dq
                                        for dkt in dk-sig
                                        for dvt in dv
                                        for xh in (getf bc :xhat1)
                                        for isig in (getf bc :isig1)
                                        collect (let ((dut (add-two-vectors (retropropagate-signal wq dqt)
                                                                        (add-two-vectors (retropropagate-signal wk dkt)
                                                                                     (retropropagate-signal wv dvt)))))
                                                  (multiple-value-bind (dxn dg db)
                                                      (layer-norm-retropropagation dut xh isig ln1)
                                                    (setf d-gain1 (add-two-vectors d-gain1 dg)
                                                          d-bias1 (add-two-vectors d-bias1 db))
                                                    (add-two-vectors dxt dxn)))))
                         (push (list new-wq new-wk new-wv new-wo new-w1 new-w2
                                     (update-layer-norm ln1 d-gain1 d-bias1 learn)
                                     (update-layer-norm ln2 d-gain2 d-bias2 learn))
                               new-blocks)))))
          ;; projection d'entrée (l'encodage de position n'a pas de poids)
          (setf (net self)
                (list (accumulate-weights w-in dx (getf cache :inputs) learn)
                      new-blocks
                      (update-layer-norm ln-f d-gain-f d-bias-f learn)
                      new-w-out)
                (outputs self) y
                (output self) (car (last y))
                (current-error self)
                (loop for yt in y
                      for gt in goals
                      sum (check-error yt gt (threshold self))))
          (values (current-error self)))))))

(defmethod learn ((self transformer))
  "Un pas d'apprentissage sur (input self) et (goal self)."
  (backpropagate self))

(defmethod train-transformer ((self transformer) sequences goals &key verbose)
  "Apprentissage par époques de la liste de SEQUENCES vers la liste de GOALS
(une liste de trames de but par séquence), jusqu'à ce que l'erreur moyenne
d'une époque soit <= (threshold self), ou après (stop self) époques.
Comme train-perceptron (depuis la généralisation nodes/edges/current-error,
cf. fred/lambda-calcul-neuromuse.md) : chaque erreur d'époque est empilée
dans (history-error self), lisible ensuite via (car (history-error self)) ou
la liste renvoyée en seconde valeur -- (current-error self), lui, continue de
porter l'erreur du dernier pas de BACKPROPAGATE (pas la moyenne d'époque),
interrogeable à tout instant pendant l'apprentissage, comme pour toutes les
autres architectures. Renvoie le transformer et la liste des erreurs par
époque."
  (assert (= (length sequences) (length goals)))
  (let ((errors '()))
    (dotimes (e (stop self))
      (let ((err (/ (loop for s in sequences
                          for g in goals
                          sum (progn (setf (goal self) g)
                                     (backpropagate self s)))
                    (length sequences))))
        (incf (epoch self))
        (push err errors)
        (push err (history-error self))
        (when (and verbose (zerop (mod (epoch self) verbose)))
          (format t "~&Epoch ~D : erreur ~,4F" (epoch self) err))
        (when (<= err (threshold self))
          (setf (last-stop self) (list (epoch self) 'completed))
          (return-from train-transformer (values self (nreverse errors))))))
    (setf (last-stop self) (list (epoch self) 'interrupted))
    (values self (nreverse errors))))

(defun next-frame-goals (sequence)
  "Buts de la prédiction de la trame suivante : la séquence décalée d'une
position. Renvoie deux valeurs : les entrées (SEQUENCE sans sa dernière trame)
et les buts (SEQUENCE sans sa première)."
  (values (butlast sequence) (rest sequence)))

(defmethod generate ((self transformer) seed n &key (decode #'identity))
  "Génération auto-régressive : part de la séquence SEED, propage, transforme
la sortie de la dernière position en trame d'entrée avec DECODE (par défaut
telle quelle, si IN-SIZE = OUT-SIZE), l'ajoute à la séquence, et recommence
N fois (fenêtre glissante de WINDOW trames). Renvoie les N trames générées."
  (let ((sequence (mapcar #'(lambda (f) (coerce f 'list)) seed))
        (generated '()))
    (dotimes (i n (nreverse generated))
      (let ((frame (coerce (funcall decode (run-transformer self :in (last sequence (window self))))
                           'list)))
        (push frame generated)
        (setf sequence (append sequence (list frame)))))))

;;; Divers

(defmethod clear ((self transformer))
  (setf (previous self) (net self)
        (net self) (init-transformer-net (in-size self) (out-size self)
                                         :model-size (model-size self) :heads (heads self)
                                         :ff-size (ff-size self) :layers (layers self))
        (epoch self) 0
        (last-stop self) '(0 nil)
        (current-error self) 1000
        (last-error self) 1000
        (history-error self) '()
        (history self) '()
        (creation-date self) (get-universal-time)
        (modification-date self) '())
  (format t "~%Transformer ~S cleared.~%" (name self))
  self)

;; FUNCTION-DESIGNATOR a demenage dans read-write.lisp (meme bug a
;; resoudre pour SAVE du mlp, cf. doc/formats.md) -- reprise ici telle quelle.

(defmethod save ((self transformer) &optional path)
  "Comme save pour mlp, mais écrit les fonctions sous la forme #'nom et les
symboles quotés, pour que le fichier se relise tel quel."
  (when (not path) (setf path (format nil "~(~A~).lisp" (name self))))
  (let ((slots (structure-slot-names (type-of self))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "(in-package :neuromuse)")
      (format stream "~&(make-instance 'transformer")
      (loop for s in slots
            do (let ((slot-value (slot-value self s)))
                 (cond ((functionp slot-value)
                        (format stream " :~A #'~S~&" s (function-designator slot-value)))
                       ((or (listp slot-value) (symbolp slot-value))
                        (format stream " :~A '~S~&" s slot-value))
                       (t (format stream " :~A ~S~&" s slot-value)))))
      (format stream ")~%")))
  (format t "~& Transformer ~S saved to file ~S !" (name self) path)
  (values))

;;; Graphe (EDGES/NODES, cf. neuromuse-main.lisp) et export DOT
;;; (read-write.lisp) -- memes conventions que pour les autres architectures,
;;; mais le squelette statique de poids (EDGES, ci-dessous) et le melange par
;;; attention (TRANSFORMER-ATTENTION-EDGES, plus bas) sont deux choses
;;; differentes : EDGES est une fonction pure de (net self) seul, comme pour
;;; toute autre classe (necessaire pour que NODES, deduit mecaniquement de
;;; EDGES, et DOT-SOURCE, generique, marchent sans rien ecrire de special
;;; ici) -- l'attention, elle, depend de la sequence passee au dernier
;;; RUN-TRANSFORMER/BACKPROPAGATE (ATTENTION-MAP), donc hors de EDGES, avec
;;; sa propre fonction et son propre mode de WRITE-DOT.

(defmethod edges ((self transformer))
  "Le squelette statique de poids de SELF -- fonction pure de (net self)
seul, meme contrat que pour mlp/som/hopfield (cf. neuromuse-main.lisp).
NET = (W-IN BLOCS LN-FINALE W-OUT), chaque bloc (WQ WK WV WO W1 W2 LN1 LN2)
-- voir la classe. Noeuds :
  (:in j)               entree, j = IN-SIZE pour le biais
  (:x b i)              flux residuel entrant dans le bloc b, b de 0 a
                        LAYERS inclus (LAYERS = sortie du dernier bloc,
                        avant W-OUT)
  (:q b i) (:k b i) (:v b i)   projections Q/K/V du bloc b
  (:attn-out b i)       apres WO
  (:ff-hidden b i)      couche cachee du MLP du bloc b, i = FF-SIZE pour
                        son biais
  (:out i)              sortie finale
Trois simplifications deliberees par rapport a TRANSFORMER-FORWARD :
LN1/LN2/LN-FINALE n'ont pas d'arc (un gain/biais par dimension, pas une
connexion entre deux noeuds -- meme raison que SLOPE/THRESHOLD n'en ont pas
pour un mlp) ; les connexions residuelles (x += ...) ne sont pas des arcs
identite separes, seulement implicites dans le fait que (:x (1+ b) i) et
(:x b i) partagent le meme indice i ; WO va directement de V a ATTN-OUT,
sans modeliser le melange par attention entre les deux -- ce melange depend
de l'entree, pas seulement de NET, donc hors de EDGES par construction (cf.
TRANSFORMER-ATTENTION-EDGES, qui le represente pour une sequence donnee)."
  (destructuring-bind (w-in blocks ln-final w-out) (net self)
    (declare (ignore ln-final))
    (flet ((matrix-edges (matrix source-tag target-tag)
             (loop for row in matrix
                   for i from 0
                   append (loop for w in row
                                for j from 0
                                collect (list (funcall source-tag j) (funcall target-tag i) w)))))
      (append
       (matrix-edges w-in (lambda (j) (list :in j)) (lambda (i) (list :x 0 i)))
       (loop for blk in blocks
             for b from 0
             append (destructuring-bind (wq wk wv wo w1 w2 ln1 ln2) blk
                      (declare (ignore ln1 ln2))
                      (append
                       (matrix-edges wq (lambda (j) (list :x b j)) (lambda (i) (list :q b i)))
                       (matrix-edges wk (lambda (j) (list :x b j)) (lambda (i) (list :k b i)))
                       (matrix-edges wv (lambda (j) (list :x b j)) (lambda (i) (list :v b i)))
                       (matrix-edges wo (lambda (j) (list :v b j)) (lambda (i) (list :attn-out b i)))
                       (matrix-edges w1 (lambda (j) (list :attn-out b j)) (lambda (i) (list :ff-hidden b i)))
                       (matrix-edges w2 (lambda (j) (list :ff-hidden b j)) (lambda (i) (list :x (1+ b) i))))))
       (matrix-edges w-out (lambda (j) (list :x (length blocks) j)) (lambda (i) (list :out i)))))))

(defun transformer-attention-edges (self &key (head 0))
  "Les arcs d'attention de SELF pour la tete HEAD (0 par defaut), lus dans
(attention-map self) -- PAS une fonction pure de (net self) seul, a la
difference de EDGES ci-dessus : reflete la derniere sequence passee a
RUN-TRANSFORMER/BACKPROPAGATE, NIL avant le premier appel (d'ou l'erreur
explicite dans WRITE-DOT plus bas, meme precedent que ROSOM/HOPFIELD pour un
cas pas encore utilisable). Noeuds (:pos b i), b l'indice du bloc, i la
position dans la sequence. Arc (:pos b j) -> (:pos b i), poids = la
probabilite d'attention de la position i vers la position j (bloc b, tete
HEAD) -- cf. TRANSFORMER-FORWARD : (nth head (nth b (attention-map self)))
est une liste d'une ligne par position i, chaque ligne la sortie de softmax
sur les positions visibles depuis i (longueur i+1 sous CAUSAL, N sinon --
un simple reflet de la longueur de chaque ligne, rien a filtrer
explicitement ici pour respecter le masque causal : aucun j > i n'apparait
jamais dans ATTENTION-MAP)."
  (loop for block-attn in (attention-map self)
        for b from 0
        append (let ((head-attn (nth head block-attn)))
                 (loop for row in head-attn
                       for i from 0
                       append (loop for w in row
                                    for j from 0
                                    collect (list (list :pos b j) (list :pos b i) w))))))

(defun transformer-attention-dot-source (ann &key (head 0) color (edge-label t) (node-label t)
                                                (scale 2.0) dark)
  "La source DOT des arcs d'attention de ANN (TRANSFORMER-ATTENTION-EDGES)
pour la tete HEAD -- positions imposees (POS=\"x,y!\"), bloc 0 en haut
(y = -b), position croissante vers la droite (x = i) -- rendre avec
ENGINE \"neato\" pour qu'elles soient respectees, meme precedent que
WRITE-DOT sur SOM/HOPFIELD. COLOR/EDGE-LABEL : cf. EDGE-ATTRS, meme palette
DIVERGING-HEX -- toujours dans sa moitie positive ici (jaune a rouge), un
poids d'attention (softmax) n'etant jamais negatif : jaune = attention
faible, rouge = forte. NODE-LABEL (t par defaut) affiche l'indice de
position seul ; le bloc se lit deja sur l'axe vertical, pas besoin de le
repeter dans le label."
  (let* ((edges (transformer-attention-edges ann :head head))
         (nodes (remove-duplicates (loop for (s tg w) in edges append (list s tg)) :test #'equal))
         (max-w (if (and color edges) (reduce #'max edges :key (lambda (e) (abs (third e)))) 1)))
    (flet ((dot-id (node) (dot-quote (princ-to-string node))))
      (with-output-to-string (stream)
        (format stream "digraph ~A {~%" (dot-quote (string-downcase (string (name ann)))))
        (write-string (dark-prelude dark) stream)
        (dolist (node nodes)
          (destructuring-bind (tag b i) node
            (declare (ignore tag))
            (format stream "  ~A [pos=~A, label=~A];~%"
                    (dot-id node)
                    (dot-quote (format nil "~F,~F!" (* scale i) (* scale (- b))))
                    (dot-quote (if node-label (princ-to-string i) "")))))
        (dolist (edge edges)
          (format stream "  ~A -> ~A ~A;~%"
                  (dot-id (first edge)) (dot-id (second edge))
                  (edge-attrs (third edge) :color color :edge-label edge-label :max-weight max-w)))
        (format stream "}~%")))))

(defmethod write-dot ((ann transformer) &optional path
                       &key (format :dot) (engine "dot") (mode :weights) (head 0)
                         color (edge-label t) (node-label t) (scale 2.0) dark)
  "Deux modes, pas deux methodes (comme MODE :u-matrix/:component sur SOM) :
MODE :weights (par defaut) -- le squelette statique de poids (EDGES,
  ci-dessus), vu generiquement par DOT-SOURCE (read-write.lisp), exactement
  comme pour mlp/perceptron. ENGINE \"dot\" par defaut (disposition
  hierarchique, entree en haut -- EDGES chaine les blocs dans cet ordre).
MODE :attention -- le melange par attention de la tete HEAD (0 par defaut),
  TRANSFORMER-ATTENTION-DOT-SOURCE -- erreur si (attention-map ann) est vide
  (RUN-TRANSFORMER/BACKPROPAGATE pas encore appele). ENGINE \"neato\"
  conseille (pas impose : contrairement a SOM/HOPFIELD, ce n'est pas le seul
  mode de cette methode) pour que les positions imposees soient respectees ;
  avec \"dot\" (le defaut de la methode), le moteur ignore POS mais produit
  quand meme une disposition hierarchique raisonnable (le graphe reste un
  DAG, un bloc = un rang).
COLOR/EDGE-LABEL/NODE-LABEL/DARK : cf. EDGE-ATTRS/DOT-SOURCE/DARK-PRELUDE.
SCALE (MODE :attention seulement) : cf. WRITE-DOT sur SOM/HOPFIELD."
  (when (not path) (setf path (format nil "~(~A~).~(~A~)" (name ann) format)))
  (let ((dot-text
          (ecase mode
            (:weights (dot-source ann :color color :edge-label edge-label
                                   :node-label node-label :dark dark))
            (:attention
             (unless (attention-map ann)
               (error "(attention-map ~S) est vide -- appeler RUN-TRANSFORMER ~
ou BACKPROPAGATE d'abord (MODE :attention lit la derniere sequence propagee)."
                      ann))
             (transformer-attention-dot-source ann :head head :color color
                                                :edge-label edge-label :node-label node-label
                                                :scale scale :dark dark)))))
    (render-dot dot-text path format engine))
  (format t "~& ~S : graphe ecrit dans ~S~@[, rendu par ~A~]"
          (name ann) path (unless (string-equal (string format) "dot") engine))
  (values))

;; Meme sweep que la fin de src/udp.lisp (UDP.LISP a deja tourne avant que ce
;; fichier existe, donc ses symboles -- MAKE-TRANSFORMER, RUN-TRANSFORMER...
;; -- ne sont pas passes par le premier sweep) : sans ca, un package qui :USE
;; :NEUROMUSE apres avoir charge ce fichier (:neuromuse-demos, par exemple)
;; ne les verrait pas, puisque :USE n'herite que des symboles deja exportes
;; au moment ou il est declare.
(do-symbols (sym (find-package :neuromuse))
  (when (eq (symbol-package sym) (find-package :neuromuse))
    (export sym :neuromuse)))

;; EOF
