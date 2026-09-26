;;;; gui.lisp - Visualisation Ltk (Tk) d'un MLP : les poids synaptiques,
;;;; au choix comme une heatmap (chaud/rouge = poids positif, froid/bleu =
;;;; poids negatif) ou comme un graphe de neurones (entree en haut, sortie
;;;; en bas, chaque lien colore comme dans la heatmap, et les cercles d'entree
;;;; et de sortie remplis en gris selon leur activite) -- un bouton bascule
;;;; de l'une a l'autre -- plus la courbe d'evolution de l'erreur et des
;;;; lignes d'etat (epoch, erreur courante, learn-fact, temperatures,
;;;; in / out / goal).
;;;;
;;;; La fenetre LIT l'instance a intervalle regulier ; elle n'ecrit jamais
;;;; dedans. Il n'y a donc rien a changer a une boucle d'apprentissage pour
;;;; la suivre a l'ecran : il suffit qu'elle empile ses erreurs dans
;;;; (history-error mlp), comme le fait examples/mlp-test.lisp.
;;;;
;;;; Dependances : Tk (Debian/Ubuntu : sudo apt install tk) et Ltk
;;;; (Quicklisp), volontairement tenus hors du systeme :neuromuse lui-meme,
;;;; qui doit rester utilisable sans interface graphique.
;;;;
;;;; Chargement :
;;;;   (ql:quickload :ltk)                ; une fois
;;;;   (asdf:load-system "neuromuse/gui")  ; cf. neuromuse.asd
;;;; ou, a la main comme perceptron.lisp : (load "src/gui.lisp")
;;;;
;;;; Usage au REPL, package :neuromuse courant :
;;;;   (make-mlp xor 2 1 2)
;;;;   (setf (learn-fact xor) .4)
;;;;   (neuromuse-gui:gui xor)            ; non bloquant : le REPL reste libre
;;;;   (load "examples/mlp-test.lisp")    ; la fenetre suit l'apprentissage
;;;;
;;;; (neuromuse-gui:demo) ouvre la meme fenetre sur des poids factices, pour
;;;; verifier l'affichage sans reseau. Le bouton "vue" bascule entre grille
;;;; (heatmap) et graphe de neurones ; (neuromuse-gui:gui xor :view :graph)
;;;; l'ouvre directement dans l'autre vue.
;;;;
;;;; THREADS : tout le dialogue avec Tk a lieu dans le thread de la fenetre,
;;;; qui se reveille lui-meme (ltk:after) pour relire le reseau. Ltk n'est pas
;;;; thread-safe : ne pas appeler refresh / draw-error-plot / ... depuis le
;;;; thread d'apprentissage.

(defpackage :neuromuse-gui
  ;; comme neuromuse-test : un package a part, qui use :neuromuse pour lire
  ;; les reseaux avec les accesseurs non qualifies (net, history-error...),
  ;; les symboles Tk restant eux prefixes ltk:.
  (:use :cl :neuromuse)
  (:export #:gui
           #:launch-gui
           #:demo
           #:gui-snapshot
           #:make-snapshot
           #:*interval*))

(in-package :neuromuse-gui)

;;; ------------------------------------------------------------------
;;; 1. Ce que la fenetre lit dans le reseau
;;; ------------------------------------------------------------------
;;;
;;; Le net d'un mlp EST deja la structure a dessiner : une liste de matrices,
;;; une par transition de couche, chaque matrice une liste de lignes, une
;;; ligne par neurone de la couche haute, contenant ses poids entrants
;;; (cf. init-mlp-net dans mlp.lisp). Rien a convertir donc ; un seul
;;; generique, GUI-SNAPSHOT, isole la fenetre du type de reseau : ecrire une
;;; methode de plus suffit a brancher une autre classe (som, rosom...). La
;;; heatmap (section 3) et le graphe de neurones (section 3bis) dessinent
;;; toutes deux directement cette meme structure, juste sous deux formes.

(defstruct snapshot
  "Photographie de l'etat a dessiner, prise en une passe."
  (weights '())     ; liste de matrices de poids, comme (net mlp)
  (errors '())      ; suite des erreurs, la plus ANCIENNE en tete
  (status "")       ; premiere ligne : identite, topologie, reglages
  (signals "")      ; seconde ligne : epoch, erreur, entree / sortie / but
  (inputs '())      ; valeurs presentees a la couche d'entree (gris, vue graphe)
  (outputs '())     ; valeurs de la couche de sortie (gris, vue graphe)
  (threshold nil))  ; seuil d'erreur a marquer sur la courbe, ou NIL

(defgeneric gui-snapshot (source)
  (:documentation "Retourne un SNAPSHOT de SOURCE pour la fenetre."))

(defgeneric reset-gui-errors (source)
  (:documentation "Vide l'historique d'erreur de SOURCE -- ce que le bouton
\"effacer erreur\" de la fenetre declenche. Une methode par classe de
source, comme GUI-SNAPSHOT, puisque l'erreur n'est pas rangee au meme
endroit partout (HISTORY-ERROR pour un ann, une liste maison pour DEMO)."))

(defmethod reset-gui-errors ((source ann))
  (setf (history-error source) nil))

(defun round3 (x)
  (if (realp x) (round1 x 3) x))

(defun vector-string (v &optional (limit 8))
  "Vecteur abrege : au plus LIMIT valeurs arrondies, pour que la ligne
d'etat reste lisible avec des reseaux a beaucoup d'entrees."
  (cond ((not (listp v)) (format nil "~A" v))
        ((null v) "()")
        ((> (length v) limit)
         (format nil "(~{~A ~}... ~D valeurs)"
                 (mapcar #'round3 (subseq v 0 limit)) (length v)))
        (t (format nil "(~{~A~^ ~})" (mapcar #'round3 v)))))

(defun network-inputs (source)
  "Valeurs presentees a la couche d'entree de SOURCE : (input source), suivies
pour un rmlp de l'activation recurrente que BACKPROPAGATE / RUN-MLP lui
concatenent avant la premiere matrice. Attention, pour ce contexte c'est
l'activation la plus RECENTE, celle qui alimentera le prochain pas : la passe
qui vient d'etre faite a utilise la precedente, qui n'est pas conservee."
  (let ((in (input source)))
    (if (and (typep source 'rmlp) (listp in))
        (append in (recurrent-layer-activation source))
        in)))

(defmethod gui-snapshot ((source mlp))
  (make-snapshot
   ;; copy-list : backpropagate remplace les matrices une a une dans
   ;; (net mlp) ; on fige l'epine dorsale de la liste pour dessiner un etat
   ;; coherent meme si l'apprentissage tourne dans un autre thread.
   :weights (copy-list (net source))
   ;; history-error est empilee par push (la plus recente en tete) par les
   ;; boucles d'apprentissage : cf. examples/mlp-test.lisp.
   :errors (remove-if-not #'realp (reverse (history-error source)))
   ;; premiere ligne : les reglages ; seconde : l'etat courant.
   :status (format nil "~A : ~D-~{~D~^-~}-~D | learn ~,3F | temp ~,2F / net-temp ~,2F | seuil ~,4F"
                   (name source)
                   (in-size source) (hidden-size source) (out-size source)
                   (learn-fact source) (temp source) (net-temp source)
                   (threshold source))
   ;; current-error n'est mis a jour que par la methode learn (desactivee) :
   ;; l'erreur vivante est en tete d'historique, absente juste apres
   ;; RESET-GUI-ERRORS ou avant le tout premier BACKPROPAGATE -- ne pas
   ;; retomber alors sur current-error, qui resterait a sa valeur sentinelle
   ;; 9999 et se lirait comme une vraie erreur.
   :signals (format nil "epoch ~D | erreur ~A | in ~A -> out ~A | goal ~A"
                    (epoch source)
                    (let ((e (find-if #'realp (history-error source))))
                      (if e (format nil "~,5F" e) "n/d"))
                    (vector-string (input source))
                    (vector-string (output source))
                    (vector-string (goal source)))
   ;; (output source) est ecrit par BACKPROPAGATE comme par RUN-MLP, avec
   ;; l'entree courante : rien a recalculer ici. Mais pas exactement ce que
   ;; RUN-MLP donnerait sur les poids affiches : apres BACKPROPAGATE c'est la
   ;; prediction faite AVANT la mise a jour des poids de ce pas, avec
   ;; (threshold source) en decalage de chaque neurone (RUN-MLP le met a 0) et
   ;; sur des poids bruites par net-temp.
   :inputs (network-inputs source)
   :outputs (output source)
   :threshold (threshold source)))

(defun network (source)
  "SOURCE est une instance, ou le symbole auquel elle est liee : make-mlp
lie le reseau a son nom (cf. initialize-instance de ann)."
  (cond ((not (symbolp source)) source)
        ((boundp source) (symbol-value source))
        (t (error "Aucun reseau lie au symbole ~S." source))))

(defun source-title (source)
  (if (ann-p source)
      (format nil "~A" (name source))
      (string-downcase (class-name (class-of source)))))

;;; ------------------------------------------------------------------
;;; 2. Palette chaud (positif, rouge) / froid (negatif, bleu)
;;; ------------------------------------------------------------------

(defun clamp (x lo hi) (max lo (min hi x)))

(defun weight->color (w max-abs)
  "Blanc pour un poids proche de 0, rouge pour un poids fortement positif,
bleu pour un poids fortement negatif. MAX-ABS normalise l'intensite (plus
grand poids en valeur absolue du reseau courant)."
  (let* ((m (if (and (realp max-abs) (plusp max-abs)) (float max-abs 1.0) 1.0))
         (v (clamp (/ (float (if (realp w) w 0) 1.0) m) -1.0 1.0))
         (c (round (* 255 (- 1.0 (abs v))))))
    (if (minusp v)
        (format nil "#~2,'0X~2,'0Xff" c c)
        (format nil "#ff~2,'0X~2,'0X" c c))))

;;; ------------------------------------------------------------------
;;; 3. Heatmap des poids
;;; ------------------------------------------------------------------
;;;
;;; Une grille par couche, de gauche a droite ; dans une grille, une ligne
;;; par neurone et une colonne par poids entrant. Les rectangles sont crees
;;; UNE SEULE FOIS (la topologie ne change pas en cours d'apprentissage) :
;;; chaque rafraichissement ne fait que reconfigurer leur couleur, ce qui
;;; est plus sur et beaucoup plus rapide que tout detruire et recreer.

(defparameter *cell-max* 40 "Cote maximum d'une cellule de poids, en pixels.")
(defparameter *cell-min* 2 "Cote minimum d'une cellule de poids, en pixels.")
(defparameter *layer-gap* 20 "Espace horizontal entre deux couches.")
(defparameter *margin* 8)
(defparameter *label-space* 16 "Hauteur reservee aux etiquettes de couche.")
(defparameter *weights-width* 640)
(defparameter *weights-height* 250)
(defparameter *error-width* 640)
(defparameter *error-height* 170)

(defun matrix-shape (weights)
  "Empreinte de la topologie dessinee : (neurones . poids par neurone) par
couche. Sert a savoir s'il faut reconstruire les rectangles."
  (mapcar (lambda (layer) (cons (length layer) (length (car layer)))) weights))

(defun max-abs-weight (weights)
  (let ((m 0.0))
    (dolist (layer weights m)
      (dolist (neuron layer)
        (dolist (w neuron)
          (when (and (realp w) (> (abs w) m))
            (setf m (abs w))))))))

(defun cell-size (shape width height)
  "Cote d'une cellule pour que toutes les couches tiennent dans WIDTH x
HEIGHT : gros carres pour les petits reseaux, pixels pour les gros."
  (let ((cols (reduce #'+ shape :key #'cdr :initial-value 0))
        (rows (reduce #'max shape :key #'car :initial-value 1))
        (gaps (* *layer-gap* (max 0 (1- (length shape))))))
    (max *cell-min*
         (min *cell-max*
              (floor (max 1 (- width (* 2 *margin*) gaps)) (max 1 cols))
              (floor (max 1 (- height (* 2 *margin*) *label-space*)) (max 1 rows))))))

(defun heatmap-extent (shape cell)
  "Largeur et hauteur, en pixels, du dessin complet (etiquettes comprises)."
  (values (+ (* cell (reduce #'+ shape :key #'cdr :initial-value 0))
             (* *layer-gap* (max 0 (1- (length shape)))))
          (+ *label-space* (* cell (reduce #'max shape :key #'car :initial-value 0)))))

(defun build-heatmap (canvas weights cell)
  "Cree un rectangle par poids et une etiquette par couche, et retourne la
structure des rectangles, calquee sur WEIGHTS. Le dessin est centre, et le
canevas reduit a sa hauteur utile : un petit reseau ne laisse pas une grande
surface blanche vide."
  (ltk:clear canvas)
  (let* ((shape (matrix-shape weights))
         (max-rows (reduce #'max shape :key #'car :initial-value 0))
         (outline (if (>= cell 5) "#bbbbbb" ""))
         (top (+ *margin* *label-space*))
         (layers '()))
    (multiple-value-bind (total-w total-h) (heatmap-extent shape cell)
      (ltk:configure canvas :height (min *weights-height* (+ total-h (* 2 *margin*))))
      (let ((x (max *margin* (floor (- *weights-width* total-w) 2))))
        (loop for layer in weights
              for k from 1
              do (let ((y (+ top (* cell (floor (- max-rows (length layer)) 2))))
                       (rows '()))
                   ;; une ligne de rectangles par neurone, un par poids entrant
                   (dolist (neuron layer)
                     (let ((cx x)
                           (row '()))
                       (dotimes (j (length neuron))
                         (let ((r (ltk:make-rectangle canvas cx y
                                                      (+ cx cell) (+ y cell))))
                           (ltk:configure r :outline outline)
                           (push r row))
                         (incf cx cell))
                       (push (nreverse row) rows))
                     (incf y cell))
                   (ltk:configure (make-instance 'ltk:canvas-text
                                                 :canvas canvas :x x :y *margin*
                                                 :text (format nil "c~D ~Dx~D"
                                                               k (length layer)
                                                               (length (car layer))))
                                  :fill "#555555")
                   (incf x (+ (* cell (length (car layer))) *layer-gap*))
                   (push (nreverse rows) layers)))
        (nreverse layers)))))

(defun paint-heatmap (items weights max-abs)
  "Reconfigure la couleur des rectangles deja crees, sans toucher a la
geometrie."
  (mapc (lambda (layer-items layer)
          (mapc (lambda (row-items row)
                  (mapc (lambda (item w)
                          (ltk:configure item :fill (weight->color w max-abs)))
                        row-items row))
                layer-items layer))
        items weights))

;;; ------------------------------------------------------------------
;;; 3bis. Vue graphe : neurones (cercles) relies par leurs poids
;;; ------------------------------------------------------------------
;;;
;;; Meme donnees que la heatmap (WEIGHTS), presentees comme un graphe en
;;; couches : la couche d'entree en haut, la sortie en bas, une rangee par
;;; couche cachee entre les deux -- chaque poids devient le segment allant
;;; du neurone source (rangee du dessus) au neurone qu'il alimente (rangee
;;; du dessous), colore exactement comme la cellule correspondante de la
;;; heatmap (WEIGHT->COLOR, meme normalisation par MAX-ABS). Comme pour la
;;; heatmap, cercles et segments sont crees une seule fois ; rafraichir ne
;;; fait plus que reconfigurer des couleurs.
;;;
;;; En plus des poids, la vue montre l'ACTIVITE du reseau : les cercles de la
;;; couche d'entree sont remplis d'un gris proportionnel a la valeur presentee
;;; (blanc = 0, noir = 1), ceux de la couche de sortie d'un gris proportionnel
;;; a la valeur predite. Les couches cachees restent blanches : le reseau ne
;;; conserve pas leur activation (BACKPROPAGATE la garde dans une variable
;;; locale), et la recalculer ici dupliquerait la passe avant. Les valeurs
;;; sont lues telles quelles dans (input mlp) / (output mlp) -- voir
;;; GUI-SNAPSHOT pour ce que (output mlp) contient exactement.

(defparameter *neuron-radius-max* 12 "Rayon maximum d'un neurone, en pixels.")
(defparameter *neuron-radius-min* 3 "Rayon minimum d'un neurone, en pixels.")
(defparameter *row-label-width* 92
  "Largeur reservee, a gauche, a l'etiquette de chaque couche.")
(defparameter *graph-row-gap* 90
  "Ecart vertical prefere entre deux couches, en pixels : les couches restent
serrees, et la fenetre ne grandit pas inutilement pour un petit reseau.")
(defparameter *graph-height-max* 420
  "Hauteur maximum du canevas de la vue graphe ; au-dela, les couches se
resserrent encore plutot que la fenetre ne grandisse.")

(defun layer-sizes (weights)
  "Taille de chaque couche -- entree, couches cachees, sortie -- deduite de
WEIGHTS seule : la couche d'entree a autant de neurones que de colonnes
dans la premiere matrice, chaque couche suivante autant de lignes que sa
matrice de transition."
  (when weights
    (cons (length (car (first weights))) (mapcar #'length weights))))

(defun graph-geometry (sizes)
  "Pour des couches de SIZES neurones : rayon d'un neurone, ecart vertical
entre deux rangees, et hauteur du canevas qui les contient toutes, cercles
du haut et du bas compris (rien n'est rogne au bord)."
  (let* ((nrows (length sizes))
         (avail-w (max 1 (- *weights-width* *margin* *row-label-width* *margin*)))
         (max-n (reduce #'max sizes :initial-value 1))
         (r0 (min *neuron-radius-max* (floor avail-w (* 2 max-n))))
         (gap (if (> nrows 1)
                  (min *graph-row-gap*
                       (floor (- *graph-height-max* (* 2 (+ r0 *margin*)))
                              (1- nrows)))
                  0))
         (radius (max *neuron-radius-min*
                      (if (> nrows 1) (min r0 (floor gap 3)) r0))))
    (values radius gap
            (+ (* 2 (+ radius *margin*)) (* gap (max 0 (1- nrows)))))))

(defun graph-positions (sizes radius gap)
  "Position (x . y) de chaque neurone, une liste par couche du haut (entree)
vers le bas (sortie)."
  (let* ((left (+ *margin* *row-label-width*))
         (avail-w (max 1 (- *weights-width* left *margin*))))
    (loop for n in sizes
          for row from 0
          collect (let ((y (+ *margin* radius (* row gap)))
                        (spacing (/ avail-w (max 1 n))))
                    (loop for i below n
                          collect (cons (+ left (* (+ i 0.5) spacing)) y))))))

(defstruct netgraph
  "Les items Tk du graphe, calques sur WEIGHTS : NEURONS une liste de
cercles par couche, LINKS une liste de matrices de segments (meme forme que
WEIGHTS) par transition de couche."
  neurons links)

(defun layer-label (k sizes)
  (cond ((zerop k) (format nil "entree (~D)" (nth k sizes)))
        ((= k (1- (length sizes))) (format nil "sortie (~D)" (nth k sizes)))
        (t (format nil "cachee ~D (~D)" k (nth k sizes)))))

(defun build-graph (canvas weights)
  "Cree une fois les cercles et les segments du graphe, positionnes par
couche, et retourne la structure NETGRAPH correspondante. Le canevas prend
la hauteur juste necessaire a ces couches."
  (ltk:clear canvas)
  (let ((sizes (layer-sizes weights)))
    (multiple-value-bind (radius gap height) (graph-geometry sizes)
      (ltk:configure canvas :height height)
      (let ((rows (graph-positions sizes radius gap)))
        ;; les liens d'abord : les neurones se dessinent alors par-dessus
        (let ((links
                (loop for matrix in weights
                      for src-row in rows
                      for dst-row in (rest rows)
                      collect (mapcar
                               (lambda (neuron dst-pos)
                                 (mapcar (lambda (w src-pos)
                                           (declare (ignore w))
                                           (ltk:make-line
                                            canvas (list (car src-pos) (cdr src-pos)
                                                         (car dst-pos) (cdr dst-pos))))
                                         neuron src-row))
                               matrix dst-row)))
              (neurons
                (loop for positions in rows
                      collect (mapcar
                               (lambda (pos)
                                 (ltk:make-oval canvas
                                                (- (car pos) radius) (- (cdr pos) radius)
                                                (+ (car pos) radius) (+ (cdr pos) radius)))
                               positions))))
          (dolist (row neurons)
            (dolist (n row) (ltk:configure n :fill "white" :outline "#333333")))
          (loop for positions in rows
                for k from 0
                when positions
                  do (ltk:configure
                      (make-instance 'ltk:canvas-text
                                     :canvas canvas :x *margin*
                                     :y (- (cdr (first positions)) 6)
                                     :text (layer-label k sizes))
                      :fill "#555555"))
          (make-netgraph :neurons neurons :links links))))))

(defun activity->gray (v)
  "Couleur #rrggbb en gris pour une activite V : blanc pour 0, noir pour 1
(hors de [0,1], V est ramene a la borne ; blanc si ce n'est pas un nombre)."
  (if (realp v)
      (let ((c (round (* 255 (- 1.0 (clamp (float v 1.0) 0.0 1.0))))))
        (format nil "#~2,'0X~2,'0X~2,'0X" c c c))
      "white"))

(defun paint-neuron-row (items values)
  "Remplit le i-eme cercle de ITEMS selon la i-eme valeur de VALUES (blanc
s'il en manque)."
  (let ((values (if (listp values) values '())))
    (loop for item in items
          for i from 0
          do (ltk:configure item :fill (activity->gray (nth i values))))))

(defun paint-graph (g weights max-abs inputs outputs)
  "Reconfigure les couleurs des items deja crees, sans toucher a la
geometrie : segments d'apres WEIGHTS, cercles d'entree d'apres INPUTS et
cercles de sortie d'apres OUTPUTS (gris, noir = 1)."
  (mapc (lambda (layer-links matrix)
          (mapc (lambda (neuron-links neuron)
                  (mapc (lambda (link w)
                          (ltk:configure link :fill (weight->color w max-abs)))
                        neuron-links neuron))
                layer-links matrix))
        (netgraph-links g) weights)
  (let ((rows (netgraph-neurons g)))
    (when rows
      (paint-neuron-row (first rows) inputs)
      (paint-neuron-row (car (last rows)) outputs))))

;;; ------------------------------------------------------------------
;;; 4. Courbe d'evolution de l'erreur
;;; ------------------------------------------------------------------
;;;
;;; Meme principe que la heatmap : les items (axes, courbe, seuil,
;;; graduations) sont crees une fois, et chaque rafraichissement ne fait que
;;; leur donner de nouvelles coordonnees. La courbe est decimee a un point
;;; par pixel : l'historique peut faire des dizaines de milliers d'epoques
;;; sans ralentir l'affichage. L'echelle verticale est celle des erreurs
;;; reellement observees (min..max), affichee en clair pour que la pente ne
;;; soit pas trompeuse.

(defun decimate (series limit)
  "Reduit SERIES a au plus LIMIT points, pas regulier, dernier conserve."
  (let ((n (length series)))
    (if (<= n limit)
        series
        (let ((stride (ceiling n limit)))
          (loop for x in series
                for i from 0
                when (or (zerop (mod i stride)) (= i (1- n)))
                  collect x)))))

(defun plot-geometry ()
  "Coin bas-gauche du trace et taille utile, en pixels : place a gauche pour
les ordonnees, en bas pour la legende des abscisses."
  (let* ((left (+ *margin* 46))
         (bottom (- *error-height* *margin* 12)))
    (values left bottom (- *error-width* left *margin*) (- bottom *margin*))))

(defstruct plot
  "Les items Tk du graphe, crees une fois et ensuite deplaces."
  curve threshold hi-text lo-text n-text)

(defun build-error-plot (canvas)
  "Cree une fois pour toutes les items du graphe : axes, courbe, ligne de
seuil et graduations. Comme pour la heatmap, les rafraichissements suivants
ne font plus que deplacer et reconfigurer ces items : la courbe ne clignote
pas et le canal vers Tk reste court."
  (multiple-value-bind (left bottom plot-w) (plot-geometry)
    (ltk:clear canvas)
    (flet ((line (coords &rest options)
             (let ((l (ltk:make-line canvas coords)))
               (apply #'ltk:configure l options)
               l))
           (text (x y)
             (make-instance 'ltk:canvas-text :canvas canvas :x x :y y :text "")))
      (line (list left *margin* left bottom) :fill "#888888")
      (line (list left bottom (+ left plot-w) bottom) :fill "#888888")
      ;; le seuil avant la courbe : la courbe passe donc au-dessus
      (make-plot :threshold (line (list left bottom left bottom)
                                  :fill "#3399cc" :dash "." :state "hidden")
                 :curve (line (list left bottom left bottom) :fill "#dd3333")
                 :hi-text (text *margin* (- *margin* 3))
                 :lo-text (text *margin* (- bottom 11))
                 :n-text (text left (+ bottom 1))))))

(defun draw-error-plot (plot series threshold)
  "Met a jour le trace : en ordonnee les erreurs reellement observees
(min..max, affiches en clair pour que la pente ne soit pas trompeuse), en
abscisse toute l'histoire, decimee a un point par pixel."
  (multiple-value-bind (left bottom plot-w plot-h) (plot-geometry)
    (if (< (length series) 2)
        (progn
          (setf (ltk:coords (plot-curve plot)) (list left bottom left bottom))
          (ltk:configure (plot-threshold plot) :state "hidden")
          (ltk:configure (plot-hi-text plot) :text "")
          (ltk:configure (plot-lo-text plot) :text "")
          (ltk:configure (plot-n-text plot)
                         :text "pas encore d'erreur : (push e (history-error net))"))
        (let* ((points (decimate series plot-w))
               (n (length points))
               (hi (reduce #'max series))
               (lo (reduce #'min series))
               (range (max 1.0d-9 (- hi lo))))
          (flet ((px (i) (+ left (round (* i plot-w) (max 1 (1- n)))))
                 (py (e) (- bottom (round (* (- e lo) plot-h) range))))
            (let ((coords '()))
              (loop for e in points
                    for i from 0
                    do (push (px i) coords)
                       (push (py e) coords))
              (setf (ltk:coords (plot-curve plot)) (nreverse coords)))
            (ltk:configure (plot-curve plot) :width (if (> n 200) 1 2))
            (if (and (realp threshold) (<= lo threshold hi))
                (let ((y (py threshold)))
                  (setf (ltk:coords (plot-threshold plot))
                        (list left y (+ left plot-w) y))
                  (ltk:configure (plot-threshold plot) :state "normal"))
                (ltk:configure (plot-threshold plot) :state "hidden")))
          (ltk:configure (plot-hi-text plot) :text (format nil "~,5F" hi))
          (ltk:configure (plot-lo-text plot) :text (format nil "~,5F" lo))
          (ltk:configure (plot-n-text plot)
                         :text (format nil "~D erreurs, de la plus ancienne a la plus recente"
                                       (length series)))))))

;;; ------------------------------------------------------------------
;;; 5. La fenetre et sa boucle de rafraichissement
;;; ------------------------------------------------------------------

(defparameter *interval* 300
  "Periode du rafraichissement automatique, en millisecondes.")

(defstruct viewer
  source
  weights-canvas error-canvas plot
  status-label signals-label scale-label auto-button view-button
  (view :heatmap)                       ; :heatmap ou :graph, meme canevas
  items                                 ; items du view courant (rectangles, ou NETGRAPH)
  built                                 ; (view . topologie) deja dessine(e)
  (auto t)
  (interval *interval*)
  (running t))

(defvar *viewer* nil "Derniere fenetre ouverte, pour inspection au REPL.")

(defun refresh (v)
  "Relit le reseau et met tout a jour : poids (grille ou graphe), courbe,
lignes d'etat."
  (let* ((snap (gui-snapshot (viewer-source v)))
         (weights (snapshot-weights snap))
         (shape (matrix-shape weights))
         (max-abs (max-abs-weight weights))
         (key (cons (viewer-view v) shape)))
    ;; premier passage, changement de vue, ou (clear net) : tout reconstruire
    (unless (equal key (viewer-built v))
      (setf (viewer-items v)
            (ecase (viewer-view v)
              (:heatmap (build-heatmap (viewer-weights-canvas v) weights
                                       (cell-size shape *weights-width*
                                                  *weights-height*)))
              (:graph (build-graph (viewer-weights-canvas v) weights)))
            (viewer-built v) key))
    (ecase (viewer-view v)
      (:heatmap (paint-heatmap (viewer-items v) weights max-abs))
      (:graph (paint-graph (viewer-items v) weights max-abs
                           (snapshot-inputs snap) (snapshot-outputs snap))))
    (draw-error-plot (viewer-plot v) (snapshot-errors snap)
                     (snapshot-threshold snap))
    (setf (ltk:text (viewer-status-label v)) (snapshot-status snap)
          (ltk:text (viewer-signals-label v)) (snapshot-signals snap)
          (ltk:text (viewer-scale-label v))
          (ecase (viewer-view v)
            (:heatmap
             (format nil "rouge : w > 0 ; bleu : w < 0 ; max |w| ~,4F ; une ligne = un neurone"
                     max-abs))
            (:graph
             (format nil "rouge : w > 0 ; bleu : w < 0 ; max |w| ~,4F ; lien : du neurone du haut vers celui qu'il alimente ; cercles d'entree et de sortie : blanc = 0, noir = 1"
                     max-abs))))
    v))

(defun guarded-refresh (v)
  "REFRESH en protegeant la fenetre : un reseau lu en pleine mise a jour par
le thread d'apprentissage peut etre momentanement incoherent ; mieux vaut
sauter une image, et afficher l'erreur dans la fenetre, qu'ouvrir un
debugger invisible dans le thread graphique."
  (handler-case (refresh v)
    (error (c)
      (ignore-errors
       (setf (ltk:text (viewer-signals-label v)) (format nil "! ~A" c))))))

(defun tick (v)
  "Reveil periodique : relit le reseau puis se reprogramme lui-meme. Tourne
dans le thread de la fenetre, donc tous les appels Tk y restent."
  (when (viewer-auto v)
    (guarded-refresh v))
  (when (viewer-running v)
    (ltk:after (viewer-interval v) (lambda () (tick v)))))

(defun update-auto-button (v)
  (setf (ltk:text (viewer-auto-button v))
        (if (viewer-auto v) "auto : marche" "auto : arret")))

(defun toggle-auto (v)
  (setf (viewer-auto v) (not (viewer-auto v)))
  (update-auto-button v)
  (when (viewer-auto v) (guarded-refresh v)))

(defun update-view-button (v)
  (setf (ltk:text (viewer-view-button v))
        (ecase (viewer-view v)
          (:heatmap "vue : grille")
          (:graph "vue : graphe"))))

(defun toggle-view (v)
  (setf (viewer-view v) (if (eq (viewer-view v) :heatmap) :graph :heatmap))
  (update-view-button v)
  (guarded-refresh v))

(defun reset-errors (v)
  "Vide l'historique d'erreur de la source (RESET-GUI-ERRORS) et rafraichit :
la courbe repart a vide immediatement, sans attendre le prochain tic."
  (reset-gui-errors (viewer-source v))
  (guarded-refresh v))

(defun launch-gui (source &key (auto t) (interval *interval*) (view :heatmap))
  "Ouvre la fenetre sur SOURCE (instance mlp / rmlp, ou le symbole qui la
nomme) et entre dans la boucle d'evenements Tk : BLOQUANT, et a n'appeler
que depuis le thread ou tournera la fenetre. VIEW est :HEATMAP (par defaut)
ou :GRAPH ; un bouton dans la fenetre bascule de l'une a l'autre en cours de
route. Voir GUI pour la version non bloquante, qui est celle a utiliser au
REPL."
  (let ((source (network source))
        (v nil))
    (ltk:with-ltk ()
      (ltk:wm-title ltk:*tk* (format nil "neuromuse - ~A" (source-title source)))
      (let* ((status (make-instance 'ltk:label :master ltk:*tk* :text ""
                                    :wraplength *weights-width*))
             (signals (make-instance 'ltk:label :master ltk:*tk* :text ""
                                     :wraplength *weights-width*))
             (wcanvas (make-instance 'ltk:canvas :master ltk:*tk*
                                     :width *weights-width* :height *weights-height*
                                     :background "white"))
             (scale (make-instance 'ltk:label :master ltk:*tk* :text ""
                                   :wraplength *weights-width*))
             (ecanvas (make-instance 'ltk:canvas :master ltk:*tk*
                                     :width *error-width* :height *error-height*
                                     :background "white"))
             (controls (make-instance 'ltk:frame :master ltk:*tk*))
             (refresh-button (make-instance 'ltk:button :master controls
                                            :text "rafraichir"))
             (auto-button (make-instance 'ltk:button :master controls :text ""))
             (view-button (make-instance 'ltk:button :master controls :text ""))
             (reset-button (make-instance 'ltk:button :master controls
                                          :text "effacer erreur")))
        (setf v (make-viewer :source source
                             :weights-canvas wcanvas :error-canvas ecanvas
                             :plot (build-error-plot ecanvas)
                             :status-label status :signals-label signals
                             :scale-label scale :auto-button auto-button
                             :view-button view-button :view view
                             :auto auto :interval interval)
              *viewer* v)
        (setf (ltk:command refresh-button) (lambda () (guarded-refresh v))
              (ltk:command auto-button) (lambda () (toggle-auto v))
              (ltk:command view-button) (lambda () (toggle-view v))
              (ltk:command reset-button) (lambda () (reset-errors v)))
        (update-auto-button v)
        (update-view-button v)
        (ltk:pack status :side :top :anchor :w :padx 8 :pady 3)
        (ltk:pack signals :side :top :anchor :w :padx 8)
        (ltk:pack wcanvas :side :top :padx 8 :pady 4)
        (ltk:pack scale :side :top :anchor :w :padx 8)
        (ltk:pack ecanvas :side :top :padx 8 :pady 4)
        (ltk:pack controls :side :top :pady 4)
        (ltk:pack refresh-button :side :left :padx 4)
        (ltk:pack auto-button :side :left :padx 4)
        (ltk:pack view-button :side :left :padx 4)
        (ltk:pack reset-button :side :left :padx 4)
        ;; fermeture de la fenetre : on arrete de se reprogrammer
        (ltk:bind ltk:*tk* "<Destroy>"
                  (lambda (event) (declare (ignore event))
                    (setf (viewer-running v) nil)))
        (guarded-refresh v)
        (tick v)))
    (when v (setf (viewer-running v) nil))
    (values)))

(defun gui (source &rest args)
  "Comme LAUNCH-GUI, mais dans son propre thread (mk-process) : le REPL
reste libre pour lancer l'apprentissage et regarder la fenetre le suivre.
Retourne le thread ; (neuromuse-gui::*viewer*) donne la fenetre ouverte."
  (mk-process "neuromuse-gui" (lambda () (apply #'launch-gui source args))))

;;; ------------------------------------------------------------------
;;; 6. Demonstration sans reseau
;;; ------------------------------------------------------------------

(defclass demo-source ()
  ((sizes :initform '(5 8 3) :initarg :sizes :accessor demo-sizes)
   (weights :initform nil :accessor demo-weights)
   (errors :initform nil :accessor demo-errors)
   (frames :initform 0 :accessor demo-frames))
  (:documentation "Source factice : ni mlp ni apprentissage, juste de quoi
verifier l'affichage seul (cf. DEMO)."))

(defmethod reset-gui-errors ((source demo-source))
  (setf (demo-errors source) nil (demo-frames source) 0))

(defmethod gui-snapshot ((source demo-source))
  (setf (demo-weights source)
        (if (demo-weights source)
            ;; derive aleatoire, pour voir bouger la heatmap
            (mapcar (lambda (layer)
                      (mapcar (lambda (neuron)
                                (mapcar (lambda (w)
                                          (clamp (+ w (- (random .2) .1)) -1.0 1.0))
                                        neuron))
                              layer))
                    (demo-weights source))
            (loop for (in out) on (demo-sizes source)
                  while out
                  collect (loop repeat out
                                collect (loop repeat in
                                              collect (- (random 2.0) 1.0))))))
  (incf (demo-frames source))
  (setf (demo-errors source)
        (nconc (demo-errors source)
               (list (max .01 (- 1.0 (* .004 (demo-frames source)) (random .05))))))
  (make-snapshot :weights (demo-weights source)
                 :errors (demo-errors source)
                 :status (format nil "demo (aucun reseau) : ~{~D~^-~}"
                                 (demo-sizes source))
                 :signals (format nil "image ~D | poids aleatoires en derive, erreur factice decroissante"
                                  (demo-frames source))
                 :inputs (loop repeat (first (demo-sizes source)) collect (random 1.0))
                 :outputs (loop repeat (car (last (demo-sizes source))) collect (random 1.0))
                 :threshold .1))

(defun demo (&key (sizes '(5 8 3)))
  "Ouvre la fenetre sur des poids factices : verifie Tk, Ltk et l'affichage
sans toucher a un reseau."
  (gui (make-instance 'demo-source :sizes sizes)))

;; EOF
