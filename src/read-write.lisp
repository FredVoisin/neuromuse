;;;; read-write.lisp - Lire et ecrire un ANN dans un fichier : SAVE
;;;; serialise l'etat (quasi-)complet d'un neuron/mlp en une forme Lisp
;;;; rechargeable ; TRACE-ACTIVATION, plus bas, journalise juste son
;;;; activation au fil d'une boucle d'apprentissage ou d'execution, comme une
;;;; sequence de formes Lisp, une par appel.

(in-package :neuromuse)

;;; ------------------------------------------------------------------
;;; SAVE : serialisation d'un neuron/mlp vers un fichier rechargeable
;;; ------------------------------------------------------------------

(defun structure-slot-names (s-name)
  "Given a class name such as a neural net's, returns the list of the slots for the class."
  #+sbcl (mapcar #'sb-mop:slot-definition-name
		 (sb-mop:class-slots
		  (find-class s-name)))
  #-sbcl
  (error "structure-slot-names is not defined for this lisp dialect,
 some features won't work (as save...)"))

(defgeneric save (self &optional path)
  (:documentation "Save neural net <self> to <path>."))

(defmethod save ((self neuron) &optional (path "ann.lisp"))
  (let ((slots (structure-slot-names (type-of self))))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :append
			    :if-does-not-exist :create)
      (format stream "(in-package :neuromuse)")
      (format stream "~&(make-instance 'neuron")
      (loop for s in slots
	    do
	    (format stream " :~S \'~S" s (funcall s self)))
      (format stream ")~%"))))

(defmethod save ((self list) &optional (path "ann.lisp"))
  (with-open-file (stream path
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :create)
    (if (atom (car self))
	(format stream " ~S~&" self)
	(save self path)))
  (values))

(defmethod save ((self t) &optional (path "ann.lisp"))
  (declare (ignore path))
  (format t "~&No method for saving ~S !~&" self))

(defun function-designator (f)
  "Nom de la fonction F (generique ou non), pour pouvoir l'ecrire et la
relire comme #'nom -- #+sbcl seulement (SB-MOP:GENERIC-FUNCTION-NAME pour un
generique comme LOGISTIC/LINEAR/..., FUNCTION-LAMBDA-EXPRESSION sinon)."
  #+sbcl (if (typep f 'generic-function)
             (sb-mop:generic-function-name f)
             (nth-value 2 (function-lambda-expression f)))
  #-sbcl (nth-value 2 (function-lambda-expression f)))

(defmethod save ((self mlp) &optional path)
  "Serialise tous les slots de SELF (STRUCTURE-SLOT-NAMES) dans un seul
MAKE-INSTANCE rechargeable avec LOAD. Un slot fonction (HIDDEN-FUN, OUT-FUN --
toujours au moins ca, meme aux valeurs par defaut, #'LOGISTIC) s'ecrit
#'nom (FUNCTION-DESIGNATOR), pas tel quel : ~S sur un objet fonction produit
une representation illisible (#<STANDARD-GENERIC-FUNCTION ...>), que LOAD ne
sait pas relire -- bug trouve en testant le round-trip SAVE/LOAD d'un mlp
construit avec ses reglages par defaut (doc/formats.md). Un slot liste ou
symbole (dont NAME : meme bug, un symbole non quote se relit comme une
reference de variable, pas comme un litteral) s'ecrit quote."
  (when (not path) (setf path (format nil "~S.lisp" (name self))))
  (let ((slots (structure-slot-names (type-of self))))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "(in-package :neuromuse)")
      (format stream "~&(make-instance 'mlp")
      (loop for s in slots
	 do
	   (let ((slot-value (funcall s self)))
	     (cond ((functionp slot-value)
		    (format stream " :~S #'~S~&" s (function-designator slot-value)))
		   ((or (listp slot-value) (symbolp slot-value))
		    (format stream " :~S '~S~&" s slot-value))
		   (t (format stream " :~S ~S~&" s slot-value)))))
      (format stream ")~%")))
  (format t "~& MLP ~S saved to file ~S !" (name self) path)
  (values))

(defmethod save ((self som) &optional path)
  "Serialise l'etat  SELF (pas tous ses slots) dans PATH (par
defaut ~S.lisp), sous une forme rechargeable avec LOAD. Contrairement a SAVE
pour un mlp, ne dumpe pas bêtement toutes les slots via
STRUCTURE-SLOT-NAMES : (net self) est une liste d'instances NEURON, pas de
nombres, et (neighbourhood self) une fonction (#'VOISINS par defaut) -- ni
l'une ni l'autre ne se relit avec ~S. A la place, un unique MAKE-INSTANCE
reconstruit la grille a la bonne taille (declenchant INIT, donc de nouveaux
NEURON correctement cables -- NN, ID -- comme a la construction d'origine),
puis les reglages vraiment utiles (TOPOLOGY, RADIUS, LEARN-FACT, TEMP,
NET-TEMP, DISTANCE, GRID-DISTANCE, EPOCH, ERROR-SCALING, MAX-ERROR) et, pour chaque neurone, son NET -- les poids
synaptiques appris, la seule partie de l'etat d'un neurone qui compte pour
l'usage du SOM (AGE, OUTPUT, DISTANCE d'un neurone restent a leurs valeurs
par defaut ; NEIGHBOURHOOD est ecrit s'il s'agit d'une fonction nommee,
comme #'VOISINS ou #'VOISINS-TOROIDAL, et laisse a #'VOISINS sinon). Tout est ecrit a l'interieur d'un seul LET, pas
plusieurs formes qui referenceraient (NAME SELF) par symbole : si ce nom est
deja lie au moment du rechargement, INITIALIZE-INSTANCE en choisit un autre
(cf. MAKE-NEW-SYMBOL), et des formes separees rateraient alors l'instance
reellement creee -- LET lie IT a ce que MAKE-INSTANCE renvoie vraiment, et
tout le reste opere sur IT."
  (when (not path) (setf path (format nil "~S.lisp" (name self))))
  (with-open-file (stream path
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "(in-package :neuromuse)~%")
    (format stream "(let ((it (make-instance 'som :name '~S :size ~D :input ~D)))~%"
	    (name self) (length (net self)) (length (input self)))
    (format stream "  (setf (topology it) '~S (radius it) ~S (learn-fact it) ~S~%        (temp it) ~S (net-temp it) ~S (distance it) '~S (grid-distance it) '~S (epoch it) ~D~%        (error-scaling it) ~S (max-error it) ~S)~%"
	    (topology self) (radius self) (learn-fact self)
	    (temp self) (net-temp self) (distance self) (grid-distance self) (epoch self)
	    (error-scaling self) (max-error self))
    ;; voisinage : seulement s'il a un nom relisible (#'VOISINS-TOROIDAL...)
    (let ((f (neighbourhood self)))
      (let ((fname (if (symbolp f) f (nth-value 2 (function-lambda-expression f)))))
        (when (and (symbolp fname) fname (fboundp fname))
          (format stream "  (setf (neighbourhood it) #'~S)~%" fname))))
    (dolist (n (net self))
      (format stream "  (setf (net (nth ~D (net it))) '~S)~%" (car (id n)) (net n)))
    (format stream "  it)~%"))
  (format t "~& SOM ~S saved to file ~S !" (name self) path)
  (values))

(defmethod save ((self rosom) &optional path)
  (declare (ignore path))
  (error "SAVE ne gere pas encore ROSOM : (net rosom) est (neurones-contenu
neurones-contexte), pas une liste plate de neurones comme pour SOM. A faire."))

;;; ------------------------------------------------------------------
;;; WRITE-DOT : (net ann) au format DOT (Graphviz), pour neato/dot/fdp/...
;;; ------------------------------------------------------------------
;;;
;;; Une methode par forme de graphe, pas par classe -- contrairement a SAVE,
;;; qui doit connaitre la forme exacte de NET pour chaque classe (et ne gere
;;; toujours pas ROSOM, cf. juste au-dessus) :
;;;   - mlp/rmlp/perceptron : la vue EDGES/NODES generique (NODES/EDGES,
;;;     neuromuse-main.lisp et chaque fichier de classe) -- poids vers
;;;     l'entree, leurs connexions telles quelles.
;;;   - som : le voisinage de grille entre neurones, pas ses connexions a
;;;     l'entree -- methode separee, plus bas (positions de grille imposees
;;;     a neato).
;;;   - hopfield/auto-assoc : une grille de cellules toutes connectees entre
;;;     elles (pas de voisinage, juste une disposition lisible) -- encore
;;;     une troisieme methode, plus bas (CELL-GRID-DOT-SOURCE).

(defun dot-quote (string)
  "STRING entre guillemets DOT, guillemets et antislashs internes echappes --
un identifiant ou un label DOT entre guillemets accepte a peu pres n'importe
quoi, ce qui evite d'avoir a connaitre la forme exacte d'un noeud (instance
NEURON, liste (COUCHE INDICE) ou (:INPUT J), entier...) pour la citer."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for c across string
          do (when (member c '(#\" #\\)) (write-char #\\ out))
             (write-char c out))
    (write-char #\" out)))

(defun gray-hex (fraction)
  "FRACTION dans [0,1] -> \"#RRGGBB\" en niveau de gris, blanc (0) a noir
(1) -- U-matrix d'un SOM (WRITE-DOT sur SOM) : plus deux voisins sont
distants dans l'espace des poids, plus l'arc/le noeud est sombre."
  (let ((v (round (* 255 (- 1 (max 0 (min 1 fraction)))))))
    (format nil "#~2,'0X~2,'0X~2,'0X" v v v)))

(defun sequential-hex (fraction)
  "FRACTION dans [0,1] -> \"#RRGGBB\", blanc (0) a bleu (1) -- plans de
composantes d'un SOM (WRITE-DOT sur SOM, MODE :component), distinct du gris
de l'U-matrix pour ne pas confondre les deux lectures."
  (let ((v (round (* 255 (- 1 (max 0 (min 1 fraction)))))))
    (format nil "#~2,'0X~2,'0XFF" v v)))

(defun diverging-hex (fraction)
  "FRACTION dans [-1,1] -> \"#RRGGBB\", bleu (negatif) - blanc (0) - rouge
(positif) -- meme convention que la heatmap de src/gui.lisp (rouge = poids
positif, bleu = negatif, normalise par le plus grand |poids|)."
  (let* ((f (max -1 (min 1 fraction)))
         (v (round (* 255 (- 1 (abs f))))))
    (if (minusp f)
        (format nil "#~2,'0X~2,'0XFF" v v)
        (format nil "#FF~2,'0X~2,'0X" v v))))

(defun render-dot (dot-text path format engine &key extra-args)
  "Ecrit DOT-TEXT (une chaine, deja une source DOT complete) dans PATH si
FORMAT est :dot ; sinon dans un fichier temporaire PATH.dot, rendu par
ENGINE vers PATH (#+sbcl, via SB-EXT:RUN-PROGRAM), efface si le rendu
reussit, conservee sinon (moteur introuvable ou echec : avertissement
plutot que de perdre la source). EXTRA-ARGS : arguments passes a ENGINE
avant -T<format> ... -o PATH -- par exemple (\"-n2\") pour que neato
respecte des positions imposees (pos=\"x,y!\") au lieu de recalculer sa
propre disposition, cf. WRITE-DOT sur SOM."
  (if (string-equal (string format) "dot")
      (with-open-file (stream path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string dot-text stream))
      #+sbcl
      (let ((dot-path (format nil "~A.dot" path)))
        (with-open-file (stream dot-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-string dot-text stream))
        (handler-case
            (let ((proc (sb-ext:run-program engine
                                            (append extra-args
                                                    (list (format nil "-T~(~A~)" format) dot-path "-o" path))
                                            :search t :output *standard-output* :error *standard-output*)))
              (if (zerop (sb-ext:process-exit-code proc))
                  (delete-file dot-path)
                  (warn "~A a echoue (code ~D) sur ~S ; source DOT conservee dans ~S"
                        engine (sb-ext:process-exit-code proc) path dot-path)))
          (error (e)
            (warn "~A injoignable (~A) ; source DOT conservee dans ~S" engine e dot-path))))
      #-sbcl
      (error "Rendu ~A non disponible hors SBCL -- utiliser FORMAT :dot (le ~
defaut) et rendre a la main : ~A ~{~A ~}-T~(~A~) fichier.dot -o fichier.~(~A~)"
             format engine extra-args format format)))

(defun edge-attrs (weight &key color (edge-label t) (max-weight 1))
  "Chaine d'attributs DOT [label=... color=...] pour un arc de poids WEIGHT.
EDGE-LABEL (t par defaut) affiche le poids arrondi (ROUND1, 3 decimales) ;
COLOR (nil par defaut) colore l'arc rouge/bleu par signe, normalise par
MAX-WEIGHT (DIVERGING-HEX) -- l'un, l'autre, ou les deux (ni l'un ni
l'autre : un arc nu)."
  (let ((parts '()))
    (when edge-label
      (push (format nil "label=~A" (dot-quote (princ-to-string (round1 weight 3)))) parts))
    (when color
      (push (format nil "color=~A"
                    (dot-quote (diverging-hex (if (zerop max-weight) 0 (/ weight max-weight)))))
            parts))
    (format nil "[~{~A~^, ~}]" (nreverse parts))))

(defun dot-source (ann &key color (edge-label t) (node-label t))
  "La source DOT de (net ann), vue comme un graphe pondere (NODES/EDGES) --
une chaine, fonction pure (ne touche a aucun fichier) : un digraph, un noeud
par element de (nodes ann) (NODE-LABEL, t par defaut, l'etiquette par sa
propre representation imprimee -- princ -- qu'il s'agisse d'une instance
NEURON, d'une liste (COUCHE INDICE)/(:INPUT J)/(:CONTEXT J) ou d'un entier
de cellule ; nil, aucune etiquette), un arc SOURCE -> CIBLE par element de
(edges ann) (cf. EDGE-ATTRS pour COLOR/EDGE-LABEL). Methode generique de
WRITE-DOT pour mlp/rmlp/perceptron -- hopfield/auto-assoc ont leur propre
methode, CELL-GRID-DOT-SOURCE plus bas, mieux adaptee a leur connectivite
complete (EDGES marche techniquement aussi bien sur eux, poids symetrique
NET[i][j] = NET[j][i] ecrit en une seule fleche par paire, information non
perdue, juste affichee dans un seul sens -- mais sans la disposition en
grille lisible de CELL-GRID-DOT-SOURCE)."
  (flet ((dot-id (node) (dot-quote (princ-to-string node))))
    (let* ((all-edges (edges ann))
           (max-w (if (and color all-edges)
                      (reduce #'max all-edges :key (lambda (e) (abs (third e))))
                      1)))
      (with-output-to-string (stream)
        (format stream "digraph ~A {~%" (dot-quote (string-downcase (string (name ann)))))
        (dolist (node (nodes ann))
          (let ((id (dot-id node)))
            (format stream "  ~A [label=~A];~%" id (if node-label id (dot-quote "")))))
        (dolist (edge all-edges)
          (format stream "  ~A -> ~A ~A;~%"
                  (dot-id (first edge)) (dot-id (second edge))
                  (edge-attrs (third edge) :color color :edge-label edge-label :max-weight max-w)))
        (format stream "}~%")))))

(defgeneric write-dot (ann &optional path &key format engine)
  (:documentation "Ecrit (net ann) comme un graphe Graphviz dans PATH (par
defaut ~(nom~).~(format~)). FORMAT (:dot par defaut) choisit ce qui atterrit
dans PATH :
  :dot            la source Graphviz telle quelle -- portable, lisible,
                  modifiable a la main, a rendre soi-meme ensuite avec
                  n'importe quel moteur (dot, neato, fdp, sfdp, circo,
                  twopi...) :  dot -Tpng reseau.dot -o reseau.png
  :png, :svg, ... n'importe quel format de sortie que ENGINE sait produire --
                  la source DOT est ecrite dans un fichier temporaire
                  (PATH.dot), rendue par ENGINE (#+sbcl, via
                  SB-EXT:RUN-PROGRAM), puis effacee si le rendu reussit
                  (sinon elle reste, pour deboguer).
Une methode par forme de graphe, pas par classe : (net ann) vu comme
EDGES/NODES, poids vers l'entree (mlp/rmlp/perceptron) ; comme une grille de
voisinage entre neurones (SOM -- ROSOM pas encore supporte) ; comme une
grille de cellules toutes connectees entre elles (HOPFIELD/AUTO-ASSOC) --
trois representations differentes, pas la meme vue rendue differemment.
Voir WRITE-DOT sur SOM et sur HOPFIELD pour les deux autres."))

(defmethod write-dot ((ann ann) &optional path
                       &key (format :dot) (engine "dot") color (edge-label t) (node-label t))
  "COLOR (nil par defaut) et EDGE-LABEL (t par defaut) : cf. EDGE-ATTRS --
arcs colores rouge/bleu par signe (comme la heatmap de src/gui.lisp),
etiquetes par leur poids, les deux a la fois ou ni l'un ni l'autre.
NODE-LABEL (t par defaut) : chaque noeud etiquete par sa propre
representation imprimee, ou nil pour un noeud nu."
  (when (not path)
    (setf path (format nil "~(~A~).~(~A~)" (name ann) format)))
  (render-dot (dot-source ann :color color :edge-label edge-label :node-label node-label)
              path format engine)
  (format t "~& ~S : graphe ecrit dans ~S~@[, rendu par ~A~]"
          (name ann) path (unless (string-equal (string format) "dot") engine))
  (values))

(defmethod write-dot ((ann rosom) &optional path &key format engine color edge-label node-label)
  (declare (ignore path format engine color edge-label node-label))
  (error "WRITE-DOT ne gere pas encore ROSOM : (net rosom) est
(content-neurones context-neurones), pas une grille plate de neurones comme
pour SOM, dont la methode WRITE-DOT suppose la forme. A faire."))

(defun som-grid-dot-source (ann &key (mode :u-matrix) component labels node-label (color t)
                                   edge-label (scale 1.2))
  "La source DOT de la topologie de grille de ANN (SOM) -- fonction pure,
cf. DOT-SOURCE. Un graphe NON dirige (GRAPH/--, pas DIGRAPH/->, le voisinage
de grille est symetrique), un noeud par neurone a sa position de grille
imposee (POS=\"x,y!\" -- carree, (2D i n) ; hexagonale, decalee par
HEX-POSITION), un arc par paire de GRID-NEIGHBOUR-PAIRS (som.lisp), toujours
a la distance entre les poids des deux neurones (EUCLIDIAN sur WEIGHTS).
MODE :u-matrix (par defaut) : COLOR (t par defaut) colore chaque arc en
  niveau de gris (GRAY-HEX) par cette distance, normalisee par la plus
  grande observee -- les arcs sombres marquent les frontieres entre
  clusters -- et chaque noeud par sa distance moyenne a ses voisins
  directs ; COLOR nil laisse arcs et noeuds nus (juste la topologie).
MODE :component : COMPONENT (l'indice d'une dimension d'entree, requis)
  choisit une des (in-size ann) cartes de composantes -- chaque noeud
  colore (blanc a bleu, SEQUENTIAL-HEX) par le poids de cette seule
  dimension, normalise par son min/max sur la carte ; COLOR sans effet ici,
  ce mode n'a pas de sens sans la couleur qui montre la carte. Appeler une
  fois par dimension pour les avoir toutes.
EDGE-LABEL (nil par defaut) affiche la distance (ROUND1, 3 decimales) en
texte sur chaque arc, meme hors MODE :u-matrix et meme si COLOR est nil.
LABELS (nil par defaut) : sequence de longueur (length (net ann)), dans
l'ordre de (net ann) -- un compte de victoires (BMU) par exemple, calcule
par l'appelant (INVENTAIRE-H, maths.lisp, sur une suite de gagnants) --
WRITE-DOT ne le fait pas lui-meme, rien dans som.lisp n'accumule cet
historique (cf. le viewer de src/gui.lisp, qui l'accumule lui-meme pour la
meme raison). A defaut de LABELS, NODE-LABEL (nil par defaut) affiche
l'indice du neurone a la place -- LABELS reste prioritaire si les deux sont
donnes. SCALE agrandit/retrecit l'espacement de la grille.
Positions non ajustees pour un :boundary :torus : les arcs qui bouclent par
le bord (une ligne/colonne entiere, premier et dernier neurone) se
retrouvent alignes avec les arcs ordinaires de cette meme ligne/colonne --
visuellement indiscernables d'eux sur une grille carree, meme si c'est bien
un arc direct entre les deux bouts, pas une chaine de voisins intermediaires.
La topologie reste correcte (GRID-NEIGHBOUR-PAIRS, pas ce fichier, la calcule),
seule la disposition a plat ne le montre pas joliment."
  (let* ((net (net ann))
         (n (length net))
         (topos (topology ann))
         (lattice (getf (cddr topos) :lattice :square))
         (pairs (grid-neighbour-pairs ann))
         (positions (loop for i below n
                          collect (if (eq lattice :hex) (hex-position (2d i n)) (2d i n))))
         (ws (mapcar #'weights net))
         (dists (mapcar (lambda (pair)
                          (euclidian (nth (first pair) ws) (nth (second pair) ws)))
                        pairs))
         (u-max (if dists (reduce #'max dists) 1))
         (u-color (and color (eq mode :u-matrix)))
         (node-sum (make-array n :initial-element 0.0))
         (node-count (make-array n :initial-element 0)))
    (loop for (i j) in pairs
          for d in dists
          do (incf (aref node-sum i) d) (incf (aref node-count i))
             (incf (aref node-sum j) d) (incf (aref node-count j)))
    (dotimes (i n)
      (when (plusp (aref node-count i))
        (setf (aref node-sum i) (/ (aref node-sum i) (aref node-count i)))))
    (let (comp-min comp-max)
      (when (eq mode :component)
        (let ((vals (mapcar (lambda (w) (nth component w)) ws)))
          (setf comp-min (reduce #'min vals) comp-max (reduce #'max vals))))
      (with-output-to-string (stream)
        (format stream "graph ~A {~%" (dot-quote (string-downcase (string (name ann)))))
        (format stream "  node [shape=circle style=filled fixedsize=true width=0.5];~%")
        (dotimes (i n)
          (let* ((pos (nth i positions))
                 (fill (ecase mode
                         (:u-matrix (if u-color
                                        (gray-hex (if (zerop u-max) 0 (/ (aref node-sum i) u-max)))
                                        "#FFFFFF"))
                         (:component (sequential-hex (if (= comp-max comp-min) 0
                                                          (/ (- (nth component (nth i ws)) comp-min)
                                                             (- comp-max comp-min))))))))
            (format stream "  n~D [pos=~A, fillcolor=~A, label=~A];~%"
                    i
                    (dot-quote (format nil "~F,~F!" (* scale (first pos)) (* scale (- (second pos)))))
                    (dot-quote fill)
                    (dot-quote (cond (labels (princ-to-string (elt labels i)))
                                      (node-label (princ-to-string i))
                                      (t ""))))))
        (loop for (i j) in pairs
              for d in dists
              do (let ((parts '()))
                   (when edge-label
                     (push (format nil "label=~A" (dot-quote (princ-to-string (round1 d 3)))) parts))
                   (when u-color
                     (push (format nil "color=~A" (dot-quote (gray-hex (if (zerop u-max) 0 (/ d u-max)))))
                           parts))
                   (format stream "  n~D -- n~D [~{~A~^, ~}];~%" i j (nreverse parts))))
        (format stream "}~%")))))

(defmethod write-dot ((ann som) &optional path
                       &key (format :dot) (engine "neato") (mode :u-matrix) component labels
                         node-label (color t) edge-label (scale 1.2))
  "Topologie de grille de ANN, pas la vue EDGES/NODES generique (cf. le
generique WRITE-DOT) -- positions imposees (POS=\"x,y!\" -- le point
d'exclamation final est ce qui les epingle : neato, comme tout moteur de la
famille, le respecte directement, sans option particuliere a passer).
ENGINE reste \"neato\" par defaut : un moteur hors de cette famille (dot,
circo, twopi) ignore generalement POS et recalcule sa propre disposition.
Cf. SOM-GRID-DOT-SOURCE pour MODE/COMPONENT/LABELS/NODE-LABEL/COLOR/
EDGE-LABEL/SCALE."
  (when (not path)
    (setf path (format nil "~(~A~).~(~A~)" (name ann) format)))
  (render-dot (som-grid-dot-source ann :mode mode :component component :labels labels
                                   :color color :edge-label edge-label
                                   :node-label node-label :scale scale)
              path format engine)
  (format t "~& ~S : graphe de grille ecrit dans ~S~@[, rendu par ~A~]"
          (name ann) path (unless (string-equal (string format) "dot") engine))
  (values))

(defun cell-grid-dot-source (ann &key color (edge-label t) node-label (scale 1.2) (threshold 0))
  "La source DOT de (net ann) vue comme une grille de cellules -- fonction
pure, cf. DOT-SOURCE/SOM-GRID-DOT-SOURCE -- pour hopfield/auto-assoc : NET y
est un tableau IN-SIZE x IN-SIZE symetrique (pas une liste de neurones comme
pour un som, ni une liste de matrices comme pour un mlp), chaque cellule
connectee a toutes les autres -- pas de notion de voisinage ici
(GRID-NEIGHBOUR-PAIRS, som.lisp, ne s'applique pas). Positions de grille
imposees (GRID-SHAPE, maths.lisp -- COLS x ROWS, pas necessairement un carre
parfait) : pas une topologie, juste une disposition lisible plutot que la
« boule de poils » qu'un moteur libre de placer les noeuds obtiendrait sur
un graphe aussi dense. Un arc par paire (I J), I < J (NET[I][J] = NET[J][I],
cf. EDGES sur ces classes), dont |poids| >= THRESHOLD (0 par defaut : tout
est trace -- sur un grand reseau, ecarl180, 180 cellules, cf.
doc/auto-assoc-exemples.md, C(180,2) = 16110 paires : relever THRESHOLD
declutter le rendu). COLOR/EDGE-LABEL : cf. EDGE-ATTRS, memes arcs rouge/
bleu par signe que la vue EDGES/NODES generique. Chaque noeud colore (blanc
a bleu, SEQUENTIAL-HEX) par sa valeur courante dans (output ann), normalisee
par le min/max observe, si elle existe deja -- quel motif le reseau
reconnait/reconstruit en ce moment -- sinon une couleur neutre : LEARN
n'ecrit jamais (output self) pour ces deux classes, seuls RUN-HOPFIELD/
RUN-AUTO-ASSOC le font (meme remarque que pour le viewer de src/gui.lisp).
NODE-LABEL (nil par defaut) affiche l'indice de la cellule."
  (let* ((net (net ann))
         (n (in-size ann))
         (out (output ann)))
    (multiple-value-bind (cols rows) (grid-shape n)
      (declare (ignore rows))
      (let* ((pairs (loop for i below n
                          append (loop for j from (1+ i) below n
                                       when (>= (abs (aref net i j)) threshold)
                                         collect (list i j))))
             (max-w (if (and color pairs)
                        (reduce #'max pairs :key (lambda (p) (abs (aref net (first p) (second p)))))
                        1))
             (out-min (and out (plusp (length out)) (reduce #'min out)))
             (out-max (and out (plusp (length out)) (reduce #'max out))))
        (with-output-to-string (stream)
          (format stream "graph ~A {~%" (dot-quote (string-downcase (string (name ann)))))
          (format stream "  node [shape=circle style=filled fixedsize=true width=0.5];~%")
          (dotimes (i n)
            (let* ((col (mod i cols)) (row (floor i cols))
                   (fill (if (and out-max (/= out-max out-min))
                             (sequential-hex (/ (- (elt out i) out-min) (- out-max out-min)))
                             "#CCCCCC")))
              (format stream "  n~D [pos=~A, fillcolor=~A, label=~A];~%"
                      i (dot-quote (format nil "~F,~F!" (* scale col) (* scale (- row)))) (dot-quote fill)
                      (dot-quote (if node-label (princ-to-string i) "")))))
          (dolist (pair pairs)
            (let ((w (aref net (first pair) (second pair))))
              (format stream "  n~D -- n~D ~A;~%" (first pair) (second pair)
                      (edge-attrs w :color color :edge-label edge-label :max-weight max-w))))
          (format stream "}~%"))))))

(defmethod write-dot ((ann hopfield) &optional path
                       &key (format :dot) (engine "neato") color (edge-label t) node-label
                         (scale 1.2) (threshold 0))
  "Grille de cellules de ANN (CELL-GRID-DOT-SOURCE), pas la vue EDGES/NODES
generique -- toutes les cellules sont deja connectees entre elles (pas de
voisinage ici) ; la grille sert juste a obtenir une disposition lisible
plutot que la « boule de poils » qu'un moteur libre produirait sur un graphe
aussi dense. ENGINE \"neato\" par defaut, meme raison que pour SOM
(POS=\"x,y!\" respecte directement)."
  (when (not path) (setf path (format nil "~(~A~).~(~A~)" (name ann) format)))
  (render-dot (cell-grid-dot-source ann :color color :edge-label edge-label :node-label node-label
                                    :scale scale :threshold threshold)
              path format engine)
  (format t "~& ~S : graphe de grille ecrit dans ~S~@[, rendu par ~A~]"
          (name ann) path (unless (string-equal (string format) "dot") engine))
  (values))

(defmethod write-dot ((ann auto-assoc) &optional path
                       &key (format :dot) (engine "neato") color (edge-label t) node-label
                         (scale 1.2) (threshold 0))
  "Comme WRITE-DOT sur HOPFIELD (CELL-GRID-DOT-SOURCE) : NET a la meme forme
(tableau IN-SIZE x IN-SIZE symetrique) pour les deux classes."
  (when (not path) (setf path (format nil "~(~A~).~(~A~)" (name ann) format)))
  (render-dot (cell-grid-dot-source ann :color color :edge-label edge-label :node-label node-label
                                    :scale scale :threshold threshold)
              path format engine)
  (format t "~& ~S : graphe de grille ecrit dans ~S~@[, rendu par ~A~]"
          (name ann) path (unless (string-equal (string format) "dot") engine))
  (values))

;;; ------------------------------------------------------------------
;;; TRACE-ACTIVATION : journal d'activation au fil d'une boucle
;;; ------------------------------------------------------------------
;;;
;;; Peut être appelée à chaque pas d'une boucle d'apprentissage ou
;;; d'execution sans rien changer par ailleurs -- comme GUI-SNAPSHOT dans
;;; src/gui.lisp.

(defgeneric activation-state (ann)
  (:documentation "Etat d'activation de ANN a tracer par TRACE-ACTIVATION :
la couche de sortie, (OUTPUT ANN), pour un mlp/rmlp/perceptron -- ou toute
autre classe qui tient son activation courante dans ce meme emplacement,
d'ou la methode par defaut sur ANN plutot qu'une par classe. Pour un som,
une liste par case de la grille : (OUTPUT neuron) de chaque neurone de
(NET ann), dans cet ordre -- c'est-a-dire l'ordre de (ID neuron), la
position sur la grille (cf. 2D, src/maths.lisp)."))

(defmethod activation-state ((ann ann))
  (output ann))

(defmethod activation-state ((ann som))
  (mapcar #'output (net ann)))

(defmethod activation-state ((ann rosom))
  (error "ACTIVATION-STATE ne gere pas encore ROSOM."))

(defun trace-activation (ann &optional (path "activation-trace.lisp"))
  "Ajoute a la suite de PATH (mode APPEND ; un fichier de formes Lisp, une
par ligne, cree si besoin) l'etat d'activation courant de ANN (une instance,
ou le symbole qui la nomme) -- cf. ACTIVATION-STATE pour ce qui est
effectivement ecrit selon sa classe. Pensee pour s'inserer dans n'importe
quelle boucle d'apprentissage ou d'execution sans rien y changer par
ailleurs, par exemple :

  (dolist (frame chant)
    (setf (input som) (coerce frame 'vector))
    (find-winner som)               ; met a jour (output neuron) de chaque case
    (trace-activation som \"trace.lisp\"))

Chaque appel ajoute UN etat, sur UNE ligne (*PRINT-PRETTY* NIL le temps de
l'ecrire : sinon le pretty-printer de ~S replie les etats les plus larges --
144 cases pour un som -- sur plusieurs lignes, ce qui rendrait le fichier
plus penible a parcourir/grep ligne par ligne sans rien changer a ce qu'on
relit avec READ). La sequence est l'ordre des appels dans le fichier, relue
par READ-ACTIVATION-TRACE. Renvoie ANN, comme BACKPROPAGATE, LEARN (som),
ROSOM-LEARN et TRAIN-PERCEPTRON -- pour pouvoir l'imbriquer directement, par
exemple (trace-activation (learn som))."
  (let ((ann (if (symbolp ann) (symbol-value ann) ann)))
    (with-open-file (stream path :direction :output
                                  :if-exists :append :if-does-not-exist :create)
      (let ((*print-pretty* nil))
        (format stream "~S~%" (activation-state ann))))
    (values ann)))
 
(defun read-activation-trace (path)
  "Relit la sequence d'etats ecrite par TRACE-ACTIVATION dans PATH : une
liste de formes Lisp, dans l'ordre d'ecriture (la plus ancienne en tete)."
  (with-open-file (stream path :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))


(defun trace-output (ann &optional (path "output-trace.lisp"))
  "Ajoute a la suite de PATH l'output courant de ANN (une instance,
ou le symbole qui la nomme) - S'insère dans n'importe
quelle boucle sans rien changer, par exemple :

  (dolist (frame chant)
    (setf (input som) (coerce frame 'vector))
    (trace-output som \"trace.lisp\"))"
  (let ((ann (if (symbolp ann) (symbol-value ann) ann)))
    (with-open-file (stream path :direction :output
                                  :if-exists :append :if-does-not-exist :create)
      (let ((*print-pretty* nil))
        (format stream "~S~%"  (activation ann :n (car (id (car (find-winner ann))))))))
    (values ann)))


; eof
