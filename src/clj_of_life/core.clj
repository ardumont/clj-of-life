(ns clj-of-life.core
  (:use midje.sweet))

;; Avoir un univers infinie 2d orthogonal.
;; Etant donne une generation initiale.

;; Les regles suivantes sont appliquees:
;; - une cellule vivante ayant 2 ou 3 voisins reste vivante
;; - une cellule vite ayant moins de 2 voisins vivants meurt
;; - une cellule vivant ayant plus de 3 voisins meurt
;; - une cellule morte ayant 3 voisins devient vivante

(defn neighbours-coord "Compute the neighbours' coordinate"
  [y x]
  (let [x- (- x 1)
        x+ (+ x 1)
        y- (- y 1)
        y+ (+ y 1)]
    [[y- x-] [y- x] [y- x+] [y x-] [y x+] [y+ x-] [y+ x] [y+ x+]]))

(fact
  (neighbours-coord 0 0) => [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
  (neighbours-coord 1 1) => [[0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2]])

(defn state "Returns the state of the cell"
  [u y x]
  (get-in u [y x]))

(fact "state"
  (state [[0 1 0]] 0 1) => 1)

(defn count-neighbours-alive "Compute the state of the neigbours of the cell with coord [y x]"
  [u y x]
  ((frequencies (map (fn [[y x]] (state u y x)) (neighbours-coord y x))) 1))

(fact
  (count-neighbours-alive [[0 1 0]
                           [0 0 0]
                           [0 0 0]] 1 1) => 1
  (count-neighbours-alive [[0 1 1]
                           [0 0 1]
                           [0 0 1]] 1 1) => 4)

(def state-which-renders-alive {0 {3 1}
                                1 {2 1
                                   3 1}})

(defn next-state-cell "Given a universe u and a cell with coordinate y x, compute the next state of the cell [y x] in the universe u"
  [u y x]
  ((state-which-renders-alive (state u y x)) (count-neighbours-alive u y x) 0))

(fact "next state-cell"
  (next-state-cell [[0 1 0]
                    [0 1 0]
                    [0 0 0]] 1 1) => 0
  (next-state-cell [[0 1 1]
                    [0 1 0]
                    [0 0 0]] 1 1) => 1
  (next-state-cell [[1 1 1]
                    [0 1 0]
                    [1 0 0]] 1 1) => 0
  (next-state-cell [[0 1 1]
                    [0 0 0]
                    [1 0 0]] 1 1) => 1
  (next-state-cell [[0 0 1]
                    [0 0 0]
                    [1 0 0]] 1 1) => 0)

(defn coordinates "Compute the coordinates of the matrix"
  [u]
  (let [rn (range (count u))]
    (for [y rn, x rn] [y x])))

(fact
  (coordinates [[0 1]
                [0 0]]) => [[0 0] [0 1] [1 0] [1 1]])

(defn next-state "Given a universe, compute the next state of the universe"
  [u]
  (reduce
   (fn [r [y x :as c]]
     (assoc-in r c (next-state-cell u y x)))
   u
   (coordinates u)))

(fact
  (next-state [[0 1 0 0]
               [0 0 0 0]
               [0 0 0 0]
               [0 0 0 0]]) => [[0 0 0 0]
                               [0 0 0 0]
                               [0 0 0 0]
                               [0 0 0 0]]
  (next-state [[0 1 1 0]
               [0 0 0 0]
               [0 0 0 0]
               [0 0 0 0]]) => [[0 0 0 0]
                               [0 0 0 0]
                               [0 0 0 0]
                               [0 0 0 0]]
  (next-state [[0 1 1 0]
               [0 1 0 0]
               [0 0 0 0]
               [0 0 0 0]]) => [[0 1 1 0]
                               [0 1 1 0]
                               [0 0 0 0]
                               [0 0 0 0]])

;; ------------------------ Side effects -------------------------

;; improvment proposition from denlab:
;; pour les couleurs je te conseilles de tout simplement remplacer les "1" par des triplets RGB : [1 2 3]
;; comme ca pas trop dur a changer au niveau de l'implem

;; apres pour faire des moyennes de couleur c'est tout bete, tu fais la moyennes des composantes rgb, 
;; par example, la moynne de rouge [255 0 0] et bleu [0 0 255]
;; c'est [255/2 0/2 255/2] = [124 0 124] (violet je crois)
;; ensuite lors de l'affichage pour tu a constructeur de couleur qui prend le rgb, donc dans ce cas : (java.awt.Color. 124 0 124)

(def *size-cell 10);; size of the cell
(def *offset 29)   ;; for the border drawn in gnome (do not work under stumpwm)

(defn get-gfx "Given a width and a height, returns a frame with these dimension"
  [width height]
  (.getGraphics
   (doto (java.awt.Frame.)
     (.setSize width height)
     (.setVisible true))))

;; better implementation ?
(defn random-universe "Random universe"
  [size]
  (vec (map vec
            (partition-all size
                           (for [y (range size)
                                 x (range size)] (rand-int 2))))))

(defn- draw-cell "Given a color and a cell's coordinate, draw the cell with the color col"
  [gfx col y x]
  (.setColor gfx col)
  (.fillRect gfx
             (* *size-cell x)
             (+ *offset (* *size-cell y))
             *size-cell *size-cell))

(defn draw "Draw the game of life"
  [gfx w h u]
  (let [blank-color java.awt.Color/WHITE
        color {0 blank-color
               1 java.awt.Color/BLACK}
        r (range (count u))]
    (doseq [x r, y r]
      (let [st (state u y x)]
        ;; clear the painting
        (draw-cell gfx blank-color y x)
        ;; optimisation for display
        (when (= 1 st)
          ;; draw the new state if needed
          (draw-cell gfx (color st) y x))))))

(defn game-of-life "Game of life"
  ([n]
     (game-of-life n (random-universe n)))
  ([n u]
     (let [w (* *size-cell n)
           h (* *size-cell n)
           gfx (get-gfx w h)]
       (iterate (fn [u] (let [nxt-universe (next-state u)]
                         (do (draw gfx w h nxt-universe)
                             (Thread/sleep 300)
                             nxt-universe))) u))))