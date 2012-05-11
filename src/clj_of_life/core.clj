(ns clj-of-life.core
  (:use midje.sweet))

;; Avoir un univers infinie 2d orthogonal.
;; Etant donne une generation initiale.

;; Les regles suivantes sont appliquees:
;; - une cellule vivante ayant 2 ou 3 voisins reste vivante
;; - une cellule vite ayant moins de 2 voisins vivants meurt
;; - une cellule vivant ayant plus de 3 voisins meurt
;; - une cellule morte ayant 3 voisins devient vivante

(def alive? {1 true
             0 false})

(comment "first implementation"
  (defn neighbours-coord "Compute the neighbours' coordinates"
    [y x]
    (for [b [-1 0 1] a [-1 0 1] :let [y+ (+ b y) x+ (+ a x)] :when (not= [y+ x+] [y x])] [y+ x+])))

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

(defn neighbours-state "Compute the state of the neigbours of the cell with coord [y x]"
  [u y x]
  (map #(get-in u %) (neighbours-coord y x)))

(fact
  (neighbours-state [[0 1 0]
                     [0 0 0]
                     [0 0 0]] 1 1) => [0 1 0 0 0 0 0 0])

;; all possible states which renders a cell alive
(def state-which-renders-a-cell-alive {0 {3 1}
                                       1 {2 1
                                          3 1}})

(defn next-state-cell "Given a universe u and a cell with coordinate y x, compute the next state of the cell [y x] in the universe u"
  [u y x]
  (let [cell-state (get-in u [y x])
        nb-neighbours (count (filter alive? (neighbours-state u y x)))]
    ((state-which-renders-a-cell-alive cell-state) nb-neighbours 0)))

(fact "next state"
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
  (let [set-rows (range (count u))]
    (for [y set-rows
          x set-rows] [y x])))

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

(def *size-cell 10)

(defn get-gfx "Given a width and a height, returns a frame with these dimension"
  [width height]
  (.getGraphics
   (doto (java.awt.Frame.)
     (.setSize width height)
     (.setVisible true))))

;; better implementation ?
(defn random-universe "Random universe"
  [size]
  (vec (map vec (partition-all size
                               (for [y (range size)
                                     x (range size)] (rand-int 2))))))

(defn draw "Draw the game of life"
  [gfx w h u]
  (let [blank-color (java.awt.Color. 255 255 255)
        color {0 blank-color
               1 (java.awt.Color. 0 0 0)}
        offset 29]
    (doseq [x (range (count u))
            y (range (count u))]
      (let [state (get-in u [y x])]
        ;; clear the painting
        (.setColor gfx blank-color)
        (.fillRect gfx
                   (* *size-cell x)
                   (+ offset (* *size-cell y))
                   *size-cell *size-cell)
        ;; optimisation for display
        (when (= 1 state)
          ;; draw the new state if needed
          (.setColor gfx (color state))
          (.fillRect gfx
                     (* *size-cell x)
                     (+ offset (* *size-cell y))
                     *size-cell *size-cell))))))

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