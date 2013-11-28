(ns clj-of-life.core
  (:use midje.sweet))

;; Rules:
;; - live cell with 2 or 3 nb stays live
;; - live cell with less than 2 dies
;; - live cell with more than 3 dies
;; - dead cell with 3 comes to life

(defn neighbours "Compute the neighbours of a cell"
  [[x y]]
  (for [dx [-1 0 1], dy [-1 0 1]
        :when (not= dx dy 0)]
    [(+ x dx) (+ y dy)]))

(defn stepper "Compute the new universe from the old one depending on the neighbours-fn"
  [neighbours-fn]
  (fn [alive-cells]
    (let [freq (frequencies (mapcat neighbours-fn alive-cells))]
      (set (for [[cell n] freq
                 :when (or (= n 3)
                           (and (= n 2)
                                (alive-cells cell)))]
             cell)))))

;; one simple game of life implem
(def next-state-universe (stepper neighbours))

(fact
  (next-state-universe #{[0 1]})  => #{}
  (next-state-universe #{[0 1] [0 2]}) => #{}
  (next-state-universe #{[0 1] [0 2] [1 1]}) => #{[0 1] [0 2] [1 1] [1 2]})

;; ------------------------ Side effects -------------------------

(def *size-cell 10);; size of the cell
(def *offset 29)   ;; for the border drawn in gnome (do not work under stumpwm)

(defn get-gfx "Given a width and a height, returns a frame with these dimension"
  [width height]
  (.getGraphics
   (doto (javax.swing.JFrame.)
     (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
     (.setSize width height)
     (.setVisible true))))

(defn random-universe "Generate a random universe of live cells"
  [size]
  (let [n (rand-int (/ (* size size) 2))]
    (set (repeatedly n (fn [] [(rand-int size) (rand-int size)])))))

(defn- draw-cell "Given a color and a cell's coordinate, draw the cell with the color col"
  [gfx col y x]
  (.setColor gfx col)
  (.fillRect gfx
             (* *size-cell x)
             (+ *offset (* *size-cell y))
             *size-cell *size-cell))

(defn draw "Draw the game of life"
  [gfx n u]
  (let [color {:dead java.awt.Color/WHITE
               :live java.awt.Color/BLACK}
        r (range n)]
    (doseq [x r, y r]
      (draw-cell gfx (:dead color) x y)       ;; clear the painting
      (when (u [x y])                         ;; optimisation for display
        (draw-cell gfx (:live color) x y))))) ;; draw the new state if needed

(defn game-of-life "Game of life"
  ([n]   (game-of-life n (random-universe n)))
  ([n u] (let [w (* *size-cell n)
               h (* *size-cell n)
               gfx (get-gfx w h)]
           (iterate (fn [u] (let [nxt-universe (next-state-universe u)]
                             (do (draw gfx n nxt-universe)
                                 (Thread/sleep 300)
                                 nxt-universe))) u))))
