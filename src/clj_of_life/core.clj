(ns clj-of-life.core
  (:use midje.sweet))

(def alive? {1 true
             0 false})

(defn neighbours-coord
  [y x]
  (for [b [-1 0 1]
        a [-1 0 1]
        :let [y+ (+ b y)
              x+ (+ a x)]
        :when (not= [y+ x+] [y x])]
    [y+ x+]))

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

;; dispatch on the state's cell
(defmulti cell-read :state)

;; cell alive
(defmethod cell-read 1 [{:keys [nb-neighbours]}]
  (cond (< nb-neighbours 2)    0
        (<= 2 nb-neighbours 3) 1
        :else                  0))

(fact
  (cell-read {:state 1
              :nb-neighbours 1}) => 0
  (cell-read {:state 1
              :nb-neighbours 2}) => 1
  (cell-read {:state 1
              :nb-neighbours 3}) => 1
  (cell-read {:state 1
              :nb-neighbours 4}) => 0)

;; cell dead
(defmethod cell-read 0 [{:keys [nb-neighbours]}]
      (if (= 3 nb-neighbours) 1 0))

(fact
  (cell-read {:state 0
              :nb-neighbours 0}) => 0
  (cell-read {:state 0
              :nb-neighbours 1}) => 0
  (cell-read {:state 0
              :nb-neighbours 2}) => 0
  (cell-read {:state 0
              :nb-neighbours 3}) => 1
  (cell-read {:state 0
              :nb-neighbours 4}) => 0)

(defn next-state-cell "Given a universe u and a cell with coordinate y x, compute the next state of the cell [y x] in the universe u"
  [u y x]
  (cell-read
   {:state (get-in u [y x])
    :nb-neighbours (count (filter alive? (neighbours-state u y x)))}))

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
                               [0 0 0 0]])

;; ------------------------ Side effects -------------------------

(def *size-cell 10)

(defn get-gfx "Given a width and a height, returns a frame with these dimension"
  [width height]
  (.getGraphics
   (doto (java.awt.Frame.)
     (.setSize width height)
     (.setVisible true))))

(defn random-universe "Random universe"
  [size]
  (vec (map vec (partition-all size
                               (for [x (range size)
                                     y (range size)] (rand-int 2))))))

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
  [n]
  (let [w (* *size-cell n)
        h (* *size-cell n)
        gfx (get-gfx w h)]
    (iterate (fn [u] (let [nxt-universe (next-state u)]
                      (do (draw gfx w h nxt-universe)
                          (Thread/sleep 300)
                          nxt-universe))) (random-universe n))))