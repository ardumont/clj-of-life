(ns clj-of-life.game "A namespace to compute the game of life policy"
  (:require [clj-of-life.draw :as d]
            [midje.sweet :as m]))

;; Rules:
;; - live cell with 2 or 3 nb stays live
;; - live cell with less than 2 dies
;; - live cell with more than 3 dies
;; - dead cell with 3 comes to life

(defn stepper
  "Compute the new universe (of live cells) from the previous one depending on the neighbours-fn function."
  [neighbours-fn]
  (fn [universe]
    (let [freq (frequencies (mapcat neighbours-fn universe))]
      (set (for [[cell n] freq
                 :when (or (= n 3)
                           (and (= n 2)
                                (universe cell)))]
             cell)))))

(defn neighbours
  "Compute the neighbours of a cell"
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= dx dy 0)]
    [(+ x dx) (+ y dy)]))

(m/fact :simple-check-around-neighbours
  (neighbours [0 0]) => (m/contains [-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1] :in-any-order)
  (neighbours [3 1]) => (m/contains [2 0] [2 1] [2 2] [3 0] [3 2] [4 0] [4 1] [4 2] :in-any-order))

;; one simple game of life implem
(def next-state-universe (stepper neighbours))

(m/fact
  (next-state-universe #{[0 1]})  => #{}
  (next-state-universe #{[0 1] [0 2]}) => #{}
  (next-state-universe #{[0 1] [0 2] [1 1]}) => #{[0 1] [0 2] [1 1] [1 2]})

(defn random-universe "Generate a random universe of live cells"
  [size]
  (let [n (rand-int (/ (* size size) 2))]
    (set (repeatedly n (fn [] [(rand-int size) (rand-int size)])))))

;; clj-of-life.game> (random-universe 10)
;; #{[9 8] [1 2] [7 1] [9 3] [6 2] [5 2] [7 4] [4 2]}
;; clj-of-life.game> (random-universe 10)
;; #{[4 3] [7 6] [8 7] [1 0] [2 2] [9 9] [0 0] [1 1] [2 3] [3 4] [6 7] [7 8] [0 1] [4 6] [7 9] [0 3] [3 7] [4 8] [5 9] [0 4] [4 9] [0 5] [1 6] [3 9] [0 6] [8 0] [8 1] [9 2] [8 2] [9 3] [6 1] [7 2] [6 2] [9 6] [4 2] [6 4]}

;; ------------------------ Side effects -------------------------

(defn game-of-life "Game of life: Given a number of rows, display a game of life with rows x rows frame."
  ([rows]
     (game-of-life rows (random-universe rows)))
  ([rows universe]
     (let [gfx (d/get-drawing-setup rows)]
       (iterate (fn [univ] (let [nxt-universe (next-state-universe univ)]
                         (do (d/draw gfx rows nxt-universe)
                             (Thread/sleep 300)
                             nxt-universe)))
                universe))))
