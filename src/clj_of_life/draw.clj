(ns clj-of-life.draw "A namespace to help in drawing the game of life")

(def *size-cell 10);; size of the cell
(def *offset 29)   ;; for the border drawn in gnome (do not work under stumpwm)

(defn- get-gfx "Given a width and a height, returns a frame with these dimension"
  [width height]
  (.getGraphics
   (doto (javax.swing.JFrame.)
     (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
     (.setSize width height)
     (.setVisible true))))

(defn- draw-cell "Given a color and a cell's coordinate, draw the cell with the color 'col'"
  [gfx col y x]
  (.setColor gfx col)
  (.fillRect gfx
             (* *size-cell x)
             (+ *offset (* *size-cell y))
             *size-cell *size-cell))

(defn get-drawing-setup "A utility function to retrieve the drawing settings."
  [rows]
  (let [wh (* rows *size-cell)]
    (get-gfx wh wh)))

(defn draw "Draw the game of life"
  [gfx n u]
  (let [color {:dead java.awt.Color/WHITE
               :live java.awt.Color/BLACK}
        r (range n)]
    (doseq [x r, y r]
      ;; clear the painting
      (draw-cell gfx (:dead color) x y)
      ;; optimisation for display
      (when (u [x y])
        ;; draw the new state if needed
        (draw-cell gfx (:live color) x y)))))
