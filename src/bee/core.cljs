(ns bee.core
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn make-pos
  [x y]
  {:x x :y y})

(defn add-pos
  [pos1 pos2]
  (let [{x1 :x y1 :y} pos1
        {x2 :x y2 :y} pos2]
    {:x (+ x1 x2) :y (+ y1 y2)}))

(defn dist
  [pos1 pos2]
  (let [{x1 :x y1 :y} pos1
        {x2 :x y2 :y} pos2
        dx (- x2 x1)
        dy (- y2 y1)]
    (js/Math.hypot dx dy)))

(def part-dim {:x 120 :y 140})

(def snap-dist (/ (dist part-dim {:x 0 :y 0}) 2))

(defn
  make-part
  []
  {:pos {:x 0 :y 0}
   :grid-pos nil
   :tiles [
     {:style 1 :pos {:x 0 :y 0}}
     {:style 1 :pos {:x 1 :y 0}}
   ]})

(defn
  make-grid
  [nx ny]
  (flatten
    (map
      (fn [y]
        (map
          (fn [x] {:pos {:x x :y y} :occupied false})
          (range nx)))
      (range ny))))

(def part (make-part))

(defonce game-state
  (atom {
    :last-pos {:x 0 :y 0}
    :dragging false
    :parts {1 part 2 part}
    :grid (make-grid 2 2)}
    :snap nil
    :won false))

(defn drag [state tag dx dy]
  (->
    state
    (update-in [:parts tag :pos :x] #(+ dx %))
    (update-in [:parts tag :pos :y] #(+ dy %))))

(defn tilepos [pos]
  (let [{tx :x ty :y} pos
        odd (mod ty 2)
        x (* (+ tx (/ odd 2)))
        y (* ty (/ 3 4))
        x (* (:x part-dim) x)
        y (* (:y part-dim) y)]
    {:x x :y y}))

(defn nearest-grid [state pos]
  (let [near (reduce
              (fn [tilea tileb]
                (let [posa (tilepos (:pos tilea))
                      posb (tilepos (:pos tileb))
                      dista (dist posa pos)
                      distb (dist posb pos)]
                  (if (< dista distb)
                    tilea
                    tileb)))
              (:grid state))]
    [(dist (tilepos (:pos near)) pos) near]))

(defn find-snap [state]
  (let [tag (:dragging state)
        part (get-in state [:parts tag])]
    (if part
      (let [anchor (:pos part)
            [d grid] (nearest-grid state anchor)]
        (if (< d snap-dist)
          grid)))))

(defn can-occupy [state pos]
  (let [tile (first (filter #(= pos (:pos %)) (:grid state)))]
    (and tile (not (:occupied tile)))))

(defn can-snap [state grid-pos part]
  (let [tag (:dragging state)
        part (get-in state [:parts tag])
        {gx :x gy :y} grid-pos]
    (every?
      #(can-occupy state
                   (let [{x :x y :y} (:pos %)]
                    {:x (+ gx x) :y (+ gy y)}))
      (:tiles part))))

(defn update-grid-marks
  [state tag occupied]
  (let [tiles (get-in state [:parts tag :tiles])
        grid-pos (get-in state [:parts tag :grid-pos])
        grids (set (map #(add-pos (:pos %) grid-pos) tiles))]
    (if grid-pos
      (update state :grid
          #(doall (map
             (fn [tile]
               (if (contains? grids (:pos tile))
                   (assoc tile :occupied occupied)
                   tile))
             %)))
      state)))

(defn update-win
  [state]
  (let [won (every? #(:occupied %) (:grid state))]
    (assoc state :won won)))

(defn update-snap
  [state grid]
  (let [tag (:dragging state)]
    (->
      state
      (assoc-in [:parts tag :grid-pos] (:pos grid))
      (update-grid-marks tag true)
      (assoc-in [:parts tag :pos] (tilepos (:pos grid)))
      (update-win))))

(defn do-snap [state]
  (let [tag (:dragging state)
        part (get-in state [:parts tag])]
    (if-let [grid (find-snap state)]
      (if (can-snap state (:pos grid) part)
        (update-snap state grid)
        state)
      state)))

(defn stop [state e]
  (let [state (do-snap state)]
    (assoc state :dragging false)))

(defn move [state e]
  (let [x (.-clientX e)
        y (.-clientY e)
        dx (- x (get-in state [:last-pos :x]))
        dy (- y (get-in state [:last-pos :y]))
        tile (:dragging state)
        state (->
                 state
                 (assoc-in [:last-pos :x] x)
                 (assoc-in [:last-pos :y] y))]
    (if tile
      (drag state tile dx dy)
      state)))

(defn start [state e tag]
  (set! (.-onmousemove js/document) #(swap! game-state move %1))
  (set! (.-onmouseup js/document) #(swap! game-state stop %1))
  (->
    state
    (update-grid-marks tag false)
    (assoc-in [:parts tag :grid-pos] nil)
    (assoc-in [:last-pos :x] (.-clientX e))
    (assoc-in [:last-pos :y] (.-clientY e))
    (assoc :dragging tag)))

(defn render-tile [tile tag]
  (let [{x :x y :y} (tilepos (:pos tile))
        style (get tile :style 0)]
    [:img.tile {:src (str "hex" style ".png")
                :style {:left x :top y}
                :onMouseDown #(swap! game-state start %1 tag)
               }]))

(defn render-part [part tag]
  (let [{x :x y :y} (:pos part)]
    [:div.part
     {:style {:left x :top y} :key tag}
     (doall (map render-tile (:tiles part) (repeat tag)))
    ]))

(defn render-grid-tile [state tile]
  (let [{tx :x ty :y} (:pos tile)
        {x :x y :y} (tilepos (:pos tile))]
    [:img.gridtile {:src (str "hex0.png")
                :id (str "grid" tx "x" ty)
                :style
     (cond->
       {:left x :top y}
       (= (:snap state) (:pos tile)) (assoc :background-color "#00ff00"))}]))

(defn render-grid [state]
  [:div.grid
    (doall (map (partial render-grid-tile state) (:grid state)))])

(defn render-game [state]
  (sab/html
    [:div
      [:div (str state)]
      (doall (map (fn [[k v]] (render-part v k)) (:parts state)))
      (render-grid state)]))

(defn renderer [state]
  (.render js/ReactDOM (render-game state) (. js/document (getElementById "app"))))

(add-watch game-state
  :renderer (fn [_ _ _ n]
        (renderer n)))

(reset! game-state @game-state)
