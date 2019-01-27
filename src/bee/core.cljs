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

(defn mul-pos
  [pos1 pos2]
  (let [{x1 :x y1 :y} pos1
        {x2 :x y2 :y} pos2]
    {:x (* x1 x2) :y (* y1 y2)}))

(defn dist
  [pos1 pos2]
  (let [{x1 :x y1 :y} pos1
        {x2 :x y2 :y} pos2
        dx (- x2 x1)
        dy (- y2 y1)]
    (js/Math.hypot dx dy)))

(def tile-dim {:x 120 :y 140})

(def snap-dist (/ (dist tile-dim {:x 0 :y 0}) 2))

(defn
  make-part
  [tiles]
  (let [style (inc (rand-int 6))
        tiles (apply vector tiles)
        {ax :x ay :y} (first tiles)]
    (print ax ay)
    {:pos {:x 600 :y 0}
     :grid-pos nil
     :tiles (doall
              (map (fn [pos]
                  {:style style
                   :pos {:x (- (:x pos) ax) :y (- (:y pos) ay)}}) tiles))}))

(defn average
  [& args]
  (/ (apply + args) (count args)))

(defn average-pos
  [& args]
  {:x (apply average (map :x args)) :y (apply average (map :y args))})

(defn tile-center
  [tile]
  (mul-pos tile-dim (add-pos (:pos tile) (make-pos 0.5 0.5))))

(defn half-pos
  [pos]
  (mul-pos (make-pos 0.5 0.5) pos))

(defn part-center
  [part]
  (apply average-pos (map tile-center (:tiles part))))

(defn part-dim
  [part]
  (let [min-x (apply min (map #(:x (:pos %)) (:tiles part)))
        max-x (apply max (map #(:x (:pos %)) (:tiles part)))
        min-y(apply min (map #(:y (:pos %)) (:tiles part)))
        max-y (apply max (map #(:y (:pos %)) (:tiles part)))
        dx (+ (- max-x min-x) 1)
        dy (+ (- max-y min-y) 1)]
    (mul-pos tile-dim (make-pos dx dy))))

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

(def tiles [{:x 0 :y 0} {:x 1 :y 0}])

(defn parse-game
  [source]
  (let [stripped (.replace source (js/RegExp. " " "g") "")
        names (set (filter (fn [c] (not (= c "."))) (.replace (.replace stripped (js/RegExp. "\n" "g") ""))))
        lines (js->clj (.split stripped "\n"))
        tiles (flatten (map (fn [y line]
                  (map (fn [x ch]
                      {:tag ch :x (+ x (quot (+ y 1) 2)) :y y})
                    (range) line))
                (range) lines))
        parts (reduce (fn [m [k v]] (assoc m k v)) nil
                (map (fn [ch]
                  [(keyword ch)
                   (make-part
                     (apply vector (filter (fn [tile] (= (:tag tile) ch)) tiles)))])
                  names))
        grid (doall
               (map (fn [tile]
                    {:pos {:x (:x tile) :y (:y tile)} :occupied false})
                  (filter (fn [tile] (not (= (:tag tile) "."))) tiles)))]
    {:grid grid :parts parts}))

(defn init-state []
  {:last-pos {:x 0 :y 0}
   :dragging false
   :parts {}
   :grid {}
   :won false
   :interact false
   :title ""
   :time 0
   :show false
   :menu :main})

(defonce game-state
  (atom (init-state)))

(defn drag [state tag dx dy]
  (->
    state
    (update-in [:parts tag :pos :x] #(+ dx %))
    (update-in [:parts tag :pos :y] #(+ dy %))))

(defn tilepos [pos]
  (let [{tx :x ty :y} pos
        x (* (:x tile-dim) (- tx (/ ty 2)))
        y (* (/ 3 4) (:y tile-dim) ty)]
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

(defn update-win
  [state]
  (if (every? #(:occupied %) (:grid state))
    (->
      state
      (assoc :menu :win)
      (assoc :won true)
      (assoc :dragging false)
      (assoc :interact false))
    state))

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
  (if (:interact state)
    (let [state (do-snap state)]
      (assoc state :dragging false))
    (assoc state :dragging false)))

(defn find-scale []
  (if-let [scalebox (js/document.getElementById "scalebox")]
    (let [rect (.getBoundingClientRect scalebox)]
      (make-pos (/ (.-width rect) 100) (/ (.-height rect) 100)))
    (make-pos 1 1)))

(defn move [state e]
  (if (:interact state)
    (if (:dragging state)
      (if (bit-and 1 (.-buttons e))
        (let [x (.-clientX e)
              y (.-clientY e)
              {sx :x sy :y} (find-scale)
              dx (/ (- x (get-in state [:last-pos :x])) sx)
              dy (/ (- y (get-in state [:last-pos :y])) sy)
              tile (:dragging state)
              state (->
                       state
                       (assoc-in [:last-pos :x] x)
                       (assoc-in [:last-pos :y] y))]
          (if tile
            (drag state tile dx dy)
            state))
        (stop state e))
      state)
    (assoc state :dragging false)))

(defn timer-tick
  [state]
  (if (and (:show state) (:interact state) (not (:won state)))
    (update state :time inc)
    state))

(defonce timer (js/setInterval #(swap! game-state timer-tick) 1000))

(defn start [state e tag]
  (set! (.-onmousemove js/document) #(swap! game-state move %1))
  (set! (.-onmouseup js/document) #(swap! game-state stop %1))
  (if (:interact state)
    (->
      state
      (update-grid-marks tag false)
      (update-win)
      (assoc-in [:parts tag :grid-pos] nil)
      (assoc-in [:last-pos :x] (.-clientX e))
      (assoc-in [:last-pos :y] (.-clientY e))
      (assoc :dragging tag))
    state))

(defn render-tile [tile tag]
  (let [{x :x y :y} (tilepos (:pos tile))
        style (get tile :style 0)
        mapname (str "tile" tag "at" (:x (:pos tile)) "x" (:y (:pos tile)))]
    [:div.tile
       {:style {:left x :top y}}
       [:map {:name mapname}
          [:area {:shape "poly" :coords "-1,106,-1,35,59,0,120,36,120,105,61,139"
                :onMouseDown #(swap! game-state start %1 tag)
              }]]
       [:img {:src (str "hex" style ".svg")
               :useMap (str "#" mapname)
                :width (:x tile-dim)
                :height (:y tile-dim)
               }
       ]
     ]))


(defn render-part [part tag]
  (let [{x :x y :y} (:pos part)]
    [:div.part
     {:style {:left x :top y} :key tag}
     (doall (map render-tile (:tiles part) (repeat tag)))
    ]))

(defn render-grid-tile [state tile]
  (let [{tx :x ty :y} (:pos tile)
        {x :x y :y} (tilepos (:pos tile))]
    [:img.gridtile {:src (str "hex0.svg")
                :width (:x tile-dim)
                :height (:y tile-dim)
                :id (str "grid" tx "x" ty)
                :style
       {:left x :top y}}]))

(defn render-grid [state]
  [:div.grid
    (doall (map (partial render-grid-tile state) (:grid state)))])

(defn format-time [sec]
  (let [[min sec] [(quot sec 60) (rem sec 60)]
        [hr min] [(quot min 60) (rem min 60)]]
    (str
      (if (> hr 0) (str (if (< hr 10) "0") hr ":"))
      (if (and (> hr 0) (< min 10)) "0") min ":"
      (if (< sec 10) "0") sec)))

(def shuffle-center (make-pos 700 150))
(def shuffle-range 170)

(defn rand-pos
  [center dist]
  (let [a (* 2 js/Math.PI (rand))
        r (* dist (js/Math.sqrt (rand)))
        x (+ (:x center) (* r (js/Math.cos a)))
        y (+ (:y center) (* r (js/Math.sin a)))]
    {:x x :y y}))

(defn shuffle-part [state tag]
  (->
    state
    (assoc-in [:parts tag :pos] (rand-pos shuffle-center shuffle-range))
    (assoc :dragging false)))

(defn shuffle-parts [state]
  (reduce shuffle-part state (filter (fn [tag] (not (get-in state [:parts tag :grid-pos]))) (keys (:parts state)))))

(defn unlock-part [state tag]
  (->
    state
    (update-grid-marks tag false)
    (assoc-in [:parts tag :grid-pos] nil)))

(defn unlock-all [state]
  (reduce unlock-part state (keys (:parts state))))

(defn reset-game [state]
  (->
    state
    (unlock-all)
    (shuffle-parts)
    (assoc :dragging false)
    (assoc :won false)
    (assoc :interact true)
    (assoc :time 0)
    (assoc :menu nil)
    (assoc :show true)))

(defn go-menu [state menu]
  (->
    state
    (assoc :interact false)
    (assoc :show false)
    (assoc :menu menu)))

(defn start-level [state title source]
  (let [level (parse-game source)]
    (print level)
    (->
      state
      (assoc :parts (:parts level))
      (assoc :grid (:grid level))
      (assoc :title title)
      (reset-game))))

(defmulti render-menu (fn [state] (:menu state)))

(defmethod render-menu :default [state])

(defmethod render-menu :main [state]
  [:div {:class "titlebg fullscreenbg"}
    [:div {:class "titlecontainer"}
      [:div {:class "logocontainer"}
        [:img {:class "logo" :src "logo.svg"}]]
    [:div {:class "centeredbuttoncontainer"}
     [:a {:onClick #(swap! game-state go-menu :level)}
      [:img {:class "button playbutton" :src "/btn_play.svg"}]]]
    [:div.snowflakes {:aria-hidden true}
     [:div.snowflake "✿"]
     [:div.snowflake "❀"]
     [:div.snowflake "❁"]
     [:div.snowflake "✿"]
     [:div.snowflake "❀"]
     [:div.snowflake "❁"]
     [:div.snowflake "✿"]
     [:div.snowflake "❀"]
     [:div.snowflake "❁"]]]])

(def level1
". A A
  B . A
 . C C")

(def level2
". A . B
  A B B
 . A . B
  . C C")


(def level3
". A . B
  A B B C C
 . A D B C
  E D D
 . E")


(def level4
". . A
  B C C D
 . C . C D
  . E E . D
 . . F F G G
  . . F F G")


(def level5
". . A
  . B B C
 . B D B C
  B E E F C
 . E G G F F
  . E H
 . . E")

(defmethod render-menu :level [state]
  [:div
    {:class "menubg fullscreenbg"}
    [:div
     {:class "menucontainer"}
	[:div [:h1 "Select Level"]]

        [:div {:class "levelcolumn"}
          [:div {:class "levelrow"}
          [:img {:class "beeimg" :src "bee_worker.png"}]
	  [:h2 "Worker"]
          [:div {:class "levelscontainer"}
            [:a
             {:onClick #(swap! game-state start-level "Level 1" level1)}
             [:img {:class "lvlicon button" :src "ico_lvl1.svg"}]]
            [:a
             {:onClick #(swap! game-state start-level "Level 2" level2)}
             [:img {:class "lvlicon button" :src "ico_lvl2.svg"}]]
          ]
          [:div {:class "levelscontainer"}
            [:a
             {:onClick #(swap! game-state start-level "Level 3" level3)}
             [:img {:class "lvlicon button" :src "ico_lvl3.svg"}]]]
          [:div {:class "levelscontainer"}
            [:a
             {:onClick #(swap! game-state start-level "Level 4" level4)}
             [:img {:class "lvlicon button" :src "ico_lvl4.svg"}]]
            [:a
             {:onClick #(swap! game-state start-level "Level 5" level5)}
             [:img {:class "lvlicon button" :src "ico_lvl5.svg"}]]
          ]]

          [:div {:class "levelrow"}
          [:img {:class "beeimg" :src "bee_drone.png"}]
	  [:h2 "Drone"]
          [:div {:class "levelscontainer"}
            [:a [:img {:class "lvlicon button" :src "ico_lvl1.svg"}]]
            [:a [:img {:class "lvlicon button" :src "ico_lvl2.svg"}]]
          ]
          [:div {:class "levelscontainer"}
            [:a [:img {:class "lvlicon button" :src "ico_lvl3.svg"}]]]
          [:div {:class "levelscontainer"}
            [:a [:img {:class "lvlicon button" :src "ico_lvl4.svg"}]]
            [:a [:img {:class "lvlicon button" :src "ico_lvl5.svg"}]]
          ]]

          [:div {:class "levelrow"}
          [:img {:class "beeimg" :src "bee_queen.png"}]
	  [:h2 "Queen"]
          "\n"
          [:div {:class "levelscontainer"}
            [:a [:img {:class "lvlicon button" :src "ico_lvl1.svg"}]]
            [:a [:img {:class "lvlicon button" :src "ico_lvl2.svg"}]]
          ]
          "\n"
          [:div {:class "levelscontainer"}
            [:a [:img {:class "lvlicon button" :src "ico_lvl3.svg"}]]]
          "\n"
          [:div {:class "levelscontainer"}
            [:a [:img {:class "lvlicon button" :src "ico_lvl4.svg"}]]
            [:a [:img {:class "lvlicon button" :src "ico_lvl5.svg"}]]
          ]]]]])

(defmethod render-menu :win [state]
  [:div.overlay
   "YOU WIN!"
   [:index {:type "button" :value "menu" :onClick #(swap! game-state go-menu :main)}]
   [:index {:type "button" :value "retrt" :onClick #(swap! game-state reset-game)}]
   [:index {:type "button" :value "next"}]
   ])

(defn pause-game [state]
  (if (:interact state)
    (->
      state
      (assoc :interact false)
      (assoc :menu :pause))
    state))

(defn unpause-game [state]
  (if (= (:menu state) :pause)
    (->
      state
      (assoc :interact (not (:won state)))
      (assoc :menu nil))
    state))

(defmethod render-menu :pause [state]
  [:div "PAUSED"
     [:input {:type "button" :value "Menu" :onClick #(swap! game-state go-menu :level)}]
     [:input {:type "button" :value "Unpause" :onClick #(swap! game-state unpause-game)}]])

(defn render-game [state]
  (sab/html
    [:div.root
     (render-menu state)
     (if (:show state)
       [:div.game
        {:id "gamearea" :style {:transform "scaled(0.5)"}}
        [:div.scalebox {:id "scalebox" :style {:height "100px" :width "100px"}}]
;        [:input {:type "button" :style {:background-image "url('/menu.jpg')" :height 100 :width 100}
;                 :onClick #(swap! game-state pause-game)}]
;        [:input {:type "button" :style {:background-image "url('/retry.jpg')" :height 100 :width 100}
;                 :onClick #(swap! game-state reset-game)}]
;        [:input {:type "button" :value "SHUFF" :style {:height 100 :width 100}
;                 :onClick #(swap! game-state shuffle-parts)}]
        [:h1.title (:title state)]
        [:h4.time (str "Time: " (format-time (:time state)))]
        [:h4 (str "WON: " (:won state))]
        (doall (map (fn [[k v]] (render-part v k)) (:parts state)))
        (render-grid state)
        [:div.gridarea {:id "gridarea"}]
        [:div.partarea {:id "partarea"}]
        ])
     ]))

(defn renderer [state]
  (.render js/ReactDOM (render-game state) (. js/document (getElementById "app"))))

(add-watch game-state
  :renderer (fn [_ _ _ n]
        (renderer n)))

(reset! game-state @game-state)

;TODO
;Scaling
;Winner screen
;Make play nice
;Random grid
;Random puzzle
;Random maze/fixed maze
