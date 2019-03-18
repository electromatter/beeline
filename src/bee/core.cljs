(ns bee.core
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]))

(enable-console-print!)

(defn play-sound [sound]
  (if-let [elem (. js/document (getElementById (str "sfx_" sound)))]
    (.play elem)))

(defn make-pos
  [x y]
  {:x x :y y})

(defn add-pos
  [pos1 pos2]
  (let [{x1 :x y1 :y} pos1
        {x2 :x y2 :y} pos2]
    {:x (+ x1 x2) :y (+ y1 y2)}))

(defn sub-pos
  [pos1 pos2]
  (let [{x1 :x y1 :y} pos1
        {x2 :x y2 :y} pos2]
    {:x (- x1 x2) :y (- y1 y2)}))

(defn mul-pos
  [pos1 pos2]
  (let [{x1 :x y1 :y} pos1
        {x2 :x y2 :y} pos2]
    {:x (* x1 x2) :y (* y1 y2)}))

(defn mag
  [pos]
  (js/Math.hypot (:x pos) (:y pos)))

(defn dist
  [pos1 pos2]
  (let [{x1 :x y1 :y} pos1
        {x2 :x y2 :y} pos2
        dx (- x2 x1)
        dy (- y2 y1)]
    (js/Math.hypot dx dy)))

(defn make-box [left top right bottom]
  {:left left :top top :right right :bottom bottom})

(defn get-box [id]
  (if-let [ele (js/document.getElementById id)]
    (let [rect (.getBoundingClientRect ele)]
      {:left (.-left rect) :top (.-top rect) :right (.-right rect) :bottom (.-bottom rect)})
    nil))

(defn shift-box [box pos]
  (if box
    (let [{l :left t :top r :right b :bottom} box
          {x :x y :y} pos]
      {:left (+ l x) :top (+ t y) :right (+ r x) :bottom (+ b y)})))

(defn scale-box [box scale]
  (if box
    (let [{l :left t :top r :right b :bottom} box]
      {:left (* scale l) :top (* scale t) :right (* scale r) :bottom (* scale b)})))

(defn get-box-rel [boxid relid]
  (if-let [box (get-box boxid)]
    (if-let [rel (get-box relid)]
      (let [{x :left y :top} rel
            box (shift-box box {:x (- x) :y (- y)})]
        box))))

(defn box-pos [box]
  (if (not box)
    (make-pos 0 0)
    (let [{x :left y :top} box]
      (make-pos x y))))

(defn merge-boxes [a b]
  (if (not a)
    b
    (if (not b)
      a
      (let [{al :left at :top ar :right ab :bottom} a
            {bl :left bt :top br :right bb :bottom} b]
        {:left (min al bl) :top (min ar br) :right (max ar br) :bottom (max ab bb)}))))

(defn mid [& args]
  (if (not args)
    0
    (/ (apply + args) (count args))))

(defn box-center [box]
  (if (not box)
    (make-pos 0 0)
    (let [{l :left t :top r :right b :bottom} box]
      (make-pos (mid l r) (mid t b)))))

(defn box-dim [box]
  (if (not box)
    (make-pos 0 0)
    (let [{l :left t :top r :right b :bottom} box]
      (make-pos (- r l) (- b t)))))

(defn fit-box-into-center [box container]
  (let [{iw :x ih :y} (box-dim box)
        {cw :x ch :y} (box-dim container)
        bc (box-center box)
        {x :x y :y} bc
        cc (box-center container)]
    (if (and (> cw iw) (> ch ih))
      [1 (sub-pos cc bc)]
      (let [ratio (min (/ cw iw) (/ ch ih))]
        [ratio (sub-pos cc {:x (* ratio x) :y (* ratio y)})]))))

(def tile-dim {:x 120 :y 140})
(def snap-dist (/ (mag tile-dim) 2))

(defn min-max-pos [& args]
  (let [min-x (apply min (map #(:x %) args))
        max-x (apply max (map #(:x %) args))
        min-y (apply min (map #(:y %) args))
        max-y (apply max (map #(:y %) args))]
    [(make-pos min-x min-y) (make-pos max-x max-y)]))

(defn tilepos [pos]
  (let [{tx :x ty :y} pos
        x (* (:x tile-dim) (- tx (/ ty 2)))
        y (* (/ 3 4) (:y tile-dim) ty)]
    {:x x :y y}))

(defn
  make-part
  [tiles]
  (let [style (inc (rand-int 6))
        tiles (apply vector tiles)
        {ax :x ay :y} (first tiles)
        [ln lx] (apply min-max-pos (map tilepos tiles))
        [rn rx] (apply min-max-pos (map #(add-pos (tilepos %) tile-dim) tiles))
        [min-pos max-pos] (min-max-pos ln lx rn rx)
        dim (sub-pos max-pos min-pos)]
    {:pos {:x 600 :y 0}
     :grid-pos nil
     :z 15
     :dim dim
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

(defn
  grid-box
  [grid]
  (let [[{l :x  t :y} {r :x b :y}] (apply min-max-pos
                                     (concat
                                       (apply min-max-pos (map #(tilepos (:pos %)) grid))
                                       (apply min-max-pos (map #(add-pos (tilepos (:pos %)) tile-dim) grid))))]
    {:left l :top t :right r :bottom b}))

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
   :start-pos {:x 0 :y 0}
   :dragging false
   :next-z 20
   :parts {}
   :grid {}
   :box nil
   :scale 1
   :grid-pos {:x 0 :y 0}
   :won false
   :interact false
   :shuffle-center (make-pos 2000 150)
   :shuffle-range 170
   :title ""
   :time 0
   :level 1
   :show false
   :menu :main})

(defonce game-state
  (atom (init-state)))

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
    (do
      (if (not (:won state)) (play-sound "win"))
      (->
        state
        (assoc :menu :win)
        (assoc :won true)
        (assoc :dragging false)
        (assoc :interact false)))
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
      (assoc-in [:parts tag :z] 10)
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
      (if (:dragging state)
        (play-sound "putdown"))
      (assoc state :dragging false))
    (assoc state :dragging false)))

(defn do-move [state x y]
  (if (:interact state)
    (if (:dragging state)
      (let [s (:scale state)
            dx (/ (- x (get-in state [:start-pos :x])) s)
            dy (/ (- y (get-in state [:start-pos :y])) s)
            delta {:x dx :y dy}
            old (get state :last-pos)
            tile (:dragging state)]
        (if tile
          (assoc-in state [:parts tile :pos] (add-pos old delta))
          state))
      state)
    (assoc state :dragging false)))

(defn move [state e]
  (if (bit-and 1 (.-buttons e))
    (do-move state (.-clientX e) (.-clientY e))
    (stop state e)))

(defn touch-move [state e]
  (.preventDefault e)
  (let [touches (.-touches e)]
    (if-let [touch (aget touches 0)]
      (do-move state (.-clientX touch) (.-clientY touch))
      (stop state nil))))

(def should-play true)

(defn play-music []
  (let [music (. js/document (getElementById "music"))]
    (set! (.-volume music) 0.9)
    (if (and (or (not (.-duration music)) (.-paused music)) should-play)
      (.play music)
      (set! should-play false))))

(defn timer-tick
  [state]
  (play-music)
  (if (and (:show state) (:interact state) (not (:won state)))
    (update state :time inc)
    state))

(defonce timer (js/setInterval #(swap! game-state timer-tick) 1000))

(defonce ev-touch-move (.addEventListener js/document "touchmove" #(do (swap! game-state touch-move %1) (:dragging @game-state)) #js {:passive false}))

(defn start [state e tag]
  (set! (.-onmousemove js/document) #(swap! game-state move %1))
  (set! (.-onmouseup js/document) #(swap! game-state stop %1))
  (set! (.-ontouchend js/document) #(swap! game-state stop %1))
  (if (:interact state)
    (let [x (.-clientX e)
          y (.-clientY e)
          old (get-in state [:parts tag :pos])]
      (play-sound "pickup")
      (->
        state
        (update-grid-marks tag false)
        (update-win)
        (update :next-z inc)
        (assoc-in [:parts tag :grid-pos] nil)
        (assoc-in [:parts tag :z] (:next-z state))
        (assoc :last-pos old)
        (assoc-in [:start-pos :x] x)
        (assoc-in [:start-pos :y] y)
        (assoc :dragging tag)))
    state))

(defn touch-start [state e tag]
  (if-let [touch (aget (.-touches e) 0)]
    (start state touch tag)
    state))

(defn tag-from-id [id]
  (if (not (= (type id) js/String))
    nil
    (let [parts (js->clj (.split id ":"))]
      (if (not (= (count parts) 2))
        nil
        (let [[pre tag] parts]
          (if (not (= pre "tile"))
            nil
            (keyword tag)))))))

(defn false-start [e]
  (let [x (.-clientX e) y (.-clientY e)]
    (if (not (:dragging @game-state))
      (loop [restorers nil prev nil]
        (let [elem (js/document.elementFromPoint x y)]
          (if elem
            (let [old-display (.-style.display elem)
                  restorer #(set! (.-style.display elem) old-display)
                  elem-id (.-id elem)
                  tag (tag-from-id elem-id)]
              (if (not (= elem prev))
                (if (not tag)
                  (do
                    (set! (.-style.display elem) "none")
                    (recur (conj restorers restorer) elem))
                  (do
                    (doseq [r restorers] (r))
                    (swap! game-state start e tag)))
                (doseq [r restorers] (r))))
            (doseq [r restorers] (r))))))))

(defn false-touch-start [e]
  (if-let [touch (aget (.-touches e) 0)]
    (false-start touch)))

(defn render-tile [tile tag]
  (let [{x :x y :y} (tilepos (:pos tile))
        style (get tile :style 0)
        mapname (str "tile" tag "at" (:x (:pos tile)) "x" (:y (:pos tile)))]
    [:div.tile
       {:style {:left x :top y}
        :onMouseDown #(false-start %)
        :onTouchStart #(false-touch-start %)}
       [:map {:name mapname}
          [:area {:shape "poly" :coords "-1,106,-1,35,59,0,120,36,120,105,61,139"
                :onDragStart #(do (.preventDefault %1) false)
                :onMouseDown #(swap! game-state start %1 tag)
                :onTouchStart #(do (swap! game-state touch-start %1 tag) false)
                :id (str "tile" tag)}]]
       [:img {:src (str "hex" style ".svg")
               :draggable false
               :onDragStart #(do (.preventDefault %1) false)
               :useMap (str "#" mapname)
                :width (:x tile-dim)
                :height (:y tile-dim)
               }
       ]
     ]))


(defn render-part [part tag]
  (let [{x :x y :y} (:pos part)]
    [:div.part
     {:style {:left x :top y :zIndex (:z part)} :key tag}
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

(defn rand-pos
  [center dist]
  (let [a (* 2 js/Math.PI (rand))
        r (* dist (js/Math.sqrt (rand)))
        x (+ (:x center) (* r (js/Math.cos a)))
        y (+ (:y center) (* r (js/Math.sin a)))]
    {:x x :y y}))

(defn update-shuffle [state]
  (if-let [box (get-box-rel "parts" "gamearea")]
    (let [{x :x y :y} (:grid-pos state)
          box (shift-box box {:x (- x) :y (- y)})
          box (scale-box box (/ 1 (:scale state)))
          {w :x h :y} (box-dim box)
          r (/ (min w h) 5)]
        (->
          state
          (assoc :shuffle-center (box-center box))
          (assoc :shuffle-range r)))
    state))

(defn shuffle-part [state tag]
  (let [shift (mul-pos (get-in state [:parts tag :dim]) (make-pos -0.5 -0.5))
        r (js/Math.hypot (:x shift) (:y shift))
        center (add-pos (:shuffle-center state) shift)
        dist (:shuffle-range state)]
    (->
      state
      (assoc-in [:parts tag :pos] (rand-pos center dist))
      (assoc :dragging false))))

(defn shuffle-parts [state]
  (let [state (update-shuffle state)]
    (reduce shuffle-part state (filter (fn [tag] (not (get-in state [:parts tag :grid-pos]))) (keys (:parts state))))))

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
  (play-sound "back")
  (->
    state
    (assoc :interact false)
    (assoc :show false)
    (assoc :menu menu)))

(defmulti level-source (fn [n] n))

(defmethod level-source :default [n])

(defn fit-level [state]
  (if-let [box (get-box-rel "grid" "gamearea")]
    (let [[scale shift] (fit-box-into-center (:box state) box)]
      (->
        state
        (stop nil)
        (assoc :scale scale)
        (assoc :grid-pos shift)))
    state))

(defonce ev-resize (.addEventListener js/window "resize" #(do (swap! game-state fit-level) true)))

(defn start-level [state n]
  (let [source (level-source n)
        title (str "Level " n)]
    (if (not source)
        (go-menu state :level)
      (let [level (parse-game source)
            box (grid-box (:grid level))]
        (play-sound "play")
      (->
        state
        (assoc :parts (:parts level))
        (assoc :grid (:grid level))
        (assoc :box box)
        (assoc :title title)
        (assoc :level n)
        (fit-level)
        (reset-game))))))

(defmulti render-menu (fn [state] (:menu state)))

(defmethod render-menu :default [state])

(defmethod render-menu :main [state]
  [:div {:class "titlebg fullscreenbg"}
    [:div.snowflakes {:aria-hidden true}
     [:div.snowflake "✿"]
     [:div.snowflake "❀"]
     [:div.snowflake "❁"]
     [:div.snowflake "✿"]
     [:div.snowflake "❀"]
     [:div.snowflake "❁"]
     [:div.snowflake "✿"]
     [:div.snowflake "❀"]
     [:div.snowflake "❁"]]
    [:div {:class "titlecontainer"}
      [:div {:class "mainrow mainpad"}]
      [:div {:class "mainrow logocontainer"}
        [:img {:class "logo" :src "logo.svg"}]]
      [:div {:class "mainrow mainmid"}]
      [:div {:class "mainrow centeredbuttoncontainer"}
        [:img {:class "button playbutton" :src "btn_play.svg"
               :onClick #(do (swap! game-state go-menu :level))}]]
      [:div {:class "mainrow mainpad"}]]])

(defmethod level-source 1 [n]
". A A
  B . A
 . C C")

(defmethod level-source 2 [n]
". A . B
  A B B
 . A . B
  . C C")


(defmethod level-source 3 [n]
". A . B
  A B B C C
 . A D B C
  E D D
 . E")


(defmethod level-source 4 [n]
". . A
  B C C D
 . C . C D
  . E E . D
 . . F F G G
  . . F F G")


(defmethod level-source 5 [n]
". . A
  . B B C
 . B D B C
  B E E F C
 . E G G F F
  . E H
 . . E")

(defmethod level-source 6 [n]
". . A . B
  C D D E B
 C F . F E
  C F F G
 . . H H G")

(defmethod level-source 7 [n]
". A . B C
  A A D B . E
 F G G D D E
  F . G D . I
 H H H H I I")

(defmethod level-source 8 [n]
". . . . B B
  . A A . C B
 . D E F F . F
  H D E G F F
 . H . E G
  . . J J
 . . J . J J
  . . M K L K
 . . M . K K
  . . M M")

(defmethod level-source 9 [n]
". . A A
  . B B B
 . C . . E
  D C . E E
 D . C E . F
  D H . F F .
 G . H H . I
  G G . H I
 J . K K . I
  J J . M I
 . . M M")

(defmethod level-source 10 [n]
". . . A
  . . A A
 . . A . A
  B B C E E
 . D D C . E
  D . C F F F
 . D D F I . H
  . . F . I J J
 . . . G G G . J
  . . . K . K J
 . . . . K K M
  . . . . . M
")

(defmethod level-source 11 [n]
".   A . B B
  A A C C . D
 E E . C C . D
  E E . . G G
 F . . H . J
  F F . . J
 . . K J J . M
  . . K . K
 . . . K K")

(defmethod level-source 12 [n]
". . A . B
  . . A B
 C C . D F F F
  E D D G . I
 E . . . G G
  E H H G . J J
 . . . H G J . M M
  . . K H . . M . M
 . . . K K K . . . M
  . . L . . N . O O
 . . . L L L N O")

(defmethod level-source 13 [n]
". . A B . C C
  . A . B B . C
 . A . . D . . C
  A E E . . F F F
 . H E . I . G G
  H . . I J . . L L
 . H . K K J . L . M M M
  N N . . P P Q Q . . R
 . N N . . P P Q . . R
  . O O O O . Q Q R R
 . . . . S T T
  . . . S S T")

(defmethod level-source 14 [n]
". . A A . . . . B B
  . . A A . . . . . B . C C
 D D D . . . . . E E F F
  .  G . H .. . I . E F
 . . H H . . . I I
  . . H . Z Z J . J
 . . K K K K L J J
  .  . M . L L L L
 . . M M . . X L
  O . P Q . R . N N
 . . . Q Q . . N N
  U V V . Q . . S S
 U V . V V . . S S T
  U . . W . . . . T
 . U W W
  . W W")
(defmethod level-source 15 [n]
". . A . B B . . O O . . Q
  A A B B . B . N . O P Q Q
 C C . F F . G N . O P R R
  C C . . G G . N O . R R
 E . . D . G . . N . R . R
  E E . . H . . . T . . T T
 . . E E H . S S . T T T
  . . I . H H . S U . V
 . . . I I . . . . U V
  . . I J . . . . X X
 . . M . J . . W X . Y
  . M M J . W W . X X Y
 K K . . J . . . Z . . Y Y")

(defmethod render-menu :level [state]
  [:div
    {:class "menubg fullscreenbg"}
    [:div
     {:class "levelcol"}
        [:div {:class "levelrow levelpad"}]
	[:div {:class "levelrow leveltitle"} [:h1 "Select Level"]]

        [:div {:class "levelrow levelitems"}
          [:div {:class "levelcol"}
          [:div {:class "levelrow"} [:img {:class "beeimg" :src "bee_worker.png"}]]
	  [:div {:class "levelrow leveltitle"} [:h2 "Worker"]]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl1.svg"
                    :onClick #(swap! game-state start-level 1)}]
             [:img {:class "lvlicon button" :src "ico_lvl2.svg"
                    :onClick #(swap! game-state start-level 2)}]
          ]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl3.svg"
                    :onClick #(swap! game-state start-level 3)}]
          ]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl4.svg"
                    :onClick #(swap! game-state start-level 4)}]
             [:img {:class "lvlicon button" :src "ico_lvl5.svg"
                    :onClick #(swap! game-state start-level 5)}]
          ]]

          [:div {:class "levelcol"}
          [:div {:class "levelrow"} [:img {:class "beeimg" :src "bee_drone.png"}]]
	  [:div {:class "levelrow leveltitle"} [:h2 "Drone"]]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl1.svg"
                    :onClick #(swap! game-state start-level 6)}]
             [:img {:class "lvlicon button" :src "ico_lvl2.svg"
                    :onClick #(swap! game-state start-level 7)}]
          ]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl3.svg"
                    :onClick #(swap! game-state start-level 8)}]
          ]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl4.svg"
                    :onClick #(swap! game-state start-level 9)}]
             [:img {:class "lvlicon button" :src "ico_lvl5.svg"
                    :onClick #(swap! game-state start-level 10)}]
          ]]

          [:div {:class "levelcol"}
          [:div {:class "levelrow"} [:img {:class "beeimg" :src "bee_queen.png"}]]
	  [:div {:class "levelrow leveltitle"} [:h2 "Queen"]]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl1.svg"
                    :onClick #(swap! game-state start-level 11)}]
             [:img {:class "lvlicon button" :src "ico_lvl2.svg"
                    :onClick #(swap! game-state start-level 12)}]
          ]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl3.svg"
                    :onClick #(swap! game-state start-level 13)}]
          ]
          [:div {:class "levelrow levelscontainer"}
             [:img {:class "lvlicon button" :src "ico_lvl4.svg"
                    :onClick #(swap! game-state start-level 14)}]
             [:img {:class "lvlicon button" :src "ico_lvl5.svg"
                    :onClick #(swap! game-state start-level 15)}]
          ]]]
        [:div {:class "levelrow levelpad"}]
        ]])

(defn next-level [state]
  (start-level state (inc (:level state))))

(defmethod render-menu :win [state]
  [:div.modal
   [:div {:class "modal-content"}
     "YOU WIN!"
     [:div {:class "modal-body aligncenter"}
       [:h1 "Un-Bee-lievable!"]
       [:p]
       [:img {:class "winmodalbutton button" :src "btn_menulg.svg" :onClick #(swap! game-state go-menu :level)}]
       [:img {:class "winmodalbutton button" :src "btn_next.svg" :onClick #(swap! game-state next-level)}]
     ]]])

(defn render-header [state]
  [:div.gameplayheadercontainer
      [:div {:class "gameplayheadercolumn alignleft"}
              [:img {:class "interfaceicon buttonblack" :src "btn_back.svg" :onClick #(swap! game-state go-menu :level)}]
              [:img {:class "interfaceicon buttonblack" :src "btn_reset.svg" :onClick #(swap! game-state reset-game)}]
              [:img {:class "interfaceicon buttonblack" :src "btn_shuffle.svg" :onClick #(swap! game-state shuffle-parts)}]
              ]
      [:div {:class "gameplayheadercolumn aligncenter paintitblack"} [:h1 (:title state)]]
      [:div {:class "gameplayheadercolumn aligncenter paintitblack"} [:h3 (str "Time: " (format-time (:time state)))]]
  ])

(defn render-parts [state]
  (doall (map (fn [[k v]] (render-part v k)) (:parts state))))

(defn render-game [state]
  (sab/html
    [:div.root
     (render-menu state)
     (if (:show state)
        [:div {:class "gameplaybg fullscreenbg"}
          [:div.gameplaycolumn
           (render-header state)
           [:div.gamearea
             [:div {:style {:position "absolute" :left (get-in state [:grid-pos :x]) :top (get-in state [:grid-pos :y])
                            :transform-origin "top left" :transform (str "scale(" (:scale state ) ")")}}
               (render-grid state)
               (render-parts state)]]]])
     [:div.overlay
      [:div {:class "gameplaybg fullscreenbg"}
       [:div.gameplaycolumn
        (render-header state)
        [:div.gamearea {:id "gamearea"}
         [:div.grid {:id "grid"}]
         [:div.parts {:id "parts"}]]
        ]]]
     ]))

(defn renderer [state]
  (.render js/ReactDOM (render-game state) (. js/document (getElementById "app"))))

(add-watch game-state
  :renderer (fn [_ _ _ n]
        (renderer n)))

(reset! game-state @game-state)
