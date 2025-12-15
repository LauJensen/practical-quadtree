(ns ftree.utils)

(defn resize-canvas!
  " Ensures each pixel on the canvas corresponds to
    a single on-screen pixel "
  [ canvas ]
  (let [rect (.getBoundingClientRect canvas)
        dpr  (or js/window.devicePixelRatio 1)
        w    (* (.-width rect)  dpr)
        h    (* (.-height rect) dpr)]
    (when (or (not= (.-width canvas) w)
              (not= (.-height canvas) h))
      (set! (.-width canvas) w)
      (set! (.-height canvas) h))))

(defn draw
  " Draws a single rectangle from tree/:bounds.

    clear? optionally wipes the canvas before drawcall "
  [ clear? canvas ctx tree color ]
  (let [[x1 y1 x2 y2] (:bounds tree)
        w      (.-width canvas)
        h      (.-height canvas)
        rw     (- x2 x1)
        rh     (- y2 y1)]
    (when clear?
       (.clearRect ctx 0 0 w h))
    (set! (.-lineWidth ctx) 1)
    (set! (.-strokeStyle ctx) color)
    (.strokeRect ctx x1 y1 rw rh)))

(defn hash-str
  " Standard 32bit hash: [..].map((% << 5) - h + ch(idx) "
  [ s ]
  (reduce #(-> (bit-shift-left %1 5)
               (- %1)
               (+ (.charCodeAt %2 0))
               (bit-or 0))
          0 s))

(defn get-tree-color
  [ {c :center} ]
  (let [hash (bit-and (hash-str (str c)) 0xffffff)
        hex  (.toString hash 16)]
    (str "#" (apply str (repeat (- 6 (count hex)) "0")) hex)))

(defn clamp
  [ v low high ]
  (-> v (max low) (min high)))

(defn rand-float
  [ max-val ]
  (/ (rand-int (* 100 max-val)) 100))
