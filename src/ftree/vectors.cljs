(ns ftree.vectors)

(defn div
  [n [x y]]
  [(/ x n) (/ y n)])

(defn sub
  [v2 v1]
  (mapv - v1 v2))

(defn add
  ([[x1 y1] [i1 j1]] [(+ x1 i1) (+ y1 j1)])
  ([v1 v2 & vecs]
   (reduce add (add v1 v2) vecs)))

(defn mul
  [n v]
  (mapv * [n n] v))

(defn magnitude
  ([ [ x y ] ]
   (js/Math.sqrt
    (+ (js/Math.pow x 2) (js/Math.pow y 2) )))
  ([ new-magnitude [ x y :as v ] ]
   (let [m (magnitude v)]
     (if (<= m 1.0e-12)
       [0 0]
       [(* x (/ new-magnitude m))
        (* y (/ new-magnitude m))]))))

(defn limit-mag
  " An attempt at reaching brutal speeds "
  [max-mag [x y]]
  (let [m2     (+ (* x x) (* y y))
        max2  (* max-mag max-mag)]
    (if (<= m2 max2)
      [x y]
      (let [s (/ max-mag (js/Math.sqrt m2))]
        [(* x s) (* y s)]))))

(defn dist
  " Distance between 2 points "
  [[x1 y1] [x2 y2]]
  (js/Math.sqrt
   (+ (js/Math.pow (- x2 x1) 2)
      (js/Math.pow (- y2 y1) 2))))
