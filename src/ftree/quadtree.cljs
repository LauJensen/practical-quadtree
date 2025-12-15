(ns ftree.quadtree
  (:require
   [ftree.utils  :as u]
   [clojure.walk :as w]))

;; Minimum cell size
(def MAX-OBJECTS-PER-NODE 2)
(def MIN-NODE-WIDTH      25)

(defn half
  " Halfway point between c2 and c1 "
  [c2 c1]
  (+ c1 (/ (- c2 c1) 2)))

(defn qtree
  " Instantiates a tree node "
  [[x1 y1 x2 y2]]
  {:root?   false
   :bounds  [x1 y1 x2 y2]
   :objects []
   :center  (mapv #(js/Math.floor %)
                  [(half x2 x1)
                   (half y2 y1)])
   :width   (- x2 x1)})

(defn inside?
  [ bounds [x y] ]
  (when-let [[x1 y1 x2 y2] (seq bounds)]
    (and (<= x1 x x2)
         (<= y1 y y2))))

(defn subdivide
  " Attaches 4 :children to the node, relocates objects "
  [ {:keys [bounds objects] :as node} ]
  (let [[x1 y1 x2 y2] bounds
        top-left      [x1           y1           (half x2 x1) (half y2 y1)]
        top-right     [(half x2 x1) y1           x2           (half y2 y1)]
        bottom-left   [x1           (half y2 y1) (half x2 x1) y2]
        bottom-right  [(half x2 x1) (half y2 y1) x2           y2]]
    (->> [top-left top-right bottom-left bottom-right]
         (mapv #(->> (filterv (fn [p] (inside? % (:position p))) objects)
                     (assoc (qtree %) :objects)))
         (assoc node :objects [] :children))))

#_(defn insert
  [ tree obj ]
  (w/prewalk
   (fn [n]
     (if (and (map? n)
              (nil? (:children n))
              (inside? (:bounds n) obj))
         (insert-or-divide n obj)
       n))
   tree))

(defn get-objects
  " Fetches all objects within a quad node, incl children "
  [ node ]
  (when (map? node)
    (into (get node :objects [])
          (mapcat #(get-objects %) (:children node)))))

(defn child-index
  " Returns 0:TL, 1:TR, 2:BL, 3:BR for point inside bounds."
  [[x1 y1 x2 y2] [px py]]
  (let [xm (half x2 x1)
        ym (half y2 y1)
        east?  (>= px xm)
        south? (>= py ym)]
    (cond
      (and (not east?) (not south?)) 0
      (and east?       (not south?)) 1
      (and (not east?) south?)       2
      :else                          3)))

(defn insert
  " insert or subdivide. "
  [node obj]
  (let [point (:position obj)]
    (if (nil? (:children node)) ;; leaf
      (let [objs (:objects node)]
        (if (or (< (count objs) MAX-OBJECTS-PER-NODE)
                (< (:width node) MIN-NODE-WIDTH))
          (update node :objects conj obj)
          (let [node' (subdivide node)
                idx   (child-index (:bounds node') point)]
            (update-in node' [:children idx] insert obj))))
      ;; internal node: just recurse down one child
      (let [idx (child-index (:bounds node) point)]
        (update-in node [:children idx] insert obj)))))

;; Lookups

(defn- dist2
  " Squared distance "
  [ [x1 y1] [x2 y2] ]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (+ (* dx dx) (* dy dy))))

(defn- circle-intersects-rect?
  [ [cx cy] r [x1 y1 x2 y2] ]
  (let [px (u/clamp cx x1 x2)
        py (u/clamp cy y1 y2)]
    (<= (dist2 [cx cy] [px py])
        (* r r))))

(defn search
  " Returns a list of objects within r-radius "
  [node r center]
  (let [r2 (* r r)]
    (letfn [(drill [n acc]
              (if (not (circle-intersects-rect? center r (:bounds n)))
                acc
                (if-let [chs (:children n)]
                  (reduce (fn [a c] (drill c a)) acc chs)
                  (reduce (fn [a p]
                            (if (<= (dist2 center (:position p)) r2)
                              (conj a p)
                              a))
                          acc
                          (:objects n)))))]
      (drill node []))))
