(ns ftree.core
  (:require
   [ftree.utils    :as u]
   [ftree.vectors  :as v]
   [ftree.ring     :as r]
   [ftree.quadtree :as qt]))

;; CONSTS

(def num-boids 200)

(def boids
  (atom []))

(def boid-defaults
  {:max-speed     2.0
   :max-force     0.4
   :perception   30})

(defn initialize-boids
  [ width height ]
  (->> (for [idx (range num-boids)]
         (merge boid-defaults
                {:id idx
                 :position     [(rand-int width) (rand-int height)]
                 :max-w        width
                 :max-h        height
                 :velocity     [(- (u/rand-float 3) 1.5)
                                (- (u/rand-float 3) 1.5)]
                 :acceleration [0 0]}))
       vec
       (reset! boids)))

(defn edge-wrap
  [ {:keys [position max-w max-h] :as boid} ]
  (let [[x y] position]
    (assoc boid
           :position
           (cond
             (<= x 0)     [max-w y]
             (<= y 0)     [x     max-h]
             (>= x max-w) [0     y]
             (>= y max-h) [x     0]
             :else        position))))

(defn cohesion
  " Steer towards center of friends "
  [ {:keys [position max-speed max-force velocity]} friends ]
  (if (seq friends)
    (->> (map :position friends)
         (reduce #(v/add %1 %2) [0 0])
         (v/div (count friends))
         (v/sub position)
         (v/magnitude max-speed)
         (v/sub velocity)
         (v/limit-mag max-force))
    [0 0]))

(defn alignment
  " Lets all go the same way "
  [ {:keys [ max-speed max-force velocity ]} friends ]
  (if (seq friends)
    (->> (map :velocity friends)
         (reduce #(v/add %1 %2) [0 0])
         (v/div (count friends))
         (v/magnitude max-speed)
         (v/sub velocity)
         (v/limit-mag max-force))
    [0 0]))

(defn seperation
  " We're too close, watch out! "
  [ {:keys [position max-speed max-force velocity]} friends ]
  (if (seq friends)
    (let [accel (reduce #(let [pos2 (:position %2)
                               dist (v/dist position pos2)]
                           (if (<= dist 1.0e-6)
                             %1
                             (v/add %1 (v/div (* dist dist)
                                               (mapv - position pos2)))))
                        [0 0]
                        friends)]
    (->> accel
         (v/div (count friends))
         (v/magnitude max-speed)
         (v/sub velocity)
         (v/limit-mag max-force)))
    [0 0]))

(defn move-boid
  " A single tick for a single boid "
  [ tree {:keys [id
                 position
                 velocity
                 perception
                 max-speed
                 max-force]
          :as boid} ]
  (let [friends        (->> (qt/search tree perception position)
                            (filterv #(not= id (:id %))))
        acceleration   (->> (v/add
                             (alignment boid friends)
                             (cohesion boid friends)
                             (seperation boid friends))
                            (v/limit-mag max-force))
        velocity       (->> (v/add velocity acceleration)
                            (v/limit-mag max-speed))]
    (edge-wrap
     (assoc boid
            :position     (v/add position velocity)
            :velocity     velocity
            :acceleration [0 0]))))

(defn move-boids
  [ tree bs ]
  (->> (mapv #(move-boid tree %) bs)
       (reset! boids)))

(defn draw-all
  " Draws an entire tree "
  [ canvas ctx tree ]
  (u/draw (:root? tree)
          canvas ctx
          tree
          "#bbb")

  ;; Draw the boids
  (doseq [[x y] (map :position (:objects tree))]
    (.fillRect ctx x y 4 4))

  (when-let [children (:children tree)]
    (doseq [c children]
      (draw-all canvas ctx c))))

(defonce stop (atom false))

(def ring-buf (atom (r/make-ring 20))) ;; avg over 20 frames

(defn start-loop!
  [ canvas ctx w h ]
  (let [last-time (atom 0)]
    (letfn [(update [current-time]
              (try
                (when-not @stop
                  (js/requestAnimationFrame update)
                  (let [bs @boids
                        t  (reduce (fn [t boid]
                                     (qt/insert t boid))
                                   (assoc (qt/qtree [0 0 w h]) :root? true)
                                   bs)]
                    (move-boids t bs)
                    (draw-all canvas ctx t)
                    (let [dt (- current-time @last-time)
                          fps (/ 1000 dt)]
                      (:idx (swap! ring-buf r/push fps))
                      (.fillText ctx
                                 (str "FPS: " (int (r/average @ring-buf)))
                                 3 20)))
                  (reset! last-time current-time))
                (catch js/Error e
                  (js/console.log "Update error:" e (.-message e) (.-stack e))
                  (js/requestAnimationFrame update))))]
      (update 0))))

(defn ^:dev/after-load insert-and-render
  [ ]
  (.clear js/console)
  (reset! stop true)
  (let [canvas-id "ftree"
        canvas  (.getElementById js/document canvas-id)
        ctx     (.getContext canvas "2d")
        w       (.-width canvas)
        h       (.-height canvas)
        tree    (time
                 (reduce (fn [t _]
                           (qt/insert t {:position [(rand-int w) (rand-int h)]}))
                         (assoc (qt/qtree [0 0 w h]) :root? true)
                         (range num-boids)))]
    (initialize-boids w h)
    (draw-all canvas ctx tree)
    (js/setTimeout ;; Give the running loop a chance to halt
     (fn []
       (reset! stop false)
       (start-loop! canvas ctx w h))
     500)
    (println "Reloaded")))

(defn main
  [ canvas-id ]
  (u/resize-canvas!
   (.getElementById js/document canvas-id))
  (insert-and-render))

(.addEventListener js/window "load"
                   (fn [] (main "ftree")))
