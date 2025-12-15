(ns ftree.ring)

(defn make-ring
  " Simple ring buffer for storing fps "
  [ n ]
  {:buffer (js/Float32Array. n)
   :idx    -1
   :len    n})

(defn push
  " Insert item and move index "
  [ {:keys [buffer idx len] :as r} x ]
  (let [i (mod (inc idx) len)]
    (aset buffer i x)
    (assoc r :idx i)))

(defn average
  [{:keys [buffer len]}]
  (if (zero? len)
    0
    (/ (reduce + 0 buffer)
       len)))
