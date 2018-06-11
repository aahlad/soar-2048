(ns soar-2048.board
  (:require [soar-2048.lib :as lib :refer [if-return-else]]
            [clojure.pprint :as pprint]))

(def row-size 4)
(def num-cells (* row-size row-size))
(def empty-board (vec (repeat num-cells nil)))

(def nil-indexes-xf
  (comp (map-indexed #(when-not %2 %1))
        (filter identity)))

(defn random-empty-cell [board]
  (rand-nth (into [] nil-indexes-xf board)))

(defn add-random-element [board]
  (assoc board (random-empty-cell board)
         (if (< (rand) 0.9) 2 4)))

(defn transpose [grid]
  (apply mapv vector grid))

(defn- reverse-rows [grid]
  (mapv (comp vec rseq) grid))

(defn- rotate [grid direction]
  (case direction
    :left (transpose (reverse-rows grid))
    :right (reverse-rows (transpose grid))))

(defn- nil-pad [x coll]
  (->> (repeat nil)
       (concat coll)
       (take x)))

(defn- slide-row-left [row]
  (loop [row (filter identity row) result []]
    (if-let [[x y] (seq row)]
      (if (= x y)
        (recur (drop 2 row) (conj result (+ x y)))
        (recur (drop 1 row) (conj result x)))
      (vec (nil-pad row-size result)))))

(defn- slide-left [grid]
  (mapv slide-row-left grid))

(def gridify-xf
  (comp (partition-all row-size)
        (map vec)))

(defn gridify [board]
  (into [] gridify-xf board))

(defn flatten-1 [grid]
  (into [] (mapcat identity) grid))

(defmacro grid-apply
  [item & fns]
  `(-> ~item gridify ~@fns flatten-1))

(defn slide [board direction]
  (case direction
    :left (grid-apply board slide-left)
    :right (grid-apply board reverse-rows slide-left reverse-rows)
    :up (grid-apply board (rotate :left) slide-left (rotate :right))
    :down (grid-apply board (rotate :right) slide-left (rotate :left))))

(defn string [board]
  (pprint/print-table (range row-size) (gridify board)))


(defn move-available? [board]
  (apply not= (map (partial slide board)
                   [:left :right :up :down])))

(defn remove-first [val coll]
  (loop [[x & rest] coll
         result []]
    (if (= x val)
      (concat result rest)
      (recur rest (conj result x)))))

(defn score [new old]
  ;; remove nils from both
  ;; remove items from new which are in old
  ;; return sum of items left in new
  (loop [[n & news] (filter identity new)
         old (filter identity old)
         result []]
    (if n
      (if (some #{n} old)
        (recur news (remove-first n old) result)
        (recur news old (conj result n)))
      (apply + result))))

(defn move [board direction]
  (when (move-available? board)
    (let [new-board (slide board direction)]
      (if (= new-board board)
        board
        (add-random-element new-board)))))

(defn new-game []
  (-> empty-board
      add-random-element
      add-random-element))

(defn to-string [x] (if x (.toString x) ""))

(defn to-int [x] (if-return-else x 0))

(defn int-board [board]
  (mapv to-int board))

