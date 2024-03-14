(ns conway-game-of-life-clj.life
  (:require [clojure.math.combinatorics :as combo]))

(defn alive? [alive neighbor-count]
  (or (and alive (<= 2 neighbor-count 3))
      (and (not alive) (= 3 neighbor-count))))

(defn neighbors [[x y]]
  (->>
    (combo/cartesian-product
     (range (dec x) (+ x 2))
     (range (dec y) (+ y 2)))
    (filter #(not (= % [x y])))
    (map vec)))

(defn neighbor-counts [state]
  (reduce (fn [acc [x y]]
            (reduce
              (fn [acc [x y]] (update acc [x y] #(if (nil? %) 1 (inc %))))
              acc
              (neighbors [x y])))
          {}
          state))

(defn next-state [state]
  (let [neighbor-counts (neighbor-counts state)
        alive-map (reduce (fn [acc [x y]] (assoc acc [x y] true)) {} state)]
       (map first (filter (fn [[coord cell-neighbor-count]] (alive? (get alive-map coord) cell-neighbor-count)) neighbor-counts))))
