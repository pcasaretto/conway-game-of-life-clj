(ns conway-game-of-life-clj.life
  (:require [clojure.math.combinatorics :as combo]))

(defn alive? [alive neighbor-count]
  (if alive
    (< 1 neighbor-count 4)
    (= neighbor-count 3)))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (zero? dx) (zero? dy)))]
    [(+ x dx) (+ y dy)]))

(defn neighbor-counts [state]
  (reduce
    (fn [counts coord]
      (reduce
        (fn [counts neighbor-coord]
          (update counts neighbor-coord (fnil inc 0)))
        counts
        (neighbors coord)))
    {}
    state))

(defn next-state [state]
  (let [neighbor-counts (neighbor-counts state)
        alive-map (reduce (fn [acc coord] (assoc acc coord true)) {} state)]
       (map first (filter (fn [[coord cell-neighbor-count]] (alive? (get alive-map coord) cell-neighbor-count)) neighbor-counts))))
