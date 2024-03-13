(ns conway-game-of-life-clj.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(def step 20)
(def size 50)

(defn alive? [alive neighbor-count]
  (or (and alive (<= 2 neighbor-count 3))
      (and (not alive) (= 3 neighbor-count))))

(defn neighbors [[x y]]
  (->>
    (combo/cartesian-product
     (range (max 0 (dec x)) (min (inc size) (+ x 2)))
     (range (max 0 (dec y)) (min (inc size) (+ y 2))))
    (filter #(not (= % [x y])))
    (map vec)))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

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

(defn pulse [low high rate millis]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ millis 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(def inital-state [[3 2] [2 3] [2 4] [2 5] [2 6] [3 7]
                   [4 8] [5 8]
                   [5 6] [6 7] [7 8]
                   [7 6] [7 5] [7 4] [7 3] [6 2]
                   [5 1] [4 1]
                   [25 25] [25 26] [25 27]])



(defn create-color [millis]
  [(pulse 0 255 3.0 millis)
   (pulse 0 255 5.0 millis)
   (pulse 0 255 7.0 millis)])

(defn setup []
  (q/frame-rate 1)
  (q/background 255)
  inital-state)

(def update-state next-state)

(defn draw-state [state]
  (let [state (zipmap state (map create-color (iterate #(+ % 200) 10000)))]
    (q/no-stroke)
    (q/background 255)
    (q/no-stroke)
    (let [w (q/width)
            h (q/height)
            hw (/ w 2)
            hh (/ h 2)]
        (doseq [[ind col] state]
          (let [x (* step (first ind))
                  y (* step (second ind))
                  col-mod (-> (+ x y (q/millis))
                              (* 0.01)
                              (q/sin)
                              (* 5))]
              (apply q/fill (map + col (repeat 3 col-mod)))
              (q/rect x y step step))))))

(q/defsketch dry-paint
   :host "host"
   :size [(* step size) (* step size)]
   :setup setup
   :update update-state
   :draw draw-state
   :middleware [m/fun-mode])

(defn -main [& args])
