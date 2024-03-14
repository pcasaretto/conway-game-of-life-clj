(ns conway-game-of-life-clj.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.math.combinatorics :as combo]
            [clojure.tools.cli :refer [parse-opts]]
            [conway-game-of-life-clj.life :as life])
  (:gen-class))

(def step (atom 50))
(def size (atom 50))
(def fps (atom 1))

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
  (q/frame-rate @fps)
  (q/background 255)
  inital-state)

(def update-state life/next-state)

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
          (let [x (* @step (first ind))
                  y (* @step (second ind))
                  col-mod (-> (+ x y (q/millis))
                              (* 0.01)
                              (q/sin)
                              (* 5))]
              (apply q/fill (map + col (repeat 3 col-mod)))
              (q/rect x y @step @step))))))

(def cli-options
  [[nil "--size SIZE" "The size of the board"
    :default 50
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   [nil "--step STEP" "The step size of the frame"
    :default 50
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   [nil "--fps STEP" "How many frames to render per second"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]])

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn -main [& args]
  (let [{options :options} (parse-opts args cli-options)
        {sizev :size stepv :step fpsv :fps} options]
       (reset! size sizev)
       (reset! step stepv)
       (reset! fps fpsv)
       (dbg stepv)
       (q/sketch
          :host "host"
          :size [(* @step @size) (* @step @size)]
          :setup setup
          :update update-state
          :draw draw-state
          :middleware [m/fun-mode])))
