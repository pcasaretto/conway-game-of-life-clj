(ns conway-game-of-life-clj.rle
  (:require [clojure.string :as str]))

(defn parse [rle]
  (let [lines (str/split-lines rle)
        content-lines (filter #(and (not (str/starts-with? % "#"))
                                    (not (str/starts-with? % "x "))) lines)
        content (apply str (interpose "" content-lines))
        parse-content (fn [content]
                        (loop [chars content
                               x 0
                               y 0
                               acc []]
                          (let [[c & rest] chars]
                            (cond
                              (nil? c) acc
                              (Character/isDigit c)
                              (let [[full-match num tag] (re-find #"(\d+)([bo])" (apply str chars))]
                                (let [num (Integer/parseInt num)
                                      size-of-match (count full-match)]
                                  (case tag
                                    "b" (recur (drop size-of-match chars) (+ x num) y acc)
                                    "o" (recur (drop size-of-match chars) (+ x num) y (into acc (map #(vector % y) (range x (+ x num)))))
                                    :else (throw (Exception. "unexpected tag")))))
                              (= c \b) (recur rest (inc x) y acc)
                              (= c \o) (recur rest (inc x) y (conj acc [x y]))
                              (= c \$) (recur rest 0 (inc y) acc)
                              :else (recur rest x y acc)))))]
       (parse-content (first (str/split content #"!" 2)))))
