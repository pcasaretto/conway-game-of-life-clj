(ns conway-game-of-life-clj.rle
  (:require [clojure.string :as str]))


(defn consume-until-char
  [char input]
  (let [[first & rest] input]
    (if (or (= first char) (empty? rest))
      rest
      (recur char rest))))

(declare parse-body)

(defn generate-points [count x y]
  (map #(vector % y) (range x (+ x count))))

(defn consume-digits
  [input]
  (loop [digits "" input input]
    (let [[first & rest] input]
      (if (Character/isDigit first)
        (recur (str digits first) rest)
        [digits input]))))

(defn parse-repeat
  [input output x y]
  (let [[count input] (consume-digits input)
        count (Integer/parseInt (str count))
        [type & rest] input]
       (cond (= type \b) (parse-body rest output (+ x count) y)
             (= type \o) (parse-body rest (concat output (generate-points count x y)) (+ x count) y)
             :else (throw (Exception. (str "unexpected character: " type))))))

(defn parse-body
  ( [input output] (parse-body input output 0 0))
  ( [input output x y]
    (let [[first & rest] input]
      (cond
        (nil? first) (throw (Exception. "unexpected end of input"))
        (Character/isDigit first) (parse-repeat input output x y)
        (= first \b) (parse-body rest output (inc x) y) ; ignore dead cells
        (= first \o) (parse-body rest (conj output [x y]) (inc x) y) ; live cell
        (= first \$) (parse-body rest output 0 (inc y)) ; newline
        (Character/isWhitespace first) (parse-body rest output x y)
        (= first \!) output
        :else (parse-body rest output)))))

(defn parse-header
  [input output]
  (let [input (consume-until-char \newline input)]
    (parse-body input output)))

(defn ignore-comments
  [rle]
  (let [[first & rest] rle]
    (if (= first \#)
      (ignore-comments (consume-until-char \newline rest))
      rle)))

(defn parse [rle]
  (parse-header (ignore-comments rle) []))
