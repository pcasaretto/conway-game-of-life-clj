(ns conway-game-of-life-clj.rle)


(defn consume-until-char
  [char input]
  (let [[first & rest] input]
    (if (or (= first char) (empty? rest))
      rest
      (recur char rest))))

(defn capture-word
  ([input] (capture-word input ""))
  ([input output]
   (let [[first & rest] input]
     (cond
       (nil? first) [output input]
       (Character/isWhitespace first) [output input]
       (= first \,) [output input]
       :else (recur rest (str output first))))))

(defn consume-whitespace [input]
  (let [[first & rest] input]
    (if (or (empty? rest) (not (Character/isWhitespace first))) input (recur rest))))

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
          (= type \o) (parse-body rest (update output :pattern #(concat % (generate-points count x y))) (+ x count) y)
          :else (throw (Exception. (str "unexpected character: " type))))))

(defn parse-body
  ([input output] (parse-body input output 0 0))
  ([input output x y]
   (let [[first & rest] input]
     (cond
       (nil? first) (throw (Exception. "unexpected end of input"))
       (Character/isDigit first) (parse-repeat input output x y)
       (= first \b) (parse-body rest output (inc x) y) ; ignore dead cells
       (= first \o) (parse-body rest (update output :pattern #(conj % [x y])) (inc x) y) ; live cell
       (= first \$) (parse-body rest output 0 (inc y)) ; newline
       (Character/isWhitespace first) (parse-body rest output x y)
       (= first \!) output
       :else (parse-body rest output)))))

(declare parse-meta)

(defn parse-header
  [input output]
  (let [[first & rest] input]
    (cond
      (= first \x) (parse-meta input output)
      (= first \y) (parse-meta input output)
      (= first \r) (parse-meta input output)
      (= \newline first) (parse-body rest output)
      (= \, first) (parse-header rest output)
      (Character/isWhitespace first) (parse-header rest output)
      :else (throw (Exception. (str "unexpected character: " first))))))

(defn parse-meta [input output]
  (let [[name input] (capture-word input)
        input (consume-whitespace input)
        input (drop 1 input) ; skip =
        input (consume-whitespace input)
        [value input] (capture-word input)]
    (parse-header input (assoc-in output [:meta (keyword name)] value))))


(defn ignore-comments
  [rle]
  (let [[first & rest] rle]
    (if (= first \#)
      (ignore-comments (consume-until-char \newline rest))
      rle)))

(defn parse [rle]
  (parse-header (ignore-comments rle) {:pattern []}))
