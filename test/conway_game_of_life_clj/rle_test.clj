(ns conway-game-of-life-clj.rle-test
  (:require [conway-game-of-life-clj.rle :as sut]
            [clojure.test :as t]))

(t/deftest parse
  (t/testing "glider"
    (let [input "x = 3, y = 3, rule = B3/S23\nbo$2bo$3o!"]
      (t/is (= [[1 0] [2 1] [0 2] [1 2] [2 2]]
               (:pattern (sut/parse input))))
      (t/is (= "3" (get-in (sut/parse input) [:meta :x])))
      (t/is (= "3" (get-in (sut/parse input) [:meta :y])))))
  (t/testing "glider with comments"
    (let [input "#C This is a glider\nx = 3, y = 3, rule = B3/S23\nbo$2bo$3o!"]
      (t/is (= [[1 0] [2 1] [0 2] [1 2] [2 2]]
               (:pattern (sut/parse input))))
      (t/is (= "3" (get-in (sut/parse input) [:meta :x])))
      (t/is (= "3" (get-in (sut/parse input) [:meta :y])))))
  (t/testing "glider with an offset"
    (let [input "x = 21, y = 3, rule = B3/S23\nbo$20bo$3o!"]
      (t/is (= [[1 0] [20 1] [0 2] [1 2] [2 2]]
               (:pattern (sut/parse input))))
      (t/is (= "21" (get-in (sut/parse input) [:meta :x])))
      (t/is (= "3" (get-in (sut/parse input) [:meta :y]))))))
