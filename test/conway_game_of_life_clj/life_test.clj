(ns conway-game-of-life-clj.life-test
  (:require [clojure.test :refer :all]
            [conway-game-of-life-clj.life :as sut]))

(deftest neighbors-test
  (testing "neighbors of a cell"
    (is (= '([0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2])
           (sut/neighbors [1 1])))
    (is (= '([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1])
           (sut/neighbors [0 0])))

  (testing "no neighbors for itself")))

(deftest neighbor-counts-test
  (testing "neighbor counts for empty state"
    (is (= {} (sut/neighbor-counts #{}))))

  (testing "neighbor counts for single cell"
    (is (= {[-1 -1] 1, [-1 0] 1, [-1 1] 1, [0 -1] 1, [0 1] 1, [1 -1] 1, [1 0] 1, [1 1] 1} (sut/neighbor-counts #{[0 0]}))))

  (testing "neighbor counts for multiple cells"
    (is (= {[2 2] 1, [0 0] 3, [2 -1] 1, [1 0] 3, [-1 0] 2, [1 1] 3, [-1 2] 1, [-1 -1] 1, [1 -1] 2, [0 2] 2, [-1 1] 2, [2 0] 2, [2 1] 2, [0 -1] 2, [1 2] 2, [0 1] 3}
           (sut/neighbor-counts #{[0 0] [0 1] [1 0] [1 1]})))))

(deftest alive?-test
  (testing "alive? function"
    (is (= true (sut/alive? true 2)))
    (is (= true (sut/alive? true 3)))
    (is (= false (sut/alive? true 1)))
    (is (= false (sut/alive? true 4)))
    (is (= true (sut/alive? false 3)))
    (is (= false (sut/alive? false 2)))
    (is (= false (sut/alive? false 4)))))