(ns assignment4.insertTest
  (:require [clojure.test :refer :all]
            [assignment4.core :refer :all]
            [clojure.zip :as zip]))

(deftest inserttest
  (is (= (insert [nil nil nil] "hi") ["hi" nil nil]))
  (is (= (insert ["hi" nil nil] "no") ["hi" ["no" nil nil] nil]))
  (is (= (insert [1 [2 [6 nil nil] nil] [3 nil nil]] 2) [1 [2 [6 nil nil] nil] [2 [3 nil nil] nil]]))
  (is (= (-> [nil nil nil] (insert 1) (insert 2) (insert 0) (insert 7)) [0 [2 [7 nil nil] nil] [1 nil nil]]))
  (is (= (-> [nil nil nil] (insert "hi") (insert "seeing") (insert "hello") (insert "saw") (insert "seeing")))))
