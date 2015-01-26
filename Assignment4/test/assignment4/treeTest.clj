(ns assignment4.treeTest
  (:require [clojure.test :refer :all]
            [assignment4.core :refer :all]
            [clojure.zip :as zip]))

(deftest leftChildTest
  (is (= (left_child [nil nil nil]) nil))
  (is (= (left_child [1 [2 nil nil] nil]) [2 nil nil]))
  (is (= (left_child [1 [3 [4 nil nil] nil] [4 nil nil]]) [3 [4 nil nil] nil])))

(deftest rightChildTest
  (is (= (right_child [nil nil nil]) nil))
  (is (= (right_child [1 [2 nil nil] [3 nil nil]]) [3 nil nil]))
  (is (= (right_child [1 [3 [4 nil nil] nil] [4 nil nil]]) [4 nil nil])))

(deftest rootTest
  (is (= (root_value [nil nil nil]) nil))
  (is (= (root_value [1 [2 nil nil] nil]) 1))
  (is (= (root_value [1 [3 [4 nil nil] nil] [4 nil nil]]) 1)))

(deftest emptyTest
  (is (= (tree-empty? [nil nil nil]) true))
  (is (= (tree-empty? [1 nil nil]) false))
  (is (= (tree-empty? [1 [2 [4 nil nil] nil] [3 nil nil]]) false)))
