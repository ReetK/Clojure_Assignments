(ns assignment4.ingTest
  (:require [clojure.test :refer :all]
            [assignment4.core :refer :all]
            [clojure.zip :as zip]))

(deftest ingtest
  (is (= (printing [nil nil nil]) []))
  (is (= (printing ["see" nil nil]) []))
  (is (= (printing ["seeing" nil nil]) ["seeing"]))
  (is (= (printing ["see" ["doing" nil nil] nil]) ["doing"]))
  (is (= (printing ["seeing" ["do" nil nil] ["showing" nil nil]]) ["seeing" "showing"])))
