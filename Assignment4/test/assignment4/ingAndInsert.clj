(ns assignment4.ingAndInsert
  (:require [clojure.test :refer :all]
            [assignment4.core :refer :all]
            [clojure.zip :as zip]))


(deftest ingandinsert
  (is (= (-> [nil nil nil] (printing)) []))
  (is (= (-> ["hi" nil nil] (insert "nothing") (printing)) ["nothing"]))
  (is (= (-> [nil nil nil] (insert "do") (insert "aa") (insert "doing") (insert "seing") (insert "has") (insert "seeing") (printing)) ["seing" "seeing" "doing"])))
