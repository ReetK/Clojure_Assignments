(ns assignment4.core
  (:require [clojure.zip :as zip]))




"Root Value, Left child and Right child of tree"

(defn left_child
  [tree]
  "This function returns the left child of the tree"
  (-> tree zip/vector-zip zip/down zip/right zip/node))

(defn right_child
  [tree]
  "This function returns the right child of the tree"
  (-> tree zip/vector-zip zip/down zip/rightmost zip/node))

(defn root_value
  [tree]
  "This function returns the root of the tree"
  (if (zip/node tree)
    (-> tree zip/vector-zip zip/down zip/node)
    nil))




"Tree empty check"

(defn tree-empty?
 [zipper]
  "This function checks if the tree is empty"
 (not (zip/node zipper)))




"Depth of a tree"

(defn cal_depth
  [tree count]
  "This function calculates the numeber of nodes under the root of root"
  (if (tree-empty? tree)
      :true
      (if (tree-empty? (-> tree zip/vector-zip zip/down zip/right))
        count
        (if (tree-empty? (-> tree zip/vector-zip zip/down zip/rightmost))
          (inc count)
          (cal_depth (left_child tree) (inc (cal_depth (right_child tree) (inc count))))))))



"------------------------------------------------------------------------------------------------------------"
"Insertion inside a tree"

(defn do_insert
  [tree value]
  "This function does the insertion of number values in the tree"
  "This function calls itself if the left and/or the right child of the tree is further a tree"
  (cond
   (= (root_value tree) value)
     (if (= (left_child tree) nil)
       (assoc-in tree [1] [value nil nil])
       (if (= (right_child tree) nil)
         (assoc-in tree [2] [value nil nil])
;;         (insert1 tree value)))
         (if (= (cal_depth (left_child tree) 0) (cal_depth (right_child tree) 0))
           (cond
            (= (root_value (left_child tree)) value)
              (assoc-in tree [1] (do_insert (left_child tree) value))
            (> (root_value (left_child tree)) value)
              (assoc-in tree [1] (do_insert (assoc-in (left_child tree) [0] value) (root_value (left_child tree))))
            (< (root_value (left_child tree)) value)
              (assoc-in tree [1] (do_insert (left_child tree) value)))
           (cond
            (= (root_value (right_child tree)) value)
              (assoc-in tree [2] (do_insert (right_child tree) value))
            (> (root_value (right_child tree)) value)
              (assoc-in tree [2] (do_insert (assoc-in (right_child tree) [0] value) (root_value (right_child tree))))
            (< (root_value (right_child tree)) value)
              (assoc-in tree [2] (do_insert (right_child tree) value))))))
   (> (root_value tree) value)
     (if (= (left_child tree) nil)
              (assoc-in (assoc-in tree [0] value) [1] [(root_value tree) nil nil])
              (if (= (right_child tree) nil)
                (assoc-in (assoc-in tree [0] value) [2] [(root_value tree) nil nil])
               (if (= (cal_depth (left_child tree) 0) (cal_depth (right_child tree) 0))
                 (cond
                   (= (root_value (left_child tree)) value)
                     (assoc-in (assoc-in tree [0] value) [1] (do_insert (left_child tree) (root_value tree)))
                   (> (root_value (left_child tree)) value)
                     (assoc-in (assoc-in tree [0] value) [1] (do_insert (left_child tree) (root_value tree)))
                   (< (root_value (left_child tree)) value)
                     (assoc-in tree [1] (do_insert (left_child tree) value)))
                  (cond
                   (= (root_value (right_child tree)) value)
                     (assoc-in (assoc-in tree [0] value) [2] (do_insert (right_child tree) (root_value tree)))
                   (> (root_value (right_child tree)) value)
                     (assoc-in (assoc-in tree [0] value) [2] (do_insert (right_child tree) (root_value tree)))
                   (< (root_value (right_child tree)) value)
                     (assoc-in tree [2] (do_insert (right_child tree) value))))))
   (< (root_value tree) value)
     (if (= (left_child tree) nil)
       (assoc-in tree [1] [value nil nil])
       (if (= (right_child tree) nil)
         (assoc-in tree [2] [value nil nil])
         (if (= (cal_depth (left_child tree) 0) (cal_depth (right_child tree) 0))
           (cond
            (= (root_value (left_child tree)) value)
              (assoc-in tree [1] (do_insert (left_child tree) value))
            (> (root_value (left_child tree)) value)
              (assoc-in tree [1] (do_insert (assoc-in (left_child tree) [0] value) (root_value (left_child tree))))
            (< (root_value (left_child tree)) value)
              (assoc-in tree [1] (do_insert (left_child tree) value)))
           (cond
            (= (root_value (right_child tree)) value)
              (assoc-in tree [2] (do_insert (right_child tree) value))
            (> (root_value (right_child tree)) value)
              (assoc-in tree [2] (do_insert (assoc-in (right_child tree) [0] value) (root_value (right_child tree))))
            (< (root_value (right_child tree)) value)
              (assoc-in tree [2] (do_insert (right_child tree) value))))))))


"----------------"


(defn do_Sinsert
  [tree value]
  "This function does the insertion of String values in the tree"
  "The function calls itself if the left and/or right child of tree are tree themselves"
  (cond
   (= (root_value tree) value)
     (if (= (left_child tree) nil)
       (assoc-in tree [1] [value nil nil])
       (if (= (right_child tree) nil)
         (assoc-in tree [2] [value nil nil])
;;         (Sinsert1 tree value)))
         (if (= (cal_depth (left_child tree) 0) (cal_depth (right_child tree) 0))
           (cond
            (= (root_value (left_child tree)) value)
              (assoc-in tree [1] (do_Sinsert (left_child tree) value))
            (> (compare (root_value (left_child tree)) value) 0)
              (assoc-in tree [1] (do_Sinsert (assoc-in (left_child tree) [0] value) (root_value (left_child tree))))
            (< (compare (root_value (left_child tree)) value) 0)
              (assoc-in tree [1] (do_Sinsert (left_child tree) value)))
           (cond
            (= (root_value (right_child tree)) value)
              (assoc-in tree [2] (do_Sinsert (right_child tree) value))
            (> (compare (root_value (right_child tree)) value) 0)
              (assoc-in tree [2] (do_Sinsert (assoc-in (right_child tree) [0] value) (root_value (right_child tree))))
            (< (compare (root_value (right_child tree)) value) 0)
              (assoc-in tree [2] (do_Sinsert (right_child tree) value))))))
   (> (compare (root_value tree) value) 0)
     (if (= (left_child tree) nil)
              (assoc-in (assoc-in tree [0] value) [1] [(root_value tree) nil nil])
              (if (= (right_child tree) nil)
                (assoc-in (assoc-in tree [0] value) [2] [(root_value tree) nil nil])
;;                (Sinsert2 tree value)))
                (if (= (cal_depth (left_child tree) 0) (cal_depth (right_child tree) 0))
                  (cond
                   (= (root_value (left_child tree)) value)
                     (assoc-in (assoc-in tree [0] value) [1] (do_Sinsert (left_child tree) (root_value tree)))
                   (> (compare (root_value (left_child tree)) value) 0)
                     (assoc-in (assoc-in tree [0] value) [1] (do_Sinsert (left_child tree) (root_value tree)))
                   (< (compare (root_value (left_child tree)) value) 0)
                     (assoc-in tree [1] (do_Sinsert (left_child tree) value)))
                  (cond
                   (= (compare (root_value (right_child tree)) value) 0)
                     (assoc-in (assoc-in tree [0] value) [2] (do_Sinsert (right_child tree) (root_value tree)))
                   (> (compare (root_value (right_child tree)) value) 0)
                     (assoc-in (assoc-in tree [0] value) [2] (do_Sinsert (right_child tree) (root_value tree)))
                   (< (compare (root_value (right_child tree)) value) 0)
                     (assoc-in tree [2] (do_Sinsert (right_child tree) value))))))
   (< (compare (root_value tree) value) 0)
     (if (= (left_child tree) nil)
       (assoc-in tree [1] [value nil nil])
       (if (= (right_child tree) nil)
         (assoc-in tree [2] [value nil nil])
;;         (Sinsert3 tree value)))))
         (if (= (cal_depth (left_child tree) 0) (cal_depth (right_child tree) 0))
           (cond
            (= (root_value (left_child tree)) value)
              (assoc-in tree [1] (do_Sinsert (left_child tree) value))
            (> (compare (root_value (left_child tree)) value) 0)
              (assoc-in tree [1] (do_Sinsert (assoc-in (left_child tree) [0] value) (root_value (left_child tree))))
            (< (compare (root_value (left_child tree)) value) 0)
              (assoc-in tree [1] (do_Sinsert (left_child tree) value)))
           (cond
            (= (compare (root_value (right_child tree)) value) 0)
              (assoc-in tree [2] (do_Sinsert (right_child tree) value))
            (> (compare (root_value (right_child tree)) value) 0)
              (assoc-in tree [2] (do_Sinsert (assoc-in (right_child tree) [0] value) (root_value (right_child tree))))
            (< (compare (root_value (right_child tree)) value) 0)
              (assoc-in tree [2] (do_Sinsert (right_child tree) value))))))))


"----------------"


(defn insert
  [tree value]
  "This function calls the string or number insertion function depending on the class of input value"
  (if (tree-empty? tree)
    (assoc-in tree [0] value)
    (if (= (type value) java.lang.Long)
      (do_insert tree value)
      (do_Sinsert tree value))))



"------------------------------------------------------------------------------------------------------------"
"To check for 'ing' strings in tree"

(defn ing_string?
  [string]
  "This function checks whether the string ends with 'ing' or not"
  (if (>= (count string) 3)
    (= "ing" (subs string (- (count string) 3) (count string)))))



(defn prints
  [tree counts final]
  "This function adds the string to the output is it ends with 'ing'"
  "This functions calls itself if the left and/or right child of the tree are tree themselves"
   (if (ing_string? (root_value tree))
    (if (= (left_child tree) nil)
      (conj final (root_value tree))
      (if (= (right_child tree) nil)
        (prints (left_child tree) (inc counts) (conj final (root_value tree)))
        (prints (right_child tree) (inc counts) (prints (left_child tree) (inc counts) (conj final (root_value tree))))))
    (if (= (left_child tree) nil)
      final
      (if (= (right_child tree) nil)
        (prints (left_child tree) (inc counts) final)
        (prints (right_child tree) (inc counts) (prints (left_child tree) (inc counts) final))))))


(defn printing
  [tree]
  "This function is used when user wants to print strings in tree ending with 'ing'"
  (prints tree 0 []))

"------------------------------------------------------------------------------------------------------------"
