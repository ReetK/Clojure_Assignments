"Write a non recursive function sdsu-nth which returns the nth element from a sequence. 
You are not allowed to use the functions nth, get or use the sequence as a function"

(defn sdsu-nth
  [x, n]
   (first
    (drop n x)))






"Write a recursive function r-sdsu-nth which returns the nth element from a sequence. You 
are not allowed to use the functions nth, get or use the sequence as a function."

(defn r-sdsu-nth
  [x, n]
   (if (> n 0)
     (r-sdsu-nth (next x) (- n 1))
      (first x)))






"Write a recursive function sdsu-reverse which reverses a sequence. You are not allowed to 
use reverse or rseq."

(defn reversing
  [old new]
   (if (= (count old) 0)
     new
     (reversing (rest old) (cons (first old) new))))

(defn sdsu-reverse
  [x]
   (reversing x '()))






"Write a function sdsu-dup that will duplicate each element in a sequence."

(defn duplicate
  [old new]
   (if (= (count old) 0)
     new
     (duplicate (drop-last old) (concat (take-last 1 old) (take-last 1 old) new))))

(defn sdsu-dup
  [x]
   (duplicate x '()))






"Write a function sdsu-no-dup that will remove consecutive duplicates from a sequence."

(defn no-duplicate
  [old new]
   (if (= (count old) 0)
     new
     (if (= (take-last 1 old) (take 1 new))
       (no-duplicate (drop-last old) new)
       (no-duplicate (drop-last old) (concat (take-last 1 old) new)))))

(defn sdsu-no-dup
  [x]
   (no-duplicate x '()))






"Write a function sdsu-pack that separates consecutive duplicates in a sequence into sub-lists."

(defn sdsu-pack [x]
  (loop [new '()
         old x
         sublist nil]
    (if (= (count old) 0) 
      (cons sublist new)
      (if (= (first sublist) (last old)) 
        (recur new (drop-last 1 old) (cons (last old) sublist)) 
        (if (= (count sublist) 0)
          (recur new (drop-last 1 old) (list (last old)))
          (recur (cons sublist new) (drop-last 1 old) (list (last old))))))))