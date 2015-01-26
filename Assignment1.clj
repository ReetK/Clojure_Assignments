"We shall contemplate truth by testing reality, via equality"
(= true true)




"To understand reality, we must compare our expectations against reality."
(= 2 (+ 1 1))




"You can test equality of many things"
(= (+ 3 5) 8 (+ 2 6))




"Some things may appear different, but be the same"
(= 2 2/1 4/2)




"You cannot generally float to heavens of integers"
(= false (= 2 2.0))





"But a looser equality is also possible"
(== 3.0 3 3/1)




"When things cannot be equal, they must be different"
(not= :fill-in-the-blank 1)




"Lists can be expressed by function or a quoted form"
(= '(1 2 3 4 5) (list 1 2 3 4 5))




"They are Clojure seqs (sequences), so they allow access to the first"
(= 2 (first '(2 3 4 5)))




"As well as the rest"
(= '(3 4 5) (rest '(2 3 4 5)))




"The rest when nothing is left is empty"
(= '() (rest '(10)))




"And construction by adding an element to the front is simple"
(= '(:a :b :c :d :e) (cons :a '(:b :c :d :e)))




"Conjoining an element to a list can be done in the reverse order"
(= '(0 :a :b :c :d :e) (conj '(:a :b :c :d :e) 0))




"You can use a list like a stack to get the first element"
(= :a (peek '(:a :b :c :d :e)))




"Or the others"
(= '(:b :c :d :e) (pop '(:a :b :c :d :e)))




"But watch out if you try to pop nothing"
(= "No dice!" (try
                (pop '())
                 (catch IllegalStateException e "No dice!")))




"The rest of nothing isn't so strict"
(= '() (try
         (rest '())
          (catch IllegalStateException e "No dice!")))




"You can use vectors in clojure to create an 'Array' like structure"
(= 1 (count [42]))




"You can create a vector in several ways"
(= [] (vec nil))




"And populate it in either of these ways"
(= [] (vec '(1)))




"There is another way as well"
(= (vec '(nil)) (vector nil))




"But you can populate it with any number of elements at once"
(= [1 2] (vec '(1 2)))




"And add to it as well"
(= '(333) (conj (vec nil) 333))




"You can get the first element of a vector like so"
(= :peanut (first [:peanut :butter :and :jelly]))




"And the last in a similar fashion"
(= :jelly (last [:peanut :buutter :and :jelly]))




"Or any index if you wish"
(= :jelly (nth [:peanut :butter :and :jelly] 3))




"You can also slice a vector"
(= (vec '(:butter :and)) (subvec [:peanut :butter :and :jelly] 1 3))




"Equality with collections is in terms of values"
(= (list 1 2 3) (vector 1 2 3))




"You can create a set in two ways"
(= #{} (set '()))




"They are another important data structure in clojure"
(= 3 (count #{1 2 3}))




"Remember that a set is a 'set'"
(= #{1 2 3 4 5} (set '( 1 1 2 2 3 3 4 4 5 5)))




"You can ask clojure for the union of two sets"
(= #{1 2 3 4 5} (clojure.set/union #{1 2 3 4} #{2 3 5}))




"And also the intersection"
(= #{2 3} (clojure.set/intersection #{ 1 2 3 4} #{2 3 5}))




"But don't forget about the difference"
(= #{1 4} (clojure.set/difference #{1 2 3 4} #{2 3 5}))




"There are two ways to create maps"
(= {} (hash-map))




"Maps in clojure associate keys with values"
(= 0 (count (hash-map)))




"A value must be supplied for each key"
(= {:a 1} (hash-map :a 1))




"The size is the number of entries"
(= 2 (count {:a 1 :b 2}))




"You can look up the value for a given key"
(= 2 (get {:a 1 :b 2} :b))




"Maps can be used as lookup functions"
(= 1 ({:a 1 :b 2} :a))




"And so can keywords"
(= 1 (:a {:a 1 :b 2}))




"But map keys need not be keywords"
(= "Vancouver" ({2006 "Torino" 2010 "Vancouver" 2014 "Sochi"} 2010))




"You may not be able to find an entry for a key"
(= nil (get {:a 1 :b 2} :c))




"But you can provide your own default"
(= :key-not-found (get {:a 1 :b 2} :c :key-not-found))




"You can find out if a key is present"
(= true (contains? {:a nil :b nil} :b))




"Or if it is missing"
(= false (contains? {:a nil :b nil} :c))




"Maps are immutable, but you can create a new, 'changed' version"
(= {1 "January" 2 "February"} (assoc {1 "January"} 2 "February"))




"You can also 'remove' an entry"
(= {1 "January"} (dissoc {1 "January" 2 "February"} 2))




"Often you will need to get the keys (which will be in hash order)"
(= (list 2006 2010 2014)
    (sort (keys {2006 "Torino" 2010 "Vancouver" 2014 "Sochi"})))




"Or the values"
(= (list "Sochi" "Torino" "Vancouver")
    (sort (vals {2006 "Torino" 2010 "Vancouver" 2014 "Sochi"})))





(defn multiply-by-ten [n]
   (* 10 n))





(defn square [n]
  (* n n))




"Functions are often defined before they are used"
(= 20 (multiply-by-ten 2))




"But they can also be defined inline"
(= 40 ((fn [n] (* 20 n)) 2))




"Or using even shorter syntax"
(= 1500 (#(* 15 %) 100))




"Short anonymous functions may take multiple arguments"
(= 15 (#(+ %1 %2 %3) 4 5 6))




"One function can beget another"
(= 4/5 ((fn[]
          ((fn [a b] (/ a b))
           4 5))))




"Higher-order functions take function arguments"
(= 25 ((fn [x] (x 5)) (fn [n] (* n n))))




"But they are often better written using the names of functions"
(= 25 ((fn [x] (x 5)) square))





(defn explain-defcon-level [exercise-term]
  (case exercise-term
    :fade-out      :you-and-what-army
    :double-take   :call-me-when-its-important
    :round-house   :o-rly
    :fast-pace     :thats-pretty-bad
    :cocked-pistol :sirens
    :say-what?))




"You will face many decisions"
(= :a (if (false? (= 4 5))
        :a
        :b))




"Some of them leave you no alternative"
(= [] (if (> 4 3)
         []))




"And in such a situation you may have nothing"
(= nil (if (nil? 0)
       [:a :b :c]))




"In others your alternative may be interesting"
(= :glory (if (not (empty? ()))
            :doom
            :glory))




"You may have a multitude of possible paths"
(let [x 5]
  (= :your-road (cond (= x 1) :road-not-taken
                      (= x 2) :another-road-not-taken
                      :else :your-road)))




"Or your fate may be sealed"
(= 'doom (if-not (zero? 3)
       'doom
       'doom))




"In case of emergency, sound the alarms"
(= :sirens
   (explain-defcon-level :cocked-pistol))




"But admit it when you don't know what to do"
(= :say-what?
   (explain-defcon-level :yo-mama))




"The map function relates a sequence to another"
(= [4 8 12] (map (fn [x] (* 4 x)) [1 2 3]))




"You may create that mapping"
(= [1 4 9 16 25] (map (fn [x] (* x x)) [1 2 3 4 5]))




"Or use the names of existing functions"
(= '(false false true false false) (map nil? [:a :b nil :c :d]))




"A filter can be strong"
(= '() (filter (fn [x] false) '(:anything :goes :here)))




"Or very weak"
(= '(:anything :goes :here) (filter (fn [x] true) '(:anything :goes :here)))




"Or somewhere in between"
(= [10 20 30] (filter (fn [x] (< x 40)) [10 20 30 40 50 60 70 80]))




"Maps and filters may be combined"
(= [10 20 30] (map (fn [x] (* 10 x)) (filter (fn [x] (< x 4)) [1 2 3 4 5 6 7 8])))




"Reducing can increase the result"
(= 24 (reduce (fn [a b] (* a b)) [1 2 3 4]))




"You can start somewhere else"
(= 2400 (reduce (fn [a b] (* a b)) 100 [1 2 3 4]))




"Numbers are not the only things one can reduce"
(= "longest" (reduce (fn [a b]
                       (if (< (count a) (count b)) b a))
                        ["which" "word" "is" "longest"]))


