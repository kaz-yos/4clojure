;;; https://www.4clojure.com

;;; Intro to vectors
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))


;;; https://www.4clojure.com/problem/71
;; -> macro for rearranging functions in reverse order
(=
 ;; usual method
 (last (sort (rest (reverse [2 5 4 1 3 6]))))
 ;; unix pipeline-like method (like dplyr's %.%)
 (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))
 ;;
 5)


;;; 4Clojure Question 29
;;
;; Write a function which takes a string and returns a new string containing only the capital letters.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [st]
  (clojure.string/replace st #"[a-z,0-9,$#*&() !]" ""))

;; This is NOT A-Z
(defn __ [st]
  (clojure.string/replace st #"[^A-Z]" ""))

(= (__ "HeLlO, WoRlD!") "HLOWRD")
(empty? (__ "nothing"))
(= (__ "$#A(*&987Zf") "AZ")

;; re-seq is nice
;; clojure.core/re-seq
;; ([re s])
;;   Returns a lazy sequence of successive matches of pattern in string,
;;   using java.util.regex.Matcher.find(), each such match processed with
;;   re-groups.
(re-seq #"[A-Z]" "Kazuki Yoshida")
(apply str (re-seq #"[A-Z]" "Kazuki Yoshida"))
;; ^ means NOT
(re-seq #"[^A-Z]" "Kazuki Yoshida")

;; as is and lower case compared. Non matching filtered in.
(filter #(not= (.toLowerCase (str %)) (str %))
        "Kazuki Yoshida")

(filter #(= (.toUpperCase (str %)) (str %))
        "Kazuki Yoshida")



;;; 4Clojure Question 32
;;
;; Write a function which duplicates each element of a sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [seq]
  (mapcat (partial repeat 2) seq))

(defn __ [seq]
  (interleave seq seq))
(interleave [1 2 3] [1 2 3])

(= (__ [1 2 3]) '(1 1 2 2 3 3))
(= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))


(#(interleave % %) [1 2 3])



;;; 4Clojure Question 48
;;
;; The some function takes a predicate function and a collection.  It returns the first logical true value of (predicate x) where x is an item in the collection.
;;
;; Use M-x 4clojure-check-answers when you're done!
(some #{2 7 6} [5 6 7 8])

;; Using a set
(= __ (some #{2 7 6} [5 6 7 8]))
(= __ (some #(when (even? %) %) [5 6 7 8]))




;;; 4Clojure Question 34
;;
;; Write a function which creates a list of all integers in a given range.
;;
;; Restrictions (please don't use these function(s)): range
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [start end]
  (if (= start (dec end))
    start
    (flatten [start (__ (inc start) end)])))

(defn __ [start end]
  (if (= start end)
    '()
    (cons start (__ (inc start) end))))

;; by infinite sequence created by iterate
(defn __ [start end]
  (take (- end start) (iterate (fn [x] (inc x)) start)))

(= (__ 1 4) '(1 2 3))
(= (__ -2 2) '(-2 -1 0 1))
(= (__ 5 8) '(5 6 7))





;;; 4Clojure Question 28
;;
;; Write a function which flattens a sequence.
;;
;; Restrictions (please don't use these function(s)): flatten
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  ;; If it is an element
  (if (not (or (list? s) (vector? s)))
    ;; Return itself as a list, necessary for mapcat to work
    (list s)
    ;; If it is a collection, map itself to elements.
    (mapcat __ s))
  )

(defn __ [s]
  ;; If it is an element (not a collection)
  (if (not (coll? s))
    ;; Return itself as a list, necessary for mapcat to work
    (list s)
    ;; If it is a collection, map itself to elements.
    (mapcat __ s))
  )

(defn __ [s] (if (not (coll? s)) [s] (mapcat __ s)))

(defn __ [s] (if (coll? s) (mapcat __ s) [s]))

(= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
(= (__ '((((:a))))) '(:a))

;; mapcat: map a function, concatenate results
(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
;; (0 1 2 3 4 5 6 7 8 9)




;;; 4Clojure Question 42
;;
;; Write a function which calculates factorials.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n]
  (if (zero? n)
    1
    (* n (__ (dec n)))))

(defn __ [n] (if (zero? n) 1 (* n (__ (dec n)))))

(= (__ 1) 1)
(= (__ 3) 6)
(= (__ 5) 120)
(= (__ 8) 40320)

;; Create a range and apply * to multiply them all
(#(apply * (range 2 (+ 1 %))) 1)





;;; 4Clojure Question 39
;;
;; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
;;
;; Restrictions (please don't use these function(s)): interleave
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a b]
  ;; Stop if either is empty,
  (if (= 0 (min (count a) (count b)))
    ;; and return an empty vector
    []
    ;; Otherwise recurse
    (flatten [(first a) (first b) (__ (rest a) (rest b))])))

(defn __ [a b]
  (if (or (empty? a) (empty? b))
    []
    (flatten [(first a) (first b) (__ (rest a) (rest b))])))

;; aceeca1
(defn __ [a b]
  (mapcat #(list %1 %2) a b))

(= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
(= (__ [1 2 3 4] [5]) [1 5])
(= (__ [30 20] [25 15]) [30 25 20 15])

(mapcat #(list %1 %2) [30 20] [25 15 1 3 ])
(mapcat list [30 20] [25 15 1 3 ])
(map list [30 20] [25 15 1 3 ])



;;; 4Clojure Question 30
;;
;; Write a function which removes consecutive duplicates from a sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (mapcat distinct (partition-by identity s)))

(= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
(= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

(partition-by identity "Leeeeeerrroyyy")
(partition-by identity [1 1 2 3 3 2 2 3])
(group-by identity [1 1 2 3 3 2 2 3])   ; group-by does not work here.
(mapcat vals (group-by identity "Leeeeeerrroyyy"))




;;; 4Clojure Question 47
;;
;; The contains? function checks if a KEY is present in a given collection.  This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.
;;
;; Use M-x 4clojure-check-answers when you're done!

(def __ 4)
(contains? #{4 5 6} __)         ; Hash set. a key is value itself
(contains? [1 1 1 1 1] __)      ; for a vector key 4 is just position 5
(contains? {4 :a 2 :b} __)      ; map. keys are keys
(not (contains? '(1 2 4) __))



;;; 4Clojure Question 45
;;
;; The iterate function can be used to produce an infinite lazy sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= __ (take 5 (iterate #(+ 3 %) 1)))
[1 4 7 10 13]



;;; 4Clojure Question 40
;;
;; Write a function which separates the items of a sequence by an arbitrary value.
;;
;; Restrictions (please don't use these function(s)): interpose
;;
;; Use M-x 4clojure-check-answers when you're done!

;; list the interposing value and each element of the collection.
;; Then mapcat to create a one-level list.
;; The first element will be the interposing value, so remove by (rest)
(defn __ [x s]
  (rest (mapcat #(list x %) s)))

(defn __ [x s]
  (butlast (interleave s (repeat x))))

(= (__ 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
(= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])




;;; 4Clojure Question 31
;;
;; Write a function which packs consecutive duplicates into sub-lists.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (partition-by identity s))

(= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))



;;; 4Clojure Question 41
;;
;; Write a function which drops every Nth item from a sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!

(butlast (take 3 [1 2 3 4 5 6 7 8]))

(reverse (take (- 8 3) (reverse [1 2 3 4 5 6 7 8])))

(defn __ [s x]
  (let [length (count s)]
   (if (< length x)
     s
     [(butlast (take x s))
      (__ (reverse (take (- length x) (reverse s))) x)])))

(defn __ [s x]
  (let [length (count s)]
    (if (< length x)
      s
      (flatten (list (butlast (take x s))
                     (__ (reverse (take (- length x) (reverse s))) x))))))

;; Take (x - 1) elements work on the rest recursively
(defn __ [s x]
  (let [length (count s)]
    (if (< length x)
      s
      (flatten (list (take (dec x) s)
                     (__ (reverse (take (- length x) (reverse s))) x))))))

;; opposite of take is drop
(defn __ [s x]
  (let [length (count s)]
    (if (< length x)
      s
      (flatten (list (take (dec x) s)
                     (__ (drop x s) x))))))

(__ [] 1)
(__ [1 2 3] 2)

(partition-all 3 [1 2 3 4 5 6 7 8])

;; Partition all in partitions of 3
(defn __ [s x]
  (mapcat #(take (- x 1) %) (partition-all x s)))
(defn __ [s x]
  (mapcat #(take (dec x) %) (partition-all x s)))

(defn __ [coll n]
  (->> (partition-all n coll)
       (map (partial take (dec n)))
       (flatten)))

(defn __ [coll n]
  (->> (partition-all n coll)   ; first step
       (mapcat (partial take (dec n)))))        ; result is then fed to this as a last argument

(= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
(= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])



;;; 4Clojure Question 52
;;
;; Let bindings and function parameter lists support destructuring.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= [2 4] (let [[a b c d e f g] (range)]
           (vector c e)))


;;; 4Clojure Question 49
;;
;; Write a function which will split a sequence into two parts.
;;
;; Restrictions (please don't use these function(s)): split-at
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n s]
  [(take n s) (reverse (take (- (count s) n) (reverse s)))])

(drop 3 [1 2 3 4 5])


(= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])




;;; 4Clojure Question 51
;;
;; Here is an example of some more sophisticated destructuring.
;;
;; Use M-x 4clojure-check-answers when you're done!

(def __ [1 2 3 4 5])

(= [1 2 [3 4 5] [1 2 3 4 5]]
   ;; a 1; b 2; rest as c; whole as d
   (let [[a b & c :as d] __]
     [a b c d]))
;; a 1
;; b 2
;; c [3 4 5]
;; d [1 2 3 4 5]

;; destructuring
(let [[_ _ x] [1 2 3]]
  x)

(let [[_ x _] [1 2 3]]
  x)

(let [[x _ y] [1 2 3]]
  [x y])



;;; 4Clojure Question 83
;;
;; Write a function which takes a variable number of booleans.  Your function should return true if some of the parameters are true, but not all of the parameters are true.  Otherwise your function should return false.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [& rest]
  (if (every? (fn [x] x) rest)
    false
    (if (some (fn [x] x) rest)
      true
      false)))

;; identity do the
(defn __ [& rest]
  (if (every? identity rest)
    false
    (if (some identity rest)
      true
      false)))

(defn __ [& rest]
  (let [len1 (count rest)
        len2 (count (filter (fn [x] x) rest))]
    len2))

;; if not all equal, true
(defn __ [& rest]
  (not= rest))

(= false (__ false false))
(= true (__ true false))
(= false (__ true))
(= true (__ false true false))
(= false (__ true true true))
(= true (__ true true true false))

(not-any? identity [true true true])
(not-any? identity [false true])
(not-any? identity [false false])
(every? identity [true true true])
(not-every? true? [true true true])



;;; 4Clojure Question 61
;;
;; Write a function which takes a vector of keys and a vector of values and constructs a map from them.
;;
;; Restrictions (please don't use these function(s)): zipmap
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [list-a list-b]
  (zipmap list-a list-b))
(__ [:a :b :c] [1 2 3])

(defn __ [list-a list-b]
  (zipmap (interleave list-a list-b)))

(defn __ [list-a list-b]
  (partition-all 2 (interleave list-a list-b)))
(__ [:a :b :c] [1 2 3])

(defn __ [list-a list-b]
  (apply hash-map (interleave list-a list-b)))
(__ [:a :b :c] [1 2 3])

(defn __ [list-a list-b]
  (into {} (for [list-a a list-b b] [a b])))
(__ [:a :b :c] [1 2 3])

(defn __ [a b]
  (apply sorted-map (interleave a b)))
(__ [:a :b :c] [1 2 3])
(__ [:foo :bar] ["foo" "bar" "baz"])

(doc sorted-map)
;; -------------------------
;; clojure.core/sorted-map
;; ([& keyvals])
;;   keyval => key val
;;   Returns a new sorted map with supplied mappings.  If any keys are
;;   equal, they are handled as if by repeated uses of assoc.


(= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
(= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})



;;; 4Clojure Question 166
;;
;; For any orderable data type it's possible to derive all of the basic comparison operations (&lt;, &le;, =, &ne;, &ge;, and &gt;) from a single operation (any operator but = or &ne; will work). Write a function that takes three arguments, a <var>less than</var> operator for the data and two items to compare. The function should return a keyword describing the relationship between the two items. The keywords for the relationship between <var>x</var> and <var>y</var> are as follows:
;;
;;
;;
;; <ul>
;;
;; <li><var>x</var> = <var>y</var> &rarr; :eq</li>
;;
;; <li><var>x</var> &gt; <var>y</var> &rarr; :gt</li>
;;
;; <li><var>x</var> &lt; <var>y</var> &rarr; :lt</li>
;;
;; </ul>
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [fn1 x y]
  (cond (fn1 x y) :lt
        (fn1 y x) :gt
        :else     :eq))
(__ < 5 1)
(__ (fn [x y] (< (count x) (count y))) "pear" "plum")

(= :gt (__ < 5 1))
(= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
(= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
(= :gt (__ > 0 2))



;;; 4Clojure Question 81
;;
;; Write a function which returns the intersection of two sets.  The intersection is the sub-set of items that each set has in common.
;;
;; Restrictions (please don't use these function(s)): intersection
;;
;; Use M-x 4clojure-check-answers when you're done!

(clojure.set/union #{0 1 2 3} #{2 3 4 5})

(require 'clojure.set)
;; require as
(require '[clojure.set :as cset])
(cset/union #{0 1 2 3} #{2 3 4 5})

(defn __ [a b]
  (let [c   (clojure.set/union a b)
        c-a (clojure.set/difference c a)
        c-b (clojure.set/difference c b)]
    (clojure.set/difference c (clojure.set/union c-a c-b))
    ))

(#{0 1 2 3} 4)
(contains? #{0 1 2 3} 4)

;; Collection works as a function. give each element and filter.
(defn __ [s1 s2]
  (set (filter s1 s2)))

(defn __ [s1 s2]
  (set (filter #(contains? s1 %) s2)))

(defn __ [a b]
  (-> (filter a b)
      set))

(= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (__ #{0 1 2} #{3 4 5}) #{})
(= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})


;;; 4Clojure Question 66
;;
;; Given two integers, write a function which
;;
;; returns the greatest common divisor.
;;
;; Use M-x 4clojure-check-answers when you're done!

(filter (fn [x] (= 0 (rem 1023 x))) (range 1 (inc 1023)))

(defn __ [a b]
  (let [small (min a b)
        large (max a b)
        for-small (set (filter (fn [x] (= 0 (rem small x))) (range 1 (inc small))))
        for-large (set (filter (fn [x] (= 0 (rem large x))) (range 1 (inc large))))]

    (apply max (clojure.set/intersection for-small for-large))
    ))

(defn __ [a b]
  (apply max (filter
              #(= 0 (rem a %) (rem b %)) ; reminder 0 for both
              (range 1 (max a b)))))

(= (__ 2 4) 2)
(= (__ 10 5) 5)
(= (__ 5 7) 1)
(= (__ 1023 858) 33)




;;; 4Clojure Question 62
;;
;; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
;;
;; Restrictions (please don't use these function(s)): iterate
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [f x]
  [x (f x) (f (f x))])

(defn __ [f x]
  (lazy-seq (loop [a x]
              a
              (recur (f a)))))


(defn positive-numbers
  ([] (positive-numbers 1))
  ([n] (cons n (lazy-seq (positive-numbers (inc n))))))
(take 5 (positive-numbers))

(defn __ [f x]
  (cons x (lazy-seq (__ f (f x)))))

(defn __ [f x] (reductions (fn [i _] (f i)) (repeat x)))
(take 5 (__ #(* 2 %) 1))

(defn __ [f x] (reduce (fn [i _] (f i)) (repeat x)))
;; (take 5 (__ #(* 2 %) 1))     ; this dies

(= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (__ inc 0)) (take 100 (range)))
(= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))




;;; 4Clojure Question 107
;;
;; <p>Lexical scope and first-class functions are two of the most basic building blocks of a functional language like Clojure. When you combine the two together, you get something very powerful called <strong>lexical closures</strong>. With these, you can exercise a great deal of control over the lifetime of your local bindings, saving their values for use later, long after the code you're running now has finished.</p>
;;
;;
;;
;; <p>It can be hard to follow in the abstract, so let's build a simple closure. Given a positive integer <i>n</i>, return a function <code>(f x)</code> which computes <i>x<sup>n</sup></i>. Observe that the effect of this is to preserve the value of <i>n</i> for use outside the scope in which it is defined.</p>
;;
;; Use M-x 4clojure-check-answers when you're done!

(require 'clojure.contrib.math)

(defn __ [n]
  (fn [x]
    (apply * (repeat n x))))

(defn __ [n]
  (partial (Math/pow % n)))

((__ 4) 3)

(take 1 ((partial (- % 2)) 5))


(= 256 ((__ 2) 16),
       ((__ 8) 2))
(= [1 8 27 64] (map (__ 3) [1 2 3 4]))
(= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))



;;; 4Clojure Question 99
;;
;; Write a function which multiplies two numbers and returns the result as a sequence of its digits.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a b]
  (map read-string (clojure.string/split (str (* a b)) #"")))

(defn __ [a b]
  (clojure.string/split (str (* a b)) #""))

(defn __ [a b]
  (map #(. Integer parseInt %) (clojure.string/split (str (* a b)) #"")))

(defn __ [a b]
  (map #(Integer/parseInt %) (clojure.string/split (str (* a b)) #"")))

;; create number as a string, map the letters, covert each letter to string and back to interger.
(fn __ [a b]
  (map #(Integer/parseInt (str %))  (str (* a b))))



;; use read-string to evaluate "1"
(defn __ [a b]
  (vec (map read-string (clojure.string/split (str (* a b)) #""))))

(= (__ 1 1) [1])
(= (__ 99 9) [8 9 1])
(= (__ 999 99) [9 8 9 0 1])


;; http://stackoverflow.com/questions/2640169/whats-the-easiest-way-to-parse-numbers-in-clojure
(let [input "5  10  0.002\n4  12  0.003"]
        (re-seq #"[\d.]+" input))
(let [input "5  10  0.002\n4  12  0.003"]
        (map read-string (re-seq #"[\d.]+" input)))


;;; 4Clojure Question 90
;;
;; Write a function which calculates the <a href="http://en.wikipedia.org/wiki/Cartesian_product"> Cartesian product</a> of two sets.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a b]
  (set (mapcat (fn [x]
                 (map (fn [y] [x y])
                      b))
               a)))

(defn __ [x1 x2]
  (set (for [i1 x1 i2 x2]       ; this does all possible combinations
         [i1 i2])))


(= (__ #{"ace" "king" "queen"} #{"&#9824;" "&#9829;" "&#9830;" "&#9827;"})
   #{["ace"   "&#9824;"] ["ace"   "&#9829;"] ["ace"   "&#9830;"] ["ace"   "&#9827;"]
     ["king"  "&#9824;"] ["king"  "&#9829;"] ["king"  "&#9830;"] ["king"  "&#9827;"]
     ["queen" "&#9824;"] ["queen" "&#9829;"] ["queen" "&#9830;"] ["queen" "&#9827;"]})

(= (__ #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

(= 300 (count (__ (into #{} (range 10))
                  (into #{} (range 30)))))



;;; 4Clojure Question 63
;;
;; Given a function f and a sequence s, write a function which returns a map.  The keys should be the values of f applied to each item in s.  The value at each key should be a vector of corresponding items in the order they appear in s.
;;
;; Restrictions (please don't use these function(s)): group-by
;;
;; Use M-x 4clojure-check-answers when you're done!

;; what we want is group-by
(defn __ [f s]
  (group-by f s))

;; interleaving
(defn __ [f s]
  (let [ks (map f s)
        vs s]
    (interleave ks vs)))
(__ #(> % 5) [1 3 6 8])

;; partition every 2
(defn __ [f s]
  (let [ks (map f s)
        vs s]
    (partition 2 (interleave ks vs))))
(__ #(> % 5) [1 3 6 8])

;; create 1-pair maps
(defn __ [f s]
  (let [ks (map f s)]
    (map (fn [x] (zipmap [(first x)] [(last x)])) (partition 2 (interleave ks s)))))
(__ #(> % 5) [1 3 6 8])
(__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])

;; merge-with is not successful
(apply merge-with vector '({false 1} {false 3} {true 6} {true 8}))
(apply merge-with list '({1/2 [1 2]} {1/2 [2 4]} {2/3 [4 6]} {1/2 [3 6]}))

(vals (apply merge-with list '({1/2 [1 2]} {1/2 [2 4]} {2/3 [4 6]} {1/2 [3 6]})))

'({1/2 [1 2]} {1/2 [2 4]} {2/3 [4 6]} {1/2 [3 6]})

(partition-by (fn [x] (keys x)) '({1/2 [1 2]} {1/2 [2 4]} {2/3 [4 6]} {1/2 [3 6]}))

(into [] '(1 2 3 4))

(defn __ [f s]
  (let [ks (map f s)]

    (apply merge-with vector
           (map (fn [x] (zipmap [(first x)] [(last x)])) (partition 2 (interleave ks s))))))

(defn __ [f s]
  (let [ks (map f s)]

    (apply merge-with (fn [x y] (conj [x] y))
           (map (fn [x] (zipmap [(first x)] [(last x)])) (partition 2 (interleave ks s))))))

(defn __ [f s]
  (let [ks (map f s)]
    ;;                                        v
    (->> (interleave ks s)
         (partition 2                          )
         (map #(zipmap [(first %)] [(last %)]) )
         (apply merge-with list)
         )
    ))
(__ #(> % 5) [1 3 6 8])
(__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
(__ count [[1] [1 2] [3] [1 2 3] [2 3]])

(map keys [{1 [1]} {2 [1 2]} {1 [3]} {3 [1 2 3]} {2 [2 3]}])
(map vals [{1 [1]} {2 [1 2]} {1 [3]} {3 [1 2 3]} {2 [2 3]}])

(apply merge [{1 [1]} {2 [1 2]} {1 [3]} {3 [1 2 3]} {2 [2 3]}])

(apply merge-with list [{1 [1]} {2 [1 2]} {1 [3]} {3 [1 2 3]} {2 [2 3]}])

(apply merge-with concat [{1 [1]} {2 [1 2]} {1 [3]} {3 [1 2 3]} {2 [2 3]}])

(apply merge-with (comp list list) [{1 [1]} {2 [1 2]} {1 [3]} {3 [1 2 3]} {2 [2 3]}])

(filter #(= 1 (keys %)) [{1 [1]} {2 [1 2]} {1 [3]} {3 [1 2 3]} {2 [2 3]}])


(map (fn [x] {(key x) (val x)}) {2/3 [4 6], 1/2 [[[1 2] [2 4]] [3 6]]})

(map (fn [x] {(key x)
              (val x)})
     {2/3 [4 6], 1/2 [[[1 2] [2 4]] [3 6]]})


;; keys
(defn __ [f s]
  (let [ks (map f s)
        ;; 1-pair maps in a list
        mp (map #(zipmap [(first %)] [(last %)]) (partition 2 (interleave ks s)))
        ;; distinct keys in a list
        ks2 (flatten (distinct (map keys mp)))
        ]

    (map ; loop over keys
     (fn [k] (map ; loop over individual maps (used as a function)
              (fn [m] (m k))
              mp))
     ks2)
    ))

;; reduce solution
(def add-to-vector (fn [f map1 new-val]
                     (let [key1 (f new-val)]
                       (if (contains? map1 key1)
                         ;; if already contained, add to vector
                         (assoc map1 key1 (conj (key1 map1) new-val))
                         ;; if not already contained, just add a vector
                         (assoc map1 key1 [new-val])))))

(add-to-vector #(> % 5) {} 2)
(add-to-vector #(> % 5) {false [2]} 2)

(defn __ [f s]
  (let [add-to-vector (fn [map1 new-val]
                        (let [key1 (f new-val)]
                          (if (contains? map1 key1)
                            ;; if already contained, add to vector
                            (assoc map1 key1 (conj (get map1 key1) new-val))
                            ;; if not already contained, just add a vector
                            (assoc map1 key1 [new-val]))))]

    (reduce add-to-vector {} s)))

(defn __ [f s]
  (apply merge-with concat (map #(hash-map (f %1) [%1]) s)))

(defn __ [f s]
  (apply merge-with concat (map (fn [x] (hash-map (f x) [x])) s)))

(map #(hash-map (count %1) [%1]) [[1 2] [2 4] [4 6] [3 6]])


(= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
(= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
(= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})

(group-by count [[1] [1 2] [3] [1 2 3] [2 3]])



;;; 4Clojure Question 122
;;
;; Convert a binary number, provided in the form of a string, to its numerical value.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (let [a (map #(if (= "1" %) 1 0) (clojure.string/split s #""))
        b (iterate (fn [x] (* 2 x)) 1)]

    (->> (interleave a b                )
         (partition-all 2               )
         (map (fn [x] (apply * x))      )
         (apply +)
         )
    )
  )

(__ "1001")
(map (fn [x] (apply * x)) '((1 1) (0 2) (0 4) (1 8)))
(apply + (map (fn [x] (apply * x)) '((1 1) (0 2) (0 4) (1 8))))


;; clean
(defn __ [s]
  (let [a (map #(if (= "1" %) 1 0) (clojure.string/split s #""))
        a (reverse a)
        b (iterate (fn [x] (* 2 x)) 1)]

    (->> (interleave a b                )
         (partition-all 2               )
         (map (fn [x] (apply * x))      )
         (apply +                       ))))

;; Integer/parseInt is good
(Integer/parseInt "110" 2)

(= 0     (__ "0"))
(= 7     (__ "111"))
(= 8     (__ "1000"))
(= 9     (__ "1001"))
(= 255   (__ "11111111"))
(= 1365  (__ "10101010101"))
(= 65535 (__ "1111111111111111"))




;;; 4Clojure Question 88
;;
;; Write a function which returns the symmetric difference of two sets.  The symmetric difference is the set of items belonging to one but not both of the two sets.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a b]
  (let [intrsct (clojure.set/intersection a b)]

    (clojure.set/union (clojure.set/difference a intrsct)
                       (clojure.set/difference b intrsct))
    ))

(set (concat #{1 2 3 4 5 6} #{1 3 5 7}))

;; clojure.core/disj
;; ([set] [set key] [set key & ks])
;;   disj[oin]. Returns a new set of the same (hashed/sorted) type, that
;;   does not contain key(s).
(defn __ [a b]
  (clojure.set/union (apply disj a b) (apply disj b a)))
;; (apply disj a b)     ; b is opened up

(defn __ [a b]
  (clojure.set/difference (set (concat a b)) (clojure.set/intersection a b)))

(= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (__ #{:a :b :c} #{}) #{:a :b :c})
(= (__ #{} #{4 5 6}) #{4 5 6})
(= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})




;;; 4Clojure Question 143
;;
;; Create a function that computes the <a href="http://en.wikipedia.org/wiki/Dot_product#Definition">dot product</a> of two sequences. You may assume that the vectors will have the same length.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [x y]
  (apply + (map #(apply * %) (partition-all 2 (interleave x y)))))

;; map can take multiple collections!!
(map * [1 2 3] [4 5 6])

(defn __ [x y]
  (apply + (map * x y)))

(= 0 (__ [0 1 0] [1 0 0]))
(= 3 (__ [1 1 1] [1 1 1]))
(= 32 (__ [1 2 3] [4 5 6]))
(= 256 (__ [2 5 6] [100 10 1]))




;;; 4Clojure Question 120
;;
;; Write a function which takes a collection of integers as an argument.  Return the count of how many elements are smaller than the sum of their squared component digits.  For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.
;;
;; Use M-x 4clojure-check-answers when you're done!

(map read-string (map clojure.string/split (map str (range 30)) #""))

(->> (range 30)
     (map (fn [elt]
            (map #(* % %) (map read-string (clojure.string/split (str elt) #"")))
            )))


(->> (range 30)
     (map (fn [elt]
            (apply +
             (map #(* % %) (map read-string (clojure.string/split (str elt) #""))))

            )))

(->> (range 30)
     (map
      (fn [elt]
        (->> (clojure.string/split (str elt) #"")
             (map read-string )
             (map #(* % %)    )
             (apply +         ))
        )
      ))




(defn __ [s]
  (count (filter
          (fn [elt]
            (->> (clojure.string/split (str elt) #"")
                 (map   read-string   ) ; read-string does not work in 4clojure?
                 (map   #(* % %)      )
                 (apply +             )
                 (<     elt           )
                 )
            )
          s)
         )
  )

;; identity based method
;; square
(defn sqr [x]
  (* x x))

(map #(sqr (Integer. (str (identity %)))) (str 15))

(reduce + (map #(sqr (Integer. (str (identity %)))) (str 15)))

(defn __ [s]
  (let [sqr (fn [x] (* x x))
        ]))




(= 8 (__ (range 10)))
(= 19 (__ (range 30)))
(= 50 (__ (range 100)))
(= 50 (__ (range 1000)))



;;; 4Clojure Question 100
;;
;; Write a function which calculates the <a href="http://en.wikipedia.org/wiki/Least_common_multiple">least common multiple</a>.  Your function should accept a variable number of positive integers or ratios.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [& args]
  (let [inf-seq    (iterate inc 1)
        ;; number of args
        n-args     (count args)
        ;; Sequence of groups of n-th elements from each number's multiples
        seq-groups (map (fn [x] (map #(* x %) args)) inf-seq)]
    ;;
    (loop [sq  (rest  seq-groups)
           acc (first seq-groups)]
      ;; check for non-unique values (max frequency = n-args)
      (if (= (apply max (vals (frequencies acc))) n-args)
        ;; if met,
        (->> (frequencies acc)
             ;; pick the [k v] with v = n-args
             (filter #(= (val %) n-args),  )
             first  ; get [k v]
             first) ; get k
        ;;
        ;; if not met, recur
        (recur (rest sq) (concat acc (first sq)))))))

;; pcl
(defn __ [& xs]
  (/ (apply * xs)

     (reduce #(if (zero? %2)
                %
                (recur %2 (mod % %2)))
             ;;
             xs)))


(== (__ 2 3) 6)
(== (__ 5 3 7) 105)
(== (__ 1/3 2/5) 2)
(== (__ 3/4 1/6) 3/2)
(== (__ 7 5/7 2 3/5) 210)




;;; 4Clojure Question 44
;;
;; Write a function which can rotate a sequence in either direction.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n s]
  (for [n (range n)]
    (conj (last s) (rest s))
    ))

(defn __ [n sq]
  (loop [s sq
         i 1]
    (if (> i n)
      s
      (recur (cons (last s) (butlast s)) (inc i)))
    ))

(defn __ [n sq]
  (loop [s sq
         i 1]
    (if (> i n)
      s
      (recur (conj (vec (rest s)) (first s)) (inc i)))
    ))
(__ 2 [1 2 3 4 5])
(__ 6 [1 2 3 4 5])

(defn __ [n sq]
  (loop [s sq
         i 0]
    (if (= i n)
      s
      (recur (conj (vec (rest s)) (first s)) (inc i)))
    ))
(__ 2 [1 2 3 4 5])
(__ 6 [1 2 3 4 5])

(defn __ [n sq]
  (loop [s sq
         i 0]
    (cond
     (n > 0) (if (= i n)
               s
               (recur (conj (vec (rest s)) (first s)) (inc i)))

     (n < 0) (if (= i n)
               s
               (recur (cons (last s) (butlast s)) (dec i)))

     :else s)
    ))


(defn __ [n sq]
  (loop [s (if (>= n 0) sq (reverse sq))
         i 0]

    (if (= i (if (>= n 0) n (* -1 n)))

      ;; return result
      (if (>= n 0) s (reverse s))

      ;; recur
      (recur (conj (vec (rest s)) (first s)) (inc i)))
    ))

;;
(mod 2 5)
(mod -2 5)
(defn __ [n sq] (let [n (mod n (count sq))]
                  (concat (drop n sq)
                          (take n sq)
                          )))

(__ 2 [1 2 3 4 5])      ; take 2
(__ -2 [1 2 3 4 5])     ; take 3
(__ 6 [1 2 3 4 5])      ; take 1

(= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (__ 1 '(:a :b :c)) '(:b :c :a))
(= (__ -4 '(:a :b :c)) '(:c :a :b))




;;; 4Clojure Question 173
;;
;; Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.): <a href="http://clojure.org/special_forms#Special Forms--(let [bindings* ] exprs*)">(let [bindings* ] exprs*)</a>
;;
;;
;;
;; Complete the bindings so all let-parts evaluate to 3.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= 3
   (let [[f s] [+ (range 3)]]
     (apply f s))

   (let [[[f s] b] [[+ 1] 2]]
     (f s b))

   (let [[f s] [inc 2]]
     (f s)))



;;; 4Clojure Question 50
;;
;; Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (group-by class s))
(set (__ [1 :a 2 :b 3 :c]))

(defn __ [s]
  (group-by type s))
(set (__ [1 :a 2 :b 3 :c]))

(defn __ [s]
  (vals (group-by class s)))

(= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})



;;; 4Clojure Question 54
;;
;; Write a function which returns a sequence of lists of x items each.  Lists of less than x items should not be returned.
;;
;; Restrictions (please don't use these function(s)): partition, partition-all
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n coll]
  (if (< (count coll) n)
      []
      (cons (take n coll) (__ n (drop n coll)))) )


(= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (__ 3 (range 8)) '((0 1 2) (3 4 5)))




;;; 4Clojure Question 43
;;
;; Write a function which reverses the interleave process into x number of subsequences.
;;
;; Use M-x 4clojure-check-answers when you're done!

(mapcat identity (repeat (range 3)))

(defn __ [coll n]
  (interleave (mapcat identity (repeat (range n))) coll)
  )
(__ [1 2 3 4 5 6] 2)
'(0 1 1 2 0 3 1 4 0 5 1 6)

(defn __ [coll n]
  (group-by first (partition-all 2 (interleave (mapcat identity (repeat (range n))) coll)))
  )
(__ [1 2 3 4 5 6] 2)
'{0 [(0 1) (0 3) (0 5)], 1 [(1 2) (1 4) (1 6)]}

(defn __ [coll n]
  (vals (group-by first (partition-all 2 (interleave (mapcat identity (repeat (range n))) coll))))
  )
(__ [1 2 3 4 5 6] 2)


(defn __ [coll n]
  (->> (range n)                ; thread this through the last argument
       (repeat                )
       (mapcat identity       )
       (#(interleave % coll)  ) ; thread through the first argument for this function
       (partition-all 2       )
       (group-by first        )
       (vals                  )
       (map #(map second %)   )
       )
  )
(__ [1 2 3 4 5 6] 2)

(defn __ [xs s] (apply map vector (partition s xs)))
(__ [1 2 3 4 5 6] 2)
(partition 2 [1 2 3 4 5 6])
(apply map vector (partition 2 [1 2 3 4 5 6]))

;; map can work on multiple collections simultaneously
(map vector '(1 2) '(3 4) '(5 6))
;; vector 1 3 5 and vector 2 4 6 at the same time.




(map identity '([(0 1) (0 3) (0 5)] [(1 2) (1 4) (1 6)]))
(map (fn [v] (map second v)) '([(0 1) (0 3) (0 5)] [(1 2) (1 4) (1 6)]))


(defn __ [coll n]
  (map #(mapcat second %) (vals (group-by first (partition-all 2 (interleave (mapcat identity (repeat (range n))) coll)))))
  )

(= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))



;;; 4Clojure Question 126
;;
;; Enter a value which satisfies the following:
;;
;; Use M-x 4clojure-check-answers when you're done!

(and (= (class 'clojure.lang.Symbol) clojure.lang.Symbol) clojure.lang.Symbol)

(let [x __]
  (and (= (class x) x) x))




;;; 4Clojure Question 118
;;
;; <p>Map is one of the core elements of a functional programming language. Given a function <code>f</code> and an input sequence <code>s</code>, return a lazy sequence of <code>(f x)</code> for each element <code>x</code> in <code>s</code>.
;;
;; Restrictions (please don't use these function(s)): map, map-indexed, mapcat, for
;;
;; Use M-x 4clojure-check-answers when you're done!

;; reduce causes realization
(defn __ [f s]
  (reduce (fn [a b] (conj a (f b))) [] s))

;; reduction is lazy
(defn __ [f s]
  (reductions (fn [a b] (conj a (f b))) [] s))
(__ inc [2 3 4 5 6])

;; map definition
(defn __ [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (chunked-seq? s)
       ;; if chunked
       (let [c (chunk-first s)
             size (int (count c))
             b (chunk-buffer size)]
         (dotimes [i size]
           (chunk-append b (f (.nth c i))))
         (chunk-cons (chunk b) (__ f (chunk-rest s))))
       ;; if not chunked
       (cons (f (first s)) (__ f (rest s)))))))

;; minimum based on map definition
(defn __ [f coll]
  ;; without lazy-seq stack overflow occurs
  (lazy-seq
   ;; until empty coll is bound to s
   (when-let [s (seq coll)]
     ;; apply f to the first element, recur with the rest
     (cons (f (first s)) (__ f (rest s))))))

;; aceeca1's solution:
;; reduction discarding %1 is basically the same as the first reduce example
;; need to give nil as a initial value and discard f(nil) by rest
(defn __ [f x] (rest (reductions #(f %2) nil x)))



(= [3 4 5 6 7]
   (__ inc [2 3 4 5 6]))
(= (repeat 10 nil)
   (__ (fn [_] nil) (range 10)))
(= [1000000 1000001]
   (->> (__ inc (range))
        (drop (dec 1000000))
        (take 2)))



;;; 4Clojure Question 55
;;
;; Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
;;
;; Restrictions (please don't use these function(s)): frequencies
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (let [intlv (partition 2 (interleave s (repeat 1)))]

    (->> intlv
         (map #(zipmap [(first %)] [(last %)]))
         (apply merge-with + )
         )
    ))

(defn __ [s]

  (apply hash-map (interleave s (repeat 1)))
  )

(apply hash-map [1 2 3 4])

(__ [1 1 2 3 2 1 1])

(for [[k v] (group-by identity [1 1 2 3 2 1 1])]
  [k (count v)])

;; into {} map creates a map
(into {} (for [[k v] (group-by identity [1 1 2 3 2 1 1])]
           [k (count v)]))

(into [] (for [[k v] (group-by identity [1 1 2 3 2 1 1])]
           [k (count v)]))



(= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
(= (__ [:b :a :b :a :b]) {:a 2, :b 3})
(= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})



;;; 4Clojure Question 56
;;
;; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
;;
;; Restrictions (please don't use these function(s)): distinct
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (distinct s))

(defn __ [s]
  (let [cnt (map count (map (fn [x] (filter #(= x %) s)) s))]
    (partition 2 (interleave s cnt))))

(__ [1 2 1 3 1 2 4])

;; sig: seq -> seq
;; purpose remove duplicates
;; stub:
;; (defn __ [s]
;;   [nil nil nil])
;;
(defn __ [s]
  (loop [sq   s
         seen #{}
         acc []]
    (let [fst-sq (first sq)]
      (cond
       (empty? sq)             acc
       ;; if it has been seen
       (contains? seen fst-sq) (recur (rest sq) seen acc)
       ;; if not, add to acc
       :else                   (recur (rest sq) (conj seen fst-sq) (conj acc fst-sq))))))


(= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
(= (__ [:a :a :b :b :c :c]) [:a :b :c])
(= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
(= (__ (range 50)) (range 50))




;;; 4Clojure Question 58
;;
;; Write a function which allows you to create function compositions.  The parameter list should take a variable number of functions, and create a function applies them from right-to-left.
;;
;; Restrictions (please don't use these function(s)): comp
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn [& funs]
  (fn [& rest]
    )
  )

(defn __ [& funs]
  (apply comp funs))

;; signature: funs-> fun
;; purpose apply multiple functions right to left
;; stub:
;; (defn __ [& funs]
;;   (fn [x] :-))
;;
;; this works good for single argument functions
(defn __ [& funs]
  (loop [funs1 (reverse funs)
         acc (fn [x] x)]
    ;;
    (if (empty? funs1)
      acc
      (recur (rest funs1) (fn [& x] ((first funs1) (apply acc x)))))))


;; only the very first function potentially takes multiple args
(defn __ [& funs]
  (loop [funs1 funs
         acc identity] ; function acculators
    ;;
    (if (empty? (rest funs1))
      ;; stop at the very last function (first to be applied)
      ;; this one may take multiple args, use apply, and warp with acc
      (fn [& x] (acc (apply (first funs1) x)))
      ;; Insert each function inside the previous one
      (recur (rest funs1) (fn [x] (acc ((first funs1) x)))))))

;; reduce solutions.
;; make the first exception by using the initial value
(defn __ [& funs]
  (fn [& x] (reduce (fn [v f] (f v)) (apply (last funs) x) (reverse (butlast funs)))))


(= [3 2 1] ((__ rest reverse) [1 2 3 4]))
(= 5 ((__ (partial + 3) second) [1 2 3 4]))
(= true ((__ zero? #(mod % 8) +) 3 5 7 9))
(= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))



;;; 4Clojure Question 135
;;
;; Your friend Joe is always whining about Lisps using the prefix notation for math. Show him how you could easily write a function that does math using the infix notation. Is your favorite language that flexible, Joe?
;;
;;
;;
;; Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. Assume a simple calculator that does not do precedence and instead just calculates left to right.
;;
;; Use M-x 4clojure-check-answers when you're done!

;; 3 elements only
(defn __ [a b c]
  (b a c))
(__ 2 + 5)

(defn __ [& args]
  (let [oprs (filter #(#{+ - * /} %) args)
        nums (filter #(not (#{+ - * /} %)) args)]
    oprs
    ))

;; kaz-yos's solution
(defn __ [& args]
  (let [operators (filter #(#{+ - * /} %)       args)
        numbers   (filter #(not (#{+ - * /} %)) args)
        init-val  (first numbers)
        numbers   (rest  numbers)]

    (loop [oprs    operators    ; list of operators
           nums    numbers      ; list of numbers
           cur-val init-val]    ; current result
      (if (and (= 1 (count oprs))
               (= 1 (count nums)))
        ;; Last case
        ((first oprs) cur-val (first nums))
        ;; Non-last case
        (recur (rest oprs) (rest nums) ((first oprs) cur-val (first nums))))
      )
    ))

;; _pcl's solution
(defn __ [& args]
  (reduce #((first %2) %1 (second %2)) (first args) (partition 2 (rest args))))

(= 7  (__ 2 + 5))
(= 42 (__ 38 + 48 - 2 / 2))
(= 8  (__ 10 / 2 - 1 * 2))
(= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))




;;; 4Clojure Question 59
;;
;; Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list.
;;
;; Restrictions (please don't use these function(s)): juxt
;;
;; Use M-x 4clojure-check-answers when you're done!

;; just is the one we want
;; juxt returns a function that apply each function and return the results in a vector
((juxt + max min) 2 3 5 1 6 4)

(defn __ [& args1]
  (fn [& args2]
    (vec (map #(apply % args2) args1))))

(defn __ [& fns]
  (fn [& xs]
    (vec (map #(apply % xs) fns))))

(= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
(= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
(= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))




;;; 4Clojure Question 110
;;
;; <p>Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers. A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself. For example, <code>[1 1]</code> is pronounced as <code>[2 1]</code> ("two ones"), which in turn is pronounced as <code>[1 2 1 1]</code> ("one two, one one").</p><p>Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations, each element being a pronunciation of the previous element.</p>
;;
;; Use M-x 4clojure-check-answers when you're done!

;; function to get the next one from the seed collection
(defn __ [coll]
  (partition-by identity coll))

(defn __ [coll]
  (map count (partition-by identity coll)))

(defn __ [coll]
  (let [partitioned (partition-by identity coll)
        nums (mapcat distinct partitioned)
        counts (map count partitioned)]
    (interleave counts nums)
    ))

(defn __ [coll]
  (rest (iterate
         ;; fun to iterate
         (fn [coll]
           (let [partitioned (partition-by identity coll)
                 nums (mapcat distinct partitioned)
                 counts (map count partitioned)]
             ;;
             (interleave counts nums)
             ))
         ;; coll to iterate
         coll)))

(defn __ [coll]
  (rest (iterate
         ;; fun to iterate
         (fn [coll]
           (let [partitioned (partition-by identity coll)]
             ;;
             (interleave (map count partitioned) (mapcat distinct partitioned))
             ))
         ;; coll to iterate
         coll)))

(defn __ [coll]
  (rest (iterate
         ;; fun to iterate
         (fn [coll]
           (mapcat (juxt count first) (partition-by identity coll)))
         ;; coll to iterate
         coll)))


(= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))

(= [3 1 2 4] (first (__ [1 1 1 4 4])))

(= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))

(= 338 (count (nth (__ [3 2]) 15)))



;;; 4Clojure Question 147
;;
;; <p>Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors, where each next one is constructed from the previous following the rules used in <a href="http://en.wikipedia.org/wiki/Pascal's_triangle">Pascal's Triangle</a>. For example, for [3 1 2], the next row is [3 4 3 2].</p>
;;
;; <p>Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), if you use an arithmetic operator like + and the result is too large to fit into a 64-bit integer, an exception is thrown. You can use +' to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint.</p>
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [v]
  (if (= 2 (count v))
    (reduce + v)
    (recur (rest v))))

(defn __ [v]
  (if (= 1 (count v))
    (conj v (first v))

    (let [fst (first v)
          lst (last  v)
          mdl (loop [v-in v
                   v-out []]
              (if (= 2 (count v-in))
                (conj v-out (reduce + v-in))
                (recur (rest v-in) (conj v-out (reduce + (take 2 v-in)))))
              )]

      (flatten [fst mdl lst])
      ))
  )


(defn __ [v] (iterate (fn [v]
                (if (= 1 (count v))
                  (conj v (first v))

                  (let [fst (first v)
                        lst (last  v)
                        mdl (loop [v-in v
                                   v-out []]
                              (if (= 2 (count v-in))
                                (conj v-out (reduce +' v-in))
                                (recur (rest v-in) (conj v-out (reduce +' (take 2 v-in)))))
                              )]

                    (flatten [fst mdl lst])
                    ))
                ) v))

(defn __ [v]
  (let [v1 (conj   v 0)
        v2 (concat [0] v)]
    (map #(+' %1 %2) v1 v2)
    ))

(defn __ [v]
  (iterate
   (fn [v]
     (let [v1 (concat   v [0])
           v2 (concat  [0] v)]
       (mapv #(+' %1 %2) v1 v2)
       ))
   v))

(= (second (__ [2 3 2])) [2 5 5 2])
(= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
(= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
(= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))



;;; 4Clojure Question 80
;;
;; A number is "perfect" if the sum of its divisors equal the number itself.  6 is a perfect number because 1+2+3=6.  Write a function which returns true for perfect numbers and false otherwise.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n]
  (= n (apply + (filter #(= 0 (rem n %)) (range 1 n)))))

(defn __ [n]
  (->> (range 1 n)
       (filter #(= 0 (rem n %)) )
       (apply +                 )
       (= n                     )))

(= (__ 6) true)
(= (__ 7) false)
(= (__ 496) true)
(= (__ 500) false)
(= (__ 8128) true)


;;; 4Clojure Question 97
;;
;; <a href="http://en.wikipedia.org/wiki/Pascal%27s_triangle">Pascal's triangle</a> is a triangle of numbers computed using the following rules:<br/></br>- The first row is 1.</br>- Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.<br/><br/>Write a function which returns the nth row of Pascal's Triangle.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n]
  (iterate
   (fn [v]
     (mapv #(+' %1 %2) (concat [0] v) (concat v [0])))
   [1]))

(defn __ [n]
  (nth (iterate
    (fn [v]
      (mapv #(+' %1 %2) (concat [0] v) (concat v [0])))
    [1]) (dec n)))

(= (__ 1) [1])

(= (map __ (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]])

(= (__ 11)
   [1 10 45 120 210 252 210 120 45 10 1])



;;; 4Clojure Question 104
;;
;; This is the inverse of <a href='92'>Problem 92</a>, but much easier. Given an integer smaller than 4000, return the corresponding roman numeral in uppercase, adhering to the <a href='http://www.numericana.com/answer/roman.htm#valid'>subtractive principle</a>.
;;
;; Use M-x 4clojure-check-answers when you're done!

[1000 900 500 400 100 90 50 40 10 9 5 4 1]
["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]

;; Loop solution.
(defn __ [n]
  (loop [d   [1000 900 500 400 100 90 50 40 10 9 5 4 1]
         r   ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
         n1  n
         acc ""]

    (if (= 0 (count d))
      ;; if the digits are exhausted
      acc
      ;; otherwise, recur
      (let [div   (first d)
            times (quot n1 div) ; how many divisors are contained
            remdr (rem  n1 div)]; reminder for next iteration

        (recur (rest d) (rest r) remdr (str acc (apply str (repeat times (first r)))))))))

;; using a builtin function
(defn __ [x] (clojure.pprint/cl-format nil "~@R" x))

(= "I" (__ 1))
(= "XXX" (__ 30))
(= "IV" (__ 4))
(= "CXL" (__ 140))
(= "DCCCXXVII" (__ 827))
(= "MMMCMXCIX" (__ 3999))
(= "XLVIII" (__ 48))






;;; 4Clojure Question 92
;;
;; Roman numerals are easy to recognize, but not everyone knows all the rules necessary to work with them. Write a function to parse a Roman-numeral string and return the number it represents.
;;
;; <br /><br />
;;
;; You can assume that the input will be well-formed, in upper-case, and follow the <a href="http://en.wikipedia.org/wiki/Roman_numerals#Subtractive_principle">subtractive principle</a>. You don't need to handle any numbers greater than MMMCMXCIX (3999), the largest number representable with ordinary letters.
;;
;; Use M-x 4clojure-check-answers when you're done!


(def roman {"I" 1, "IV" 4, "V" 5, "IX" 9, "X" 10, "XL" 40, "L" 50, "XC" 90, "C" 100, "CD" 400, "D" 500, "CM" 900, "M" 1000})
(roman "V")

(def roman-combo #{"CM" "CD" "XC" "XL" "IX" "IV"})
[900 400 90 40 9 4]

"M M M CM XC IX"

;; take 4's and 9's first?
(re-find #"CM" "MMMCMXCIX")

;; Replace CM
(if (re-find #"CM" "MMMCMXCIX")
  (clojure.string/replace "MMMCMXCIX" "CM" "")
  "MMMCMXCIX")


;; this one calculate the 9's and 4's only
(defn __ [st]
  (loop [s st
         r [#"CM" #"CD" #"XC" #"XL" #"IX" #"IV"]
         n [900  400   90   40    9    4]
         acc 0]

    (if (= 0 (count r))
      acc

      (if (re-find (first r) s)
        (recur (clojure.string/replace s (re-pattern (first r)) "") (rest r) (rest n) (+ acc (first n)))
        (recur s (rest r) (rest n) acc)
        )
      )
    ))
(__ "MMMCMXCIX")

;; just return the first step results. both number and the remaining letters
(defn __ [st]
  (let [[s acc] (loop [s st
                       r [#"CM" #"CD" #"XC" #"XL" #"IX" #"IV"]
                       n [900  400   90   40    9    4]
                       acc 0]

                  (if (= 0 (count r))
                    [s acc]

                    (if (re-find (first r) s)
                      (recur (clojure.string/replace s (re-pattern (first r)) "") (rest r) (rest n) (+ acc (first n)))
                      (recur s (rest r) (rest n) acc)
                      )
                    )
                  )]

    [s acc]
    ))
(__ "MMMCMXCIX")


;; preliminary version that works
(defn __ [st]
  ;; first step is assignment to [s acc]
  (let [[s acc] (loop [s   st
                       r   [#"CM" #"CD" #"XC" #"XL" #"IX" #"IV"]
                       n   [900  400   90   40    9    4]
                       acc 0]

                  ;; Loop until all regex are used
                  (if (= 0 (count r))
                    ;; Return the remaining string and accumulated number
                    [s acc]
                    ;; Otherwise, check if there is a match, and add corresponding number
                    (if (re-find (first r) s)
                      (recur (clojure.string/replace s (re-pattern (first r)) "") (rest r) (rest n) (+ acc (first n)))
                      (recur s (rest r) (rest n) acc)
                      )))]

    ;; 2nd step using the first step results
    (loop [s2   s
           r2   [#"M" #"D" #"C" #"L" #"X" #"V" #"I"]
           n2   [1000 500 100 50 10 5 1]
           acc2 acc]

      (if (= 0 (count r2))
        acc2

        (if-let [match-seq (re-seq (first r2) s2)]
          (recur (clojure.string/replace s2 (re-pattern (first r2)) "") (rest r2) (rest n2) (+ acc2 (* (count match-seq) (first n2))))
          (recur s2 (rest r2) (rest n2) acc2)
          )))))

(__ "MMMCMXCIX")


;; Using only the second loop in the previous one
(defn __ [st]
  ;; first step is assignment to [s acc]
  (loop [r   [#"CM" #"CD" #"XC" #"XL" #"IX" #"IV" #"M" #"D" #"C" #"L" #"X" #"V" #"I"]
         n   [900 400 90 40 9 4 1000 500 100 50 10 5 1]
         s   st
         acc 0]

    ;; Loop until all regex are used
    (if (= 0 (count r))
      ;; If exhausted, return the accumulated number
      acc
      ;; Otherwise, check if there are matches, and add corresponding number
      (if-let [match-seq (re-seq (first r) s)]
        ;; if there are matches, remove the matching parts, add correpsonding number to accumulator
        (recur (rest r) (rest n) (clojure.string/replace s (re-pattern (first r)) "")
               (+ acc (* (count match-seq) (first n))))
        ;; if there are no matches, move on without changing the string or adding number
        (recur (rest r) (rest n) s
               acc)))))

;; without comments
(defn __ [st]
  (loop [r   [#"CM" #"CD" #"XC" #"XL" #"IX" #"IV" #"M" #"D" #"C" #"L" #"X" #"V" #"I"]
         n   [900 400 90 40 9 4 1000 500 100 50 10 5 1]
         s   st
         acc 0]
    (if (= 0 (count r))
      acc
      (if-let [match-seq (re-seq (first r) s)]
        (recur (rest r) (rest n) (clojure.string/replace s (re-pattern (first r)) "")
               (+ acc (* (count match-seq) (first n))))
        (recur (rest r) (rest n) s
               acc)))))

;; without two recur's
(defn __ [st]
  (loop [r   [#"CM" #"CD" #"XC" #"XL" #"IX" #"IV" #"M" #"D" #"C" #"L" #"X" #"V" #"I"]
         n   [900 400 90 40 9 4 1000 500 100 50 10 5 1]
         s   st
         acc 0]
    (if (= 0 (count r))
      acc
      (recur (rest r) (rest n)
             ;; remove the matching parts from s
             (clojure.string/replace s (re-pattern (first r)) "")
             ;; add number corresponding to number of matches
             (+ acc (* (count (re-seq (first r) s)) (first n)))))))
(__ "MMMCMXCIX")

;;
(defn __ [x]
  (let
    [R {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
    (reduce +
      (map
        (partial reduce #(- (R %2) %1) 0)
        (re-seq #"IV|IX|XL|XC|XM|CD|CM|[IVXLCDM]" x)))))

;; Returns a lazy sequence of successive matches
(re-seq #"IV|IX|XL|XC|XM|CD|CM|[IVXLCDM]" "MMMCMXCIX")
;; ("M" "M" "M" "CM" "XC" "IX")

(map
 (partial reduce #(- ({\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} %2) %1) 0)
 (re-seq #"IV|IX|XL|XC|XM|CD|CM|[IVXLCDM]" "MMMCMXCIX"))

(= 14 (__ "XIV"))
(= 827 (__ "DCCCXXVII"))
(= 3999 (__ "MMMCMXCIX"))
(= 48 (__ "XLVIII"))



;;; 4Clojure Question 128
;;
;; <p>A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs - and thirteen cards in each suit. Two is the lowest rank, followed by other integers up to ten; then the jack, queen, king, and ace.</p>
;;
;;
;;
;; <p>It's convenient for humans to represent these cards as suit/rank pairs, such as H5 or DQ: the heart five and diamond queen respectively. But these forms are not convenient for programmers, so to write a card game you need some way to parse an input string into meaningful components. For purposes of determining rank, we will define the cards to be valued from 0 (the two) to 12 (the ace)</p>
;;
;;
;;
;; <p>Write a function which converts (for example) the string "SJ" into a map of <code>{:suit :spade, :rank 9}</code>. A ten will always be represented with the single character "T", rather than the two characters "10".</p>
;;
;; Use M-x 4clojure-check-answers when you're done!

(interleave (map str (range 2 10)) (range 0 10))

(zipmap "DHCTJQKA" [:diamond :heart :club 8 9 10 11 12])
(zipmap (map identity "23456789TJQKA") (range 13))
(zipmap (seq "23456789TJQKA") (range 13))

(defn __ [s]
  (let [suit-letter (str (first s))
        rank-letter (str (second s))
        suits {"D" :diamond, "H" :heart, "C" :club, "S" :spade}
        ranks {"2" 0 "3" 1 "4" 2 "5" 3 "6" 4 "7" 5 "8" 6 "9" 7 "T" 8 "J" 9 "Q" 10 "K" 11 "A" 12}]
    ;;
    {:suit (get suits suit-letter), :rank (get ranks rank-letter)}))

(defn __ [s]
  (let [suit-letter (first s)
        rank-letter (second s)
        suits {\D :diamond, \H :heart, \C :club, \S :spade}
        ranks (zipmap (seq "23456789TJQKA") (range 13))]
    ;;
    {:suit (get suits suit-letter), :rank (get ranks rank-letter)}))


(= {:suit :diamond :rank 10} (__ "DQ"))
(= {:suit :heart :rank 3} (__ "H5"))
(= {:suit :club :rank 12} (__ "CA"))
(= (range 13) (map (comp :rank __ str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))




;;; 4Clojure Question 96
;;
;; Let us define a binary tree as "symmetric" if the left half of the tree is the mirror image of the right half of the tree.  Write a predicate to determine whether or not a given binary tree is symmetric. (see <a href='/problem/95'>To Tree, or not to Tree</a> for a reminder on the tree representation we're using).
;;
;; Use M-x 4clojure-check-answers when you're done!

;; divide at level 1, recursively sort, and compare after sorting?
[1
 [2
  nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
 [2
  nil [3 [4 [5 nil nil] [6 nil nil]] nil]]]


(def tree-sort (fn tree-sort [x]
                 (cond
                  ;; leave terminal node alone
                  ((complement coll?) x) [x]
                  ;; if it is a collection node, with terminals only, sort it by first element
                  (and (coll? x) (every? (complement coll?) x)) (sort x)
                  ;; Otherwise recur
                  :else (for [node     x
                              solution [(tree-sort node)]]
                          solution))))

(tree-sort '(:a (:b nil nil) (:b nil nil)))
(tree-sort '(1 [:b nil nil] [[:c nil nil] nil]))
(tree-sort [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
            [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
(sort-by #(first %) [[4] [6 nil nil] [5 nil nil]])

;; sig: seq of seq -> bool
;; purpose check for symmetry
;;
;; (defn __ [tree]
;;   nil)
;;
(defn __ [tree]
  (let [a (second tree)
        b (last   tree)
        ;; recuring function
        tree-sort (fn tree-sort [x]
                    (cond
                     ;; leave terminal node alone
                     ((complement coll?) x) [x]
                     ;; if it is a collection node, with terminals only,
                     (and (coll? x) (every? (complement coll?) x)) (reverse (sort x))
                     ;; Otherwise recur
                     ;; sort the node with collections first.
                     ;; sort it by first element (coll) or element itself
                     :else (for [node     (sort-by #(if (coll? %) (first %) %) x)
                                 solution [(tree-sort (node))]]
                             solution)))]
    ;;
    [(tree-sort a) (tree-sort b)]
    ;; (= (tree-sort a) (tree-sort b))
    ))
;;
(sort-by #(if (coll? %) (first %) %) '(:a (:c nil nil) :d (:b nil nil) ))

;;
(defn __ [tree]
  (let [a (first  tree)
        b (second tree)
        c (last   tree)
        ;; recuring function
        tree-sort (fn tree-sort [x]
                    (cond
                     ;;
                     ;; leave terminal node (one element, not a collection) alone
                     ((complement coll?) x) x
                     ;; if it is a collection node, with terminal nodes only,
                     ;; reverse sort
                     (and (coll? x) (every? (complement coll?) x)) (reverse (sort x))
                     ;; Otherwise recur
                     ;; sort the node with collections first.
                     ;; sort it by first element (coll) or element itself
                     :else (for [node     (reverse (sort-by #(if (coll? %) (first %) %) x))
                                 solution [(tree-sort node)]]
                             solution)))]
    ;; return sorted list
    [a (tree-sort b) (tree-sort c)]
    ;; check for balance. a is not needed
    ;; (= (tree-sort b) (tree-sort c))
    ))

(__ '(:a (:b (:c nil nil) nil) (:b nil nil)))

;; comparison strategy. wrong
(defn __ [tree]

  (cond
   ;; if at the terminal, check second and third elements for equality
   (and (coll? tree) (every? (complement coll?) tree)) (= (second tree) (last tree))
   ;;
        ))


;;
(defn __ [tree]
  (let [a (first  tree)
        b (second tree)
        c (last   tree)
        ;; recuring function
        tree-rev (fn tree-rev [x]
                   (cond
                    ;;
                    ;; if it is an element, not a collection, leave it alone
                    ((complement coll?) x) x
                    ;; if it is a collection node with terminal elements only, leave it alone
                    (and (coll? x) (every? (complement coll?) x)) x
                    ;; Otherwise (collection of at least one collection node)
                    ;; reverse 2nd and 3rd elements
                    ;; sort it by first element (coll) or element itself
                    :else (for [node     [(first x) (last x) (second x)]
                                solution [(tree-rev node)]]
                            solution)))]
    ;; return sorted list
    ;; [a b (tree-rev c)]
    ;; check for balance. a is not needed
    (= b (tree-rev c))
    ))

;; simpler recursion
(defn __ [tree]
  (let [a (first  tree)
        b (second tree)
        c (last   tree)
        ;; recuring function
        tree-rev (fn tree-rev [a b c]
                   [a
                    ;; c second
                    (if (coll? c)
                      tree-rev c
                      c)
                    ;; b third
                    (if (coll? b)
                      tree-rev b
                      b)])]
    ;;
    (= b (tree-rev c))))


;; 0x89
(deffn __ [x]
  (let [mirrored (fn mirrored [t]
                   (if (sequential? t)
                     ;; if it is a sequence reverse left and right and recur
                     (let [h (first t)
                           l (second t)
                           r (nth t 2)]
                       [h (mirrored r) (mirrored l)])
                     ;; if not a sequence, leave it alone
                     t))
        ;;
        l (second x)
        r (nth x 2)]
    ;;
    (= l (mirrored r))))

;; pcl
(defn __ [[_ L R]]
  (letfn
      [(flip [[v l r]]
         (list v
               (if (coll? r) (flip r) r)
               (if (coll? l) (flip l) l)))]
    ;
    (= L (flip R))))

[1
 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
 (2 (3 nil (4 [6 nil nil] [5 nil nil])) nil)]

(= (__ '(:a (:b nil nil) (:b nil nil))) true)
(= (__ '(:a (:b nil nil) nil)) false)
(= (__ '(:a (:b nil nil) (:c nil nil))) false)
(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)
;; this is true if sorted, but not true if unsorted
(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)
(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)




;;; 4Clojure Question 153
;;
;; Given a set of sets, create a function which returns <code>true</code>
;;
;; if no two of those sets have any elements in common<sup>1</sup> and <code>false</code> otherwise.
;;
;; Some of the test cases are a bit tricky, so pay a little more attention to them.
;; <sup>1</sup>Such sets are usually called <i>pairwise disjoint</i> or <i>mutually disjoint</i>.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a-set]
  (->> (for [x a-set
             y (disj a-set x)]
         (clojure.set/intersection x y)) ; all possible pairwise intersection
       ;; true if all empty
       (every? empty?,  )))


;; actually can just open it up and check element-wise
(apply distinct? (apply concat #{#{:a :b :c :d :e}
                                  #{:a :b :c :d}
                                  #{:a :b :c}
                                  #{:a :b}
                                  #{:a}}))

(= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
   true)
(= (__ #{#{:a :b :c :d :e}
         #{:a :b :c :d}
         #{:a :b :c}
         #{:a :b}
         #{:a}})
   false)
(= (__ #{#{[1 2 3] [4 5]}
         #{[1 2] [3 4 5]}
         #{[1] [2] 3 4 5}
         #{1 2 [3 4] [5]}})
   true)
(= (__ #{#{'a 'b}
         #{'c 'd 'e}
         #{'f 'g 'h 'i}
         #{''a ''c ''f}})
   true)
(= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
         #{#{:x :y :z} #{:x :y} #{:z} #{}}
         #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false)
(= (__ #{#{(= "true") false}
         #{:yes :no}
         #{(class 1) 0}
         #{(symbol "true") 'false}
         #{(keyword "yes") ::no}
         #{(class '1) (int \0)}})
   false)
(= (__ #{#{distinct?}
         #{#(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}})
   true)
(= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
         #{'+ '* mapcat (comment mapcat)}
         #{(do) set contains? nil?}
         #{, , , #_, , empty?}})
   false)




;;; 4Clojure Question 146
;;
;; <p>Because Clojure's <code>for</code> macro allows you to "walk" over multiple sequences in a nested fashion, it is excellent for transforming all sorts of sequences. If you don't want a sequence as your final output (say you want a map), you are often still best-off using <code>for</code>, because you can produce a sequence and feed it into a map, for example.</p>
;;
;; <p>For this problem, your goal is to "flatten" a map of hashmaps. Each key in your output map should be the "path"<sup>1</sup> that you would have to take in the original map to get to a value, so for example <code>{1 {2 3}}</code> should result in <code>{[1 2] 3}</code>. You only need to flatten one level of maps: if one of the values is a map, just leave it alone.</p>
;;
;; <p><sup>1</sup> That is, <code>(get-in original [k1 k2])</code> should be the same as <code>(get result [k1 k2])</code></p>
;;
;; Use M-x 4clojure-check-answers when you're done!

;; sig: map -> map
;; purpose: flatten a map with a sequential key
;; This recursive version is not restricted to two levels
(defn __
  ;; body 1: user function (merge seq of maps; only worked here)
  ([amap] (reduce merge (__ amap [])))
  ;; body 2: helper function
  ([amap acc]
     (cond
      ;; if it is not a map, return it                      ; this vector is opened up by solution binding
      (not (= (type amap) clojure.lang.PersistentArrayMap)) [(hash-map acc amap)]
      ;;
      :else (for [node     amap
                  solution (__ (val node) (conj acc (key node)))]
              ;;
              solution))))


;; (zipmap [k1 k2 k3] [v1 v2 v3])
;; (hash-map k1 v1 k2 v2)
(reduce #(merge %1 %2) (map #(zipmap [(first %)] [(second %)]) '([[a p] 1] [[a q] 2] [[b m] 3] [[b n] 4])))

(= (__ '{a {p 1, q 2}
         b {m 3, n 4}})
   '{[a p] 1, [a q] 2
     [b m] 3, [b n] 4})
(= (__ '{[1] {a b c d}
         [2] {q r s t u v w x}})
   '{[[1] a] b, [[1] c] d,
     [[2] q] r, [[2] s] t,
     [[2] u] v, [[2] w] x})
(= (__ '{m {1 [a b c] 3 nil}})
   '{[m 1] [a b c], [m 3] nil})

(defn __ [x] (into {}
                   (for
                       [[k v] x
                        [k2 v2] v]
                     [[k k2] v2])))

(__ '{a {p 1, q 2}
      b {m {3 2}, n 4}})


;;; 4Clojure Question 95
;;
;; Write a predicate which checks whether or not a given sequence represents a <a href="http://en.wikipedia.org/wiki/Binary_tree">binary tree</a>.  Each node in the tree must have a value, a left child, and a right child.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [tree1]
  (if (not (= 3 (count tree1)))
    false
    (let [coll-nodes (filter coll? tree1)]
      (if (empty? coll-nodes)
        true
        (map __ coll-nodes)))))

;; for macro and recursion
(defn __ [tree1]
  (if (not (and
            ;; has three elements
            (= 3 (count tree1))
            ;; AND the first one is not a collection
            ((complement coll?) (first tree1))))
    ;; if either is not met
    [false]
    ;; if both are met, check if terminal (all elements are not collection)
    (if (every? (complement coll?) tree1)
      ;; if it is terminal
      [true]
      ;;
      ;; otherwise, pick one solution to follow
      (for [coll-node (filter coll? tree1)
            solution  (__ coll-node)]
        solution))))

;; for macro and recursion
(defn __ [tree]
  (cond
   ;; if not three element, bad
   (not= 3 (count tree)) [false]
   ;; if first is collection, bad
   (coll? (first tree))  [false]
   ;; if remaing two are not collection or nil, bad
   (some (fn [x] (not (or (nil? x) (coll? x)))) (rest tree)) [false]
   ;; if remaining two are both nil, good
   (every? (fn [x] (nil? x)) (rest tree)) [true]
   ;;
   ;; Otherwise, check for collection nodes
   :else (for [node     (filter coll? (rest tree))
               solution (__ node)]
           solution)))


;; accumulator
(defn __
  ([tree] (every? identity (__ tree '())))
  ;;
  ([tree acc] (cond
               ;; if not three element, bad
               (not= 3 (count tree)) '(false)
               ;; if first is collection, bad
               (coll? (first tree))  '(false)
               ;; if remaing two are not collection or nil, bad
               (some (fn [x] (not (or (nil? x) (coll? x)))) (rest tree)) '(false)
               ;; if remaining two are both nil, good
               (every? (fn [x] (nil? x)) (rest tree)) '(true)
               ;;
               ;; Otherwise, check for collection nodes
               :else (for [node     (filter coll? (rest tree))
                           solution (__ node (conj acc true))]
                       solution))))

;; 0x89's solution:
(defn tree? [x]
  (or
   ;; a node has to be nil
   (nil? x)
   ;; or all of these
   (and
    (sequential? x)
    (= 3 (count x))
    (not (sequential? (first x))) ; first element is not sequence
    (tree? (second x)) ; 2nd element is also meet these criteria
    (tree? (nth x 2))  ; 3rd element is also meet these criteria
    )))

(= (__ '(:a (:b nil nil) nil))
   true)
(= (__ '(:a (:b nil nil)))
   false)
(= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)
(= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)
(= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)
(= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
   false)
(= (__ '(:a nil ()))
   false)




;;; 4Clojure Question 70
;;
;; Write a function that splits a sentence up into a sorted list of words.  Capitalization should not affect sort order and punctuation should be ignored.
;;
;; Use M-x 4clojure-check-answers when you're done!

;; sig: string -> seq of string
;; purpose: sort words
(defn __ [s]
  (let [seq-strings       (->> (clojure.string/replace s #"[.!]" "")
                               (#(clojure.string/split % #" "),  ))
        ;; lower case
        seq-strings-lower (map clojure.string/lower-case seq-strings)
        ;; how they correspond
        str-map           (zipmap seq-strings-lower seq-strings)]
    ;; sort lower case ones, and map the originals
    (map #(get str-map %) (sort seq-strings-lower))))

;; heavy Java interop
(defn __ [x] (sort-by #(.toLowerCase %) (.split (.replaceAll x "[^a-zA-Z ]" "") " ")))

(.replaceAll "Have a nice day.!!!!" "[^a-zA-Z ]" "")
(.split (.replaceAll "Have a nice day.!!!!" "[^a-zA-Z ]" "") " ")
(sort-by #(.toLowerCase %) (.split (.replaceAll "Have a nice day.!!!!" "[^a-zA-Z ]" "") " "))

;; sort-by
(defn __ [s]
  (let [seq-strings (->> (clojure.string/replace s #"[.!]" "")
                         (#(clojure.string/split % #" "),  ))]
    ;; sort by lower case representations
    (sort-by #(clojure.string/lower-case %) seq-strings)))

(= (__  "Have a nice day.")
   ["a" "day" "Have" "nice"])
(= (__  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])
(= (__  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])



;;; 4Clojure Question 76
;;
;; The trampoline function takes a function f and a variable number of parameters.  Trampoline calls f with any parameters that were supplied.  If f returns a function, trampoline calls that function with no arguments.  This is repeated, until the return value is not a function, and then trampoline returns that non-function value.  This is useful for implementing mutually recursive algorithms in a way that won't consume the stack.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= __
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))


(letfn
    [(foo [x y] #(bar (conj x y) y))
     (bar [x y] (if (> (last x) 10)
                  x
                  #(foo x (+ 2 y))))]
  (trampoline foo [] 1))



;;; 4Clojure Question 67
;;
;; Write a function which returns the first x
;;
;; number of prime numbers.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n]
  (loop [lazy-num (drop 3 (range))
         i         1
         acc      [2]]
    ;;
    (cond
     ;; if enough numbers have accumulated, return acc
     (= i n) acc
     ;; if current number is not divisible by all numbers in acc, it is a prime
     (every? #((complement zero?) (rem (first lazy-num) %)) acc) (recur (rest lazy-num)
                                                                        (inc i)
                                                                        (conj acc (first lazy-num)))
     ;; Otherwise, just move on
     :else (recur (rest lazy-num) i acc))))

(= (__ 2) [2 3])
(= (__ 5) [2 3 5 7 11])
(= (last (__ 100)) 541)



;;; 4Clojure Question 74
;;
;; Given a string of comma separated integers, write a function which returns a new comma separated string that only contains the numbers which are perfect squares.
;;
;; Use M-x 4clojure-check-answers when you're done!

;; sig: string -> string
;; purpose keep only the perfect squares
;; stub:
;; (defn __ [s]
;;   "0")
;;
(defn __ [s]
  (let [nums (map #(Integer/parseInt %) (clojure.string/split s #","))]
    ;; Filter for perfect square, and then back to string
    (->> nums
         (filter #(zero? (mod (Math/sqrt %) 1)),  )
         (interpose ","                        ,  )
         (apply str                            ,  ))))

(= (__ "4,5,6,7,8,9") "4,9")
(= (__ "15,16,25,36,37") "16,25,36")



;;; 4Clojure Question 69
;;
;; Write a function which takes a function f and a variable number of maps.  Your function should return a map that consists of the rest of the maps conj-ed onto the first.  If a key occurs in more than one map, the mapping(s) from the latter (left-to-right) should be combined with the mapping in the result by calling (f val-in-result val-in-latter)
;;
;; Restrictions (please don't use these function(s)): merge-with
;;
;; Use M-x 4clojure-check-answers when you're done!

;; sig: f maps -> map
;; purpose merge with a function
;; stub
;; (defn __ [f & maps]
;;   {})
;;
(defn __ [f & maps]
  (let [my-merge-with (fn [f m1 m2]
                        (loop [keys2 (keys m2)
                               acc   m1]
                          ;;
                          (let [fst-keys2 (first keys2)]
                            (cond
                             ;; if all keys are worked on, return acc
                             (empty? keys2) acc
                             ;; If m1 has the same key, merge-with f
                             (contains? m1 fst-keys2) (recur (rest keys2)
                                                             (assoc acc fst-keys2(f (get m1 fst-keys2)
                                                                                    (get m2 fst-keys2))))
                             ;; If m1 has no such key, just merge
                             :else (recur (rest keys2) (assoc acc fst-keys2 (get m2 fst-keys2)))))))]

    (reduce #(my-merge-with f %1 %2) (first maps) (rest maps))))

(= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})
(= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})
(= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})



;;; 4Clojure Question 85
;;
;; Write a function which generates the <a href="http://en.wikipedia.org/wiki/Power_set">power set</a> of a given set.  The power set of a set x is the set of all subsets of x, including the empty set and x itself.
;;
;; Use M-x 4clojure-check-answers when you're done!
;;
;; signature: set -> set of sets
;; purpose: powerset of a given set
;; stub:
;; (defn __ [a-set]
;;   #{})
;;
;; http://rosettacode.org/wiki/Power_set#R
(defn __ [aset]
  (->> (loop [;; count down from the number of elements in aset
              counter (count aset)
              ;; acc start as list of an empty set
              acc #{#{}}]
         ;; stop and return acc
         (if (zero? counter)
           acc
           (recur (dec counter) (set ; make acc a set to reduce the number of elements
                                 (concat acc
                                         ;; Add to acc, all possible one element addition to acc elements
                                         ;; like (#{1} #{:a}) -> (#{1 1} #{:a 1} #{1 :a} #{:a :a})
                                         ;; duplications are removed
                                         (flatten (for [x aset]
                                                    (map #(conj % x) acc))))))))
       ;; Put elements after the recursion into a set
       (into #{},  )))


(= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
(= (__ #{}) #{#{}})
(= (__ #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
(= (count (__ (into #{} (range 10)))) 1024)


;; http://rosettacode.org/wiki/Power_set#Clojure
(defn powerset
  [coll]
  (reduce
   ;; This function is used for each element in coll
   (fn [a  ; set of sets to add to
        x] ; element from coll
     (->> a
          ;; concatenate element x with each elemental set within a (return as a set)
          (map #(set (concat #{x} %)) ,  )
          ;; add to the original set of sets
          (concat a                   ,  )
          ;; make everything a set
          (set                        ,  )))
   ;; initial value for reduce
   #{#{}}
   ;; collection to work on
   coll))

(powerset #{1 2 3})



;;; 4Clojure Question 73
;;
;; A <a href="http://en.wikipedia.org/wiki/Tic-tac-toe">tic-tac-toe</a> board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, and empty is represented by :e.  A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row.  Write a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither player has won.
;;
;; Use M-x 4clojure-check-answers when you're done!

;; sig: vector of vectors -> bool or nil
;; purpose check the board for winning pattern
;; stub
;; (defn __ [board]
;;   :nil)
;;
(def board1 [[:e :e :e]
             [:e :e :e]
             [:e :e :e]])
(def board2 [[:x :e :o]
             [:x :e :e]
             [:x :e :o]])
(def board3 [[:e :x :e]
             [:o :o :o]
             [:x :e :x]])
(def board4 [[:x :e :e]
             [:o :x :e]
             [:o :e :x]])
(defn row [board]
  (let [row-value-set (set (filter #(= 1 (count %)) (map (fn [x] (distinct x)) board)))]
    (cond
     (contains? row-value-set '(:o)) :o
     (contains? row-value-set '(:x)) :x
     :else                           nil)))
(row board1) ; none
(row board2) ; col win
(row board3) ; row win


(defn col [board]
  (let [board-transposed (apply map vector board)]
    (row board-transposed)))
(col board1) ; none
(col board2) ; col win
(col board3) ; row win

(defn diag [board]
  (let [diag1 [(get-in board [0 0])
               (get-in board [1 1])
               (get-in board [2 2])]
        diag2 [(get-in board [0 2])
               (get-in board [1 1])
               (get-in board [2 0])]]
    (row [diag1 diag2])))
(diag board4)
;;
;; Using named functions
(defn __ [board]
  (let [row (row board)
        col (col board)
        diag (diag board)]
    (first (filter (complement nil?) [row col diag]))))
;;

(defn __ [board]
  ;; define a function for row assessment
  (let [row-fun (fn [board]
                  (let [row-value-set (set (filter #(= 1 (count %)) (map (fn [x] (distinct x)) board)))]
                    (cond
                     (contains? row-value-set '(:o)) :o
                     (contains? row-value-set '(:x)) :x
                     :else                           nil)))
        ;; Get values for each one rows, cols, and diags using row-fun
        row  (row-fun board)
        col  (row-fun (apply map vector board))
        diag (let [diag1 [(get-in board [0 0])
                          (get-in board [1 1])
                          (get-in board [2 2])]
                   diag2 [(get-in board [0 2])
                          (get-in board [1 1])
                          (get-in board [2 0])]]
               (row-fun [diag1 diag2]))]
    ;; Check for non-nil values, (first '()) will result in nil
    (first (filter (complement nil?) [row col diag]))))
;;
;; as one function


;;

(= nil (__ [[:e :e :e]
            [:e :e :e]
            [:e :e :e]]))
(= :x (__ [[:x :e :o]
           [:x :e :e]
           [:x :e :o]]))
(= :o (__ [[:e :x :e]
           [:o :o :o]
           [:x :e :x]]))
(= nil (__ [[:x :e :o]
            [:x :x :e]
            [:o :x :o]]))
(= :x (__ [[:x :e :e]
           [:o :x :e]
           [:o :e :x]]))
(= :o (__ [[:x :e :o]
           [:x :o :e]
           [:o :e :x]]))
(= nil (__ [[:x :o :x]
            [:x :o :x]
            [:o :x :o]]))





;;; 4Clojure Question 115
;;
;; A balanced number is one whose component digits have the same sum on the left and right halves of the number.  Write a function which accepts an integer n, and returns true iff n is balanced.
;;
;; Use M-x 4clojure-check-answers when you're done!
;;
;; sig: number -> bool
;; purpose check for balancing
;; stub:
;; (defn __ [n]
;;   nil)
;;
(defn __ [n]
  (let [n-as-str    (str n)
        half-length (quot (count n-as-str) 2)
        left        (map str (take half-length n-as-str))
        right       (map str (take half-length (reverse n-as-str)))
        sum-as-int  (fn [sq] (reduce + (map #(Integer/parseInt %) sq)))]
    ;;
    (= (sum-as-int left) (sum-as-int right))))

;; take-last is available
(defn __ [n]
  (let [n-as-str    (str n)
        half-length (quot (count n-as-str) 2)
        left        (map str (take      half-length n-as-str))
        right       (map str (take-last half-length n-as-str))
        sum-as-int  (fn [sq] (reduce + (map #(Integer/parseInt %) sq)))]
    ;;
    (= (sum-as-int left) (sum-as-int right))))
;;
(= true (__ 11))
(= true (__ 121))
(= false (__ 123))
(= true (__ 0))
(= false (__ 88099))
(= true (__ 89098))
(= true (__ 89089))
(= (take 20 (filter __ (range)))
   [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])





;;; 4Clojure Question 137
;;
;; Write a function which returns a sequence of digits of a non-negative number (first argument) in numerical system with an arbitrary base (second argument). Digits should be represented with their integer values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16.
;;
;; Use M-x 4clojure-check-answers when you're done!
;; sig: number, number -> vector
;; purpose create a number in base x
;; stub:
;; (defn __ [n base]
;;   [0 0 0])
;;
(defn __ [n base]
  (if (zero? n)
    [0]
    ;; obtain the largest base^x that is smaller than n
    (let [max-possible-divisor (loop [div 1]
                                 (cond
                                  (= n div) div         ; necessary for (__ 8 8) etc
                                  (< n div) (/ div base)
                                  :else     (recur (* div base))))]
      ;;
      (loop [div       max-possible-divisor ; start from the biggest
             remaining n
             acc       []]
        (if (and (< div 1))
          acc
          (recur (/ div base) (- remaining (* div (quot remaining div))) (conj acc (quot remaining div))))))))
;;
;; 0x89's solution:
;; start from divisor base, the divide the quotient by base (next step), ...
(defn it
  ;; body 1
  ([n base] (it n base []))
  ;; body 2 for tail recursion
  ([n base res]
     (let [q    (quot n base)
           r    (rem  n base)
           nres (cons r res)]
       ;;
       (if (zero? q)
         nres
         (recur q base nres)))))

;;
(= [1 2 3 4 5 0 1] (__ 1234501 10))
(= [0] (__ 0 11))
(= [1 0 0 1] (__ 9 2))
(= [1 0] (let [n (rand-int 100000)](__ n n)))
(= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))



;;; 4Clojure Question 105
;;
;; Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword, and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!
;; sig: vector -> map
;; purpose transform a vector with keys and values to a map
;; stub:
;; (defn __ [v]
;;   {:nil nil})
;;
(defn __ [v]
  (loop [vector1 v
         map-all {}
         map-cur {}]
    (cond
     ;; if nothing left return map-all after merging map-cur
     (empty? vector1) (conj map-all map-cur)
     ;; if a keyword is encountered, conj map-cur to map-all, and create a new map
     (keyword? (first vector1)) (recur (rest vector1)
                                       (conj map-all map-cur)
                                       (hash-map (first vector1) []))
     ;; Otherwise, keep map-all as is, update map-cur by adding new element to a vector
     :else (recur (rest vector1)
                  map-all
                  (hash-map (first (keys map-cur)) (conj (first (vals map-cur)) (first vector1)))))))
;;
;;
(partition-by keyword? [:a 1 2 3 :b :c 4])
(split-with keyword? [:a 1 2 3 :b :c 4])

(= {} (__ []))
(= {:a [1]} (__ [:a 1]))
(= {:a [1], :b [2]} (__ [:a 1, :b 2]))
(= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))




;;; 4Clojure Question 60
;;
;; Write a function which behaves like reduce, but returns each intermediate value of the reduction.  Your function must accept either two or three arguments, and the return sequence must be lazy.
;;
;; Restrictions (please don't use these function(s)): reductions
;;
;; Use M-x 4clojure-check-answers when you're done!

;; sig: fun, initial val, seq -> seq
;; purpose reduction

;; reduce part
(defn __ [f & args]
  ;; obtain initial value, and the rest of the sequence
  (let [init (if (= 1 (count args))
               (first (first args))
               (first args))
        sq   (if (= 1 (count args))
               (rest (first args))
               (second args))]
    (reduce f init sq)))

;; first two steps only
(defn __ [f & args]
  ;; obtain initial value, and the rest of the sequence
  (let [init (if (= 1 (count args))
               (first (first args))
               (first args))
        sq   (if (= 1 (count args))
               (rest (first args))
               (second args))]
    [init (f init (first sq))]))

;; recursion
(defn __ [f & args]
  ;; obtain initial value, and the rest of the sequence
  (let [init (if (= 1 (count args))
               (first (first args))
               (first args))
        sq   (if (= 1 (count args))
               (rest (first args))
               (second args))
        fun  (fn fun [a b]
               (if (empty? b)
                 []
                 [(f a (first b))
                  (fun (f a (first b)) (rest b))]))]

    [init (fun init sq)]
    ))

;; non lazy version
(defn __ [f & args]
  ;; obtain initial value, and the rest of the sequence
  (let [;; initial value
        init (if (= 1 (count args))
               (first (first args))
               (first args))
        ;; the rest of the sequence
        sq   (if (= 1 (count args))
               (rest (first args))
               (second args))
        ;; function for recursion
        fun  (fn fun [a b]
               (loop [a1   a
                      b1   b
                      acc [a1]]
                 ;;
                 (let [first-b1 (first b1)]
                   (if (empty? b1)
                     acc
                     (recur (f a1 first-b1) (rest b1) (conj acc (f a1 first-b1)))))))]
    ;;
    (fun init sq)))


;; what we want using reductions
(defn __
  ([f args] (__ f (first args) (second args)))
  ([f init sq]
     (reductions f init sq)))


;; what we want
(take 5 (reductions + (range)))
(reductions conj [1] [2 3 4])
(reductions * 2 [3 4 5])

;; non lazy
(defn __
  ([f sq] (__ f (first sq) (rest sq)))
  ([f init sq]
     (if (empty? sq)
       [init]
       (cons init (__ f (f init (first sq)) (rest sq))))))

;; lazy
(defn __
  ([f sq] (__ f (first sq) (rest sq)))
  ([f init sq]
     (if (empty? sq)
       [init]
       (cons init (lazy-seq (__ f (f init (first sq)) (rest sq)))))))

;; lazy 2
(defn __
  ([f sq] (__ f (first sq) (rest sq)))
  ([f init sq]
     (if (empty? sq)
       [init]
       (lazy-seq (cons init (__ f (f init (first sq)) (rest sq)))))))


(__ conj [1] [2 3 4])
(__ * 2 [3 4 5])
(take 5 (__ + (range)))



(= (take 5 (__ + (range))) [0 1 3 6 10])
(= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)




;;; 4Clojure Question 75
;;
;; Two numbers are coprime if their greatest common divisor equals 1.  Euler's totient function f(x) is defined as the number of positive integers less than x which are coprime to x.  The special case f(1) equals 1.  Write a function which calculates Euler's totient function.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn gcd [a b]
  (let [min-ab (min a b)
        divs   (range (inc min-ab) 0 -1)]
    (first (filter #(= 0 (rem a %) (rem b %)) divs))))

(gcd 1 2)
(gcd 2 4)
(gcd 105 35)

(defn __ [n]
  ;; define greatest common divisor function
  (let [gcd (fn [a b]
              (let [min-ab (min a b)
                    divs   (range (inc min-ab) 0 -1)]
                (first (filter #(= 0 (rem a %) (rem b %)) divs))))]
    ;; use it 
    (if (= n 1)
      ;; special case for n = 1
      1
      ;; count integers < n that have gcd of 1 with number n
      (count (filter #(= 1 (gcd n %)) (range 1 n))))))

(= (__ 1) 1)
(= (__ 10) (count '(1 3 7 9)) 4)
(= (__ 40) 16)
(= (__ 99) 60)




;;; 4Clojure Question 86
;;
;; Happy numbers are positive integers that follow a particular formula: take each individual digit, square it, and then sum the squares to get a new number. Repeat with the new number and eventually, you might get to a number whose squared sum is 1. This is a happy number. An unhappy number (or sad number) is one that loops endlessly. Write a function that determines if a number is happy or not.
;;
;; Use M-x 4clojure-check-answers when you're done!


;; sequence including 4 is infinite

;; function to get the next number
(defn next-number [n]
  (let [seq-digits (map #(Integer/parseInt (str %)) (str n))]
    (reduce + (map #(* % %) seq-digits))))

(defn __ [n]
  ;; define next-number
  (let [next-number (fn [n]
                      (let [seq-digits (map #(Integer/parseInt (str %)) (str n))]
                        (reduce + (map #(* % %) seq-digits))))]
    ;; stop at either 4 (failure) or 1 (success)
    (cond
     (= n 4) false
     (= n 1) true
     :else (recur (next-number n)))))

;; check against numbers seen
(defn __ [n]
  ;; define next-number
  (let [next-number (fn [n]
                      (let [seq-digits (map #(Integer/parseInt (str %)) (str n))]
                        (reduce + (map #(* % %) seq-digits))))]
    
    (loop [seen #{} ; set of numbers seen
           num  n]
      (println (str num))
      (cond
       (contains? seen num) false
       (= num 1)            true
       :else                (recur (conj seen num) (next-number num))))))


(= (__ 7) true)
(= (__ 986543210) true)
(= (__ 2) false)
(= (__ 3) false)




;;; 4Clojure Question 93
;;
;; Write a function which flattens any nested combination of sequential things (lists, vectors, etc.), but maintains the lowest level sequential items.  The result should be a sequence of sequences with only one level of nesting.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [sq]
  (let [my-flatten (fn my-flatten [sq]
                     (if (every? #((complement coll?) %) sq)
                       ;; if all elements are non-collection, return as is
                       sq
                       ;; otherwise, map recursion to each element
                       (map #(my-flatten %) sq)))]
    (map my-flatten sq)))


(defn __ [sq]
  (let [my-flatten (fn my-flatten [sq]
                     (cond
                      ;; if it is an element return as is
                      (not (coll? sq)) sq
                      ;; if it is a collection and there is only one element return flattened
                      ;; (and (= 1 (count sq)) ) (flatten sq)
                      ;; ;; if it is a collection and all its elements are collections
                      ;; (every? coll? sq) (map flatten sq)
                      ;; otherwise, map recursion to each element
                      :else (map #(my-flatten %) sq)))]
    ;;
    (my-flatten sq)))


;; for macro
(defn __ [sq]
  (let [my-flatten (fn my-flatten [sq]
                     (cond
                      ;; if it is an element return as is
                      (not (coll? sq)) sq
                      ;; if it is a collection of non-collections only
                      (every? #((complement coll?) %) sq) [sq]
                      ;; if it is a collection and there is only one element return flattened
                      (= 1 (count sq))  (first sq)
                      ;; ;; if it is a collection and all its elements are collections
                      ;; (every? coll? sq) (map flatten sq)
                      ;; otherwise, map recursion to each element
                      :else (map #(my-flatten %) sq)))]
    ;;
    (for [elt      sq
          solution (my-flatten elt)]
      solution)))


;;
(defn __ [sq]
  (let [my-flatten (fn my-flatten [sq]
                     (cond
                      ;; if it is an element return as is
                      (not (coll? sq)) sq
                      ;; if it is a collection of non-collections only
                      (every? #((complement coll?) %) sq) sq
                      ;; if it is a collection and there is only one element return flattened
                      (= 1 (count sq))  (first sq)
                      ;; ;; if it is a collection and all its elements are collections
                      (every? coll? sq) (map flatten sq)
                      ;; otherwise, map recursion to each element
                      :else (map #(my-flatten %) sq)))]
    ;;
    (my-flatten sq)))


;; sig: coll -> bool
;; purpose: check if there is only one elemental collection inside
;;
(defn only-one-level [coll]
  (cond
   ;; if all elements are non-collections, true
   (every? #((complement coll?) %) coll) true
   ;; if there are non-collection elements and there are multiple elements, false
   (> 1 (count coll)) false
   ;; if there is only one collection element, recur inside
   :else (recur (first coll))))

(only-one-level [:a :b])
(only-one-level [[:a :b]])
(only-one-level [[:a :b] :c])


(first (first [[[:a :b]]]))

(__ '(((3 4) ((((5 6)))) ) ))

(__ '((1 2)((3 4)((((:a :b) (5 6)))))))


(= (__ [["Do"] ["Nothing"]])
   [["Do"] ["Nothing"]])

(= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
   [[:a :b] [:c :d] [:e :f]])

(= (__ '((1 2)((3 4)((((5 6)))))))
   '((1 2)(3 4)(5 6)))




;;; 4Clojure Question 158
;;
;; Write a function that accepts a curried function of unknown arity <i>n</i>.  Return an equivalent function of <i>n</i> arguments.
;;
;; <br/>
;;
;; You may wish to read <a href="http://en.wikipedia.org/wiki/Currying">this</a>.
;;
;; Use M-x 4clojure-check-answers when you're done!

;; sig: fun, numbers -> fun
;; purpose apply curried functions

;; idea

;; input
(fn [a]
  (fn [b]
    (* a b)))
;; return value
(fn [a b]
  (* a b))


;; test
(defn __ [fun]
  ;; return a function
  (fn [& args]
    (loop [fun1  fun
           args1 args]
      ;;
      (if (empty? (rest args1))
        ;; if only one argument left, just apply the function and return result
        (fun1 (first args1))
        ;; if multiple arguments exist, apply the first and loop
        (recur (fun1 (first args1)) (rest args1))))))

;; when there are only two loop arguments, think of reduce
;; pcl
(defn __ [f]
  (fn [& xs]
    ;; %1 is the function from previous step, %2 is the next argument
    (reduce #(%1 %2) f xs)))


(= 10 ((__ (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d))))))
       1 2 3 4))
(= 24 ((__ (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (* a b c d))))))
       1 2 3 4))
(= 25 ((__ (fn [a]
             (fn [b]
               (* a b))))
       5 5))




;;; 4Clojure Question 53
;;
;; Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. If two sub-sequences have the same length, use the one that occurs first. An increasing sub-sequence must have a length of 2 or greater to qualify.
;;
;; Use M-x 4clojure-check-answers when you're done!

;; use reduced to stop reduction
;; http://stackoverflow.com/questions/23822260/how-to-stop-a-reduce-function-from-processing-the-list-once-the-desired-accumula
(reduce (fn [sum x] 
          (if (> sum 10) 
            (reduced 10) 
            (+ sum x))) 
        0 
        [1 2 3 4 5 6 7 8 9 10])


;; take values from a sequence while it is strictly increasing
(defn take-while-inc [sq]
  (loop [acc [(first sq)]
         sq1 (rest sq)]
    (if (or (empty? sq1) (>= (last acc) (first sq1)))
      acc
      (recur (conj acc (first sq1)) (rest sq1)))))

(take-while-inc [0 1 2 3 0 4 5])
(take-while-inc [0 1 2])

;; Create a strictly increasing sequence starting at each position
(defn seq-at-each-pos [sq]
  (loop [acc []
         sq2 sq]
    (if (empty? sq2)
      acc
      (recur (conj acc (take-while-inc sq2)) (rest sq2)))))

;; unify these helpers
(defn __ [sq]
  (let [seq-at-each-pos1 (seq-at-each-pos sq)
        lengths          (map count seq-at-each-pos1)
        max-length       (reduce max lengths)
        answer           (first (drop-while #(< (count %) max-length) seq-at-each-pos1))]
    ;;
    (if (= 1 (count answer))
      []
      answer)))


;; as one function


(defn __ [sq]
  (let [;; helper function 1: take values from a sequence while it is strictly increasing
        take-while-inc (fn [sq]
                         (loop [acc [(first sq)]
                                sq1 (rest sq)]
                           (if (or (empty? sq1) (>= (last acc) (first sq1)))
                             acc
                             (recur (conj acc (first sq1)) (rest sq1)))))
        ;; helper function 2: Create a strictly increasing sequence starting at each position
        seq-at-each-pos (fn  [sq]
                          (loop [acc []
                                 sq2 sq]
                            (if (empty? sq2)
                              acc
                              (recur (conj acc (take-while-inc sq2)) (rest sq2)))))
        ;; Obtain strictly increasing sequences
        seq-at-each-pos1 (seq-at-each-pos sq)
        ;; Check their length
        lengths          (map count seq-at-each-pos1)
        ;; Obtain the max
        max-length       (reduce max lengths)
        ;; Get the first sequence with the max length
        answer           (first (drop-while #(< (count %) max-length) seq-at-each-pos1))]
    ;;
    ;; Drop one element answer
    (if (= 1 (count answer))
      []
      answer)))

(= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (__ [5 6 1 3 2 7]) [5 6])
(= (__ [2 3 3 4 5]) [3 4 5])
(= (__ [7 6 5 4]) [])

