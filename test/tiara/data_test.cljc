(ns tiara.data-test
  (:require [clojure.test :refer [deftest testing is]]
            [tiara.data :refer [ordered-map EMPTY_MAP ordered-set oset EMPTY_SET]]))

#?(:clj (set! *warn-on-reflection* true))

(defn make-key [n] (keyword (str "k" n)))

(defn make-kv-range [n] (interleave (map make-key (range n)) (range n)))

(defn make-map
  [m]
  (apply m (make-kv-range 20)))

(defn make-map-assoc [m]
  (reduce #(apply assoc %1 %2)
          (m)
          (shuffle (map (juxt make-key identity) (range 20)))))

(deftest test-map-equiv
  (testing "map equivalence for constructed maps"
    (is (= (apply ordered-map (make-kv-range 8))
           (apply array-map (make-kv-range 8))))
    (is (= (apply array-map (make-kv-range 8))
           (apply ordered-map (make-kv-range 8))))
    (is (= (apply ordered-map (make-kv-range 20))
           (apply hash-map (make-kv-range 20))))
    (is (= (apply hash-map (make-kv-range 20))
           (apply ordered-map (make-kv-range 20))))))

(deftest test-map-hash-equiv
  (testing "map hash equivalence for constructed maps"
    (is (= (hash (apply ordered-map (make-kv-range 8)))
           (hash (apply array-map (make-kv-range 8)))))
    (is (= (hash (apply ordered-map (make-kv-range 20)))
           (hash (apply hash-map (make-kv-range 20)))))))

(deftest test-seq
  (testing "seq functionality of a map"
    (is (= (map (juxt make-key identity) (range 20))
           (seq (apply ordered-map (make-kv-range 20)))))
    (is (= (seq (apply array-map (make-kv-range 20)))
           (seq (apply ordered-map (make-kv-range 20)))))))

#?(:clj
  (defn- drain-iterable
    "Similar to iterator-seq, without chunking"
    [i]
    (let [^java.util.Iterator it (.iterator ^Iterable i)]
      (loop [v []]
        (if (.hasNext it)
          (recur (conj v (.next it)))
          v)))))

#?(:clj
  (deftest test-iterable
    (testing "Testing the iterable interface"
      (is (= [] (drain-iterable (ordered-map))))
      (is (= [[:a 1] [:b 2]] (drain-iterable (ordered-map :a 1 :b 2)))))))

(deftest test-assoc
  (testing "equivalence of maps constructed using assoc"
    (is (= {:a 1 :b 2} (assoc (ordered-map :a 1) :b 2)))
    (is (= (make-map-assoc hash-map)
           (make-map-assoc ordered-map)))
    (is (= (make-map-assoc ordered-map)
           (make-map-assoc hash-map)))))

(deftest test-conj
  (testing "equivalence of maps constructed using conj"
    (let [kvs (shuffle (map (juxt make-key identity) (range 20)))]
      (is (= (reduce conj EMPTY_MAP kvs)
             (reduce conj (hash-map) kvs))))))

(deftest test-dissoc
  (testing "dissoc works equivalently to hashmap"
    (is (= {:a 1} (dissoc (ordered-map :a 1 :b 2) :b)))
    (let [rms (take 8 (map make-key (shuffle (range 20))))]
      (is (= (reduce dissoc (make-map ordered-map) rms)
             (reduce dissoc (make-map hash-map) rms))))))

(deftest test-get
  (testing "get works as expected"
    (is (= 2 (get (ordered-map :a 1 :b 2) :b)))
    (let [mp (make-map ordered-map)]
      (doseq [n (shuffle (range 20))]
        (is (= n (get mp (make-key n))))))))

(deftest test-invoke
  (testing "invoke works as expected"
    (is (= 2 ((ordered-map :a 1 :b 2) :b)))
    (let [mp (make-map ordered-map)]
      (doseq [n (shuffle (range 20))]
        (is (= n (mp (make-key n))))))))

(deftest test-find
  (testing "find works as expected"
    (is (= [:b 2] (find (ordered-map :a 1 :b 2) :b)))
    (let [mp (make-map ordered-map)]
      (doseq [n (shuffle (range 20))]
        (is (= [(make-key n) n] (find mp (make-key n))))))))

(deftest test-keys
  (testing "keys returns the correct order"
    (let [kvs (shuffle (map (juxt make-key identity) (range 20)))
          mp (reduce conj EMPTY_MAP kvs)]
      (is (= (keys mp) (map first kvs))))))

(deftest test-vals
  (testing "vals returns the correct order"
    (let [kvs (shuffle (map (juxt make-key identity) (range 20)))
          mp (reduce conj EMPTY_MAP kvs)]
      (is (= (vals mp) (map second kvs))))))

(deftest test-strings
  (testing "If the str function works on maps"
    (is (= "{:a \"one\", :b \"two\"}" (str (ordered-map :a "one" :b "two"))))))

(deftest test-meta
  (testing "If metadata survives modifications to the map"
    (let [m (with-meta (ordered-map :a "one") {:doc "data"})]
      (is (= "data" (:doc (meta (assoc m :b "two")))))
      (is (= "data" (:doc (meta (dissoc m :a)))))
      (is (= "data" (:doc (meta (dissoc m :b)))))
      (is (= "data" (:doc (meta (empty m))))))))

(deftest test-construct
  (testing "If multiple uses of a key show up in the place of the first key"
    (let [m (ordered-map :a 1 :b 2 :c 3 :a 4 :d 5 :e 6 :f 7 :h 8 :i 9 :j 10)]
      (is (= {:a 4 :b 2 :c 3 :d 5 :e 6 :f 7 :h 8 :i 9 :j 10} m))
      (is (= [[:a 4] [:b 2] [:c 3] [:d 5] [:e 6]
              [:f 7] [:h 8] [:i 9] [:j 10]] (seq m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set testing

(defn krange [n] (map make-key (range n)))

(deftest test-set-equiv
  (testing "set equivalence for constructed sets"
    (is (= (apply ordered-set (range 8))
           (set (range 8))))
    (is (= (set (range 8))
           (apply ordered-set (range 8))))
    (is (= (apply ordered-set (range 20))
           (apply hash-set (range 20))))
    (is (= (apply hash-set (range 20))
           (apply ordered-set (range 20)))))
  (testing "set equivalence for constructed sets with keyword entries"
    (is (= (apply ordered-set (krange 8))
           (set (krange 8))))
    (is (= (set (krange 8))
           (apply ordered-set (krange 8))))
    (is (= (apply ordered-set (krange 20))
           (apply hash-set (krange 20))))
    (is (= (apply hash-set (krange 20))
           (apply ordered-set (krange 20))))))

(deftest test-set-hash-equiv
  (testing "set equivalence for constructed sets"
    (is (= (hash (apply ordered-set (range 8)))
           (hash (set (range 8)))))
    (is (= (hash (apply ordered-set (range 20)))
           (hash (apply hash-set (range 20))))))
  (testing "set equivalence for constructed sets with keyword entries"
    (is (= (hash (apply ordered-set (krange 8)))
           (hash (set (krange 8)))))
    (is (= (hash (apply ordered-set (krange 20)))
           (hash (apply hash-set (krange 20)))))))

(deftest test-set-seq
  (testing "seq functionality of a set"
    (is (= (krange 20)
           (seq (oset (krange 20)))))))

(defn make-set-conj [st]
  (reduce conj (st) (shuffle (krange 20))))

(deftest test-set-conj
  (testing "equivalence of sets constructed using conj"
    (is (= #{:a :b} (conj (ordered-set :a) :b)))
    (is (= (make-set-conj hash-set)
           (make-set-conj ordered-set)))
    (is (= (make-set-conj ordered-set)
           (make-set-conj hash-set)))))

(deftest test-disj
  (testing "disj works equivalently to hashset"
    (is (= #{:a} (disj (ordered-set :a :b) :b)))
    (let [rms (take 8 (shuffle (krange 20)))]
      (is (= (reduce disj (oset (krange 20)) rms)
             (reduce disj (set (krange 20)) rms))))))

(deftest test-set-get
  (testing "get works on sets as expected"
    (is (= :b (get (ordered-set :a :b) :b)))
    (let [s (oset (krange 20))]
      (doseq [n (shuffle (range 20))]
        (is (= (make-key n) (get s (make-key n))))
        (is (nil? (get s (make-key (+ 20 n)))))))))

(deftest test-set-invoke
  (testing "invoke works on sets as expected"
    (is (= :b ((ordered-set :a :b) :b)))
    (let [s (oset (krange 20))]
      (doseq [n (shuffle (range 20))]
        (is (= (make-key n) (s (make-key n))))
        (is (nil? (s (make-key (+ 20 n)))))))))

(deftest test-set-strings
  (testing "If the str function works on sets"
    (is (= "#{:a :b}" (str (ordered-set :a :b))))))

#?(:cljs (cljs.test/run-tests))

;; TIARA Is A Recursive Acronym
