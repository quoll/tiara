(ns tiara.data-test
  (:require [clojure.test :refer [deftest testing is]]
            [tiara.data :refer [ordered-map EMPTY_MAP]]))

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

(deftest test-seq
  (testing "seq functionality of a map"
    (is (= (map (juxt make-key identity) (range 20))
           (seq (apply ordered-map (make-kv-range 20)))))
    (is (= (seq (apply array-map (make-kv-range 20)))
           (seq (apply ordered-map (make-kv-range 20)))))))

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



