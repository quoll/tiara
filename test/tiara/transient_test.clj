(ns tiara.transient-test
  (:require [clojure.test :refer [deftest testing is]]
            [tiara.data-test :refer [make-map make-map-assoc make-kv-range]]
            [tiara.data :refer [ordered-map EMPTY_MAP ordered-set oset EMPTY_SET]]))

(set! *warn-on-reflection* true)

(deftest test-round-trip
  (testing "equivalence of maps that round-trip through transience"
    (is (= {:a 1} (persistent! (transient (ordered-map :a 1)))))
    (is (= (apply hash-map (make-kv-range 20))
           (persistent! (transient (apply ordered-map (make-kv-range 20))))))))

(deftest test-assoc!
  (testing "equivalence of maps built with assoc"
    (is (= {:a 1 :b 2} (-> (transient EMPTY_MAP)
                           (assoc! :a 1)
                           (assoc! :b 2)
                           persistent!)))
    (let [om (apply ordered-map (make-kv-range 20))]
      (is (= om
             (persistent! (reduce (fn [m [k v]] (assoc! m k v))
                                  (transient EMPTY_MAP)
                                  om))))
      (is (= om
             (persistent! (reduce (fn [m [k v]] (assoc! m k v))
                                  (transient (apply ordered-map (flatten (take 10 om))))
                                  (drop 10 om))))))))

(deftest test-conj!
  (testing "equivalence of maps built with conj!"
    (is (= {:a 1 :b 2} (-> (transient EMPTY_MAP)
                           (conj! [:a 1])
                           (conj! [:b 2])
                           persistent!)))
    (let [om (apply ordered-map (make-kv-range 20))]
      (is (= om
             (persistent! (reduce (fn [m kv] (conj! m kv))
                                  (transient EMPTY_MAP)
                                  om))))
      (is (= om
             (persistent! (reduce (fn [m kv] (conj! m kv))
                                  (transient (apply ordered-map (flatten (take 10 om))))
                                  (drop 10 om))))))))

(deftest test-into
  (testing "equivalent of maps built with into"
    (let [data1 [[:a 1] [:b 2]]
          rng (make-kv-range 20)
          rngv (map vec (partition 2 rng))]
      (is (= {:a 1 :b 2} (into EMPTY_MAP data1)))
      (is (= {:a 1 :b 2} (persistent! (reduce conj! (transient EMPTY_MAP) data1))))
      (is (= (apply ordered-map rng)
             (into EMPTY_MAP rngv)))
      (is (= (apply ordered-map rng)
             (into (apply ordered-map (flatten (take 20 rng)))
                   (drop 10 rngv)))))))

(deftest test-internal-mutability
  (testing "That small amounts of data do not change the object"
    (let [tm (transient EMPTY_MAP)]
      (assoc! tm :a 1)
      (assoc! tm :b 2)
      (conj! tm [:c 3])
      (is (= {:a 1 :b 2 :c 3} (persistent! tm))))))

;; TIARA Is A Recursive Acronym
