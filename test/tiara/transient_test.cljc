(ns tiara.transient-test
  (:require [clojure.test :refer [deftest testing is]]
            [tiara.data-test :refer [make-kv-range make-multi-kv-range even-only make-key]]
            [tiara.data :refer [ordered-map EMPTY_MAP ordered-set oset EMPTY_SET
                                multi-map EMPTY_MULTI_MAP]]))

#?(:clj (set! *warn-on-reflection* true))

(deftest test-ordered-round-trip
  (testing "equivalence of maps that round-trip through transience"
    (is (= {:a 1} (persistent! (transient (ordered-map :a 1)))))
    (let [m1 (apply hash-map (make-kv-range 20))
          m2 (persistent! (transient (apply ordered-map (make-kv-range 20))))]
      (is (= m1 m2))
      (is (= (map vec (partition 2 (make-kv-range 20))) (seq m2))))))

(deftest test-multi-round-trip
  (testing "equivalence of maps that round-trip through transience"
    (is (= {:a #{1}} (persistent! (transient (multi-map :a 1)))))
    (is (= {:a #{1 2} :b #{3}} (persistent! (transient (multi-map :a 1 :a 2 :b 3)))))
    (let [m1 (apply hash-map (make-kv-range 20))
          m2 (persistent! (transient (apply multi-map (make-kv-range 20))))]
      (is (= m1 m2))
      (is (= (set (map vec (partition 2 (make-kv-range 20))))
             (set (seq m2)))))))

(deftest test-ordered-assoc!
  (testing "equivalence of maps built with assoc"
    (is (= {:a 1 :b 2} (-> (transient EMPTY_MAP)
                           (assoc! :a 1)
                           (assoc! :b 2)
                           persistent!)))
    (let [om (apply ordered-map (make-kv-range 20))
          om2 (persistent! (reduce (fn [m [k v]] (assoc! m k v))
                                   (transient EMPTY_MAP)
                                   om))
          om3 (persistent! (reduce (fn [m [k v]] (assoc! m k v))
                                   (transient (apply ordered-map (flatten (take 10 om))))
                                   (drop 10 om)))]
      (is (= om om2))
      (is (= (seq om) (seq om2)))
      (is (= om om3))
      (is (= (seq om) (seq om3))))))

(deftest test-multi-assoc!
  (testing "equivalence of maps built with assoc"
    (is (= {:a #{1} :b #{2 3}} (-> (transient EMPTY_MULTI_MAP)
                                   (assoc! :a 1)
                                   (assoc! :b 2)
                                   (assoc! :b 3)
                                   persistent!)))
    (let [om (apply multi-map (make-kv-range 20))
          om2 (persistent! (reduce (fn [m [k v]] (assoc! m k v))
                                   (transient EMPTY_MULTI_MAP)
                                   om))
          om3 (persistent! (reduce (fn [m [k v]] (assoc! m k v))
                                   (transient (apply multi-map (flatten (take 8 om))))
                                   (drop 8 om)))]
      (is (= om om2))
      (is (= (seq om) (seq om2)))
      (is (= om om3))
      (is (= (seq om) (seq om3))))))

(deftest test-ordered-conj!
  (testing "equivalence of maps built with conj!"
    (is (= {:a 1 :b 2} (-> (transient EMPTY_MAP)
                           (conj! [:a 1])
                           (conj! [:b 2])
                           persistent!)))
    (let [om (apply ordered-map (make-kv-range 20))
          om2 (apply ordered-map (make-kv-range 1100))
          tom (persistent! (reduce conj!
                                   (transient EMPTY_MAP)
                                   om))
          tom-2 (persistent! (reduce conj!
                                     (transient (apply ordered-map (flatten (take 10 om))))
                                     (drop 10 om)))
          tom2 (persistent! (reduce conj!
                                    (transient EMPTY_MAP)
                                    om2))
          tom2-2 (persistent! (reduce conj!
                                      (transient (apply ordered-map (flatten (take 550 om2))))
                                      (drop 550 om2)))]
      (is (= om tom))
      (is (= (seq om) (seq tom)))
      (is (= om tom-2))
      (is (= (seq om) (seq tom-2)))
      (is (= om2 tom2))
      (is (= (seq om2) (seq tom2)))
      (is (= om2 tom2-2))
      (is (= (seq om2) (seq tom2-2))))))

(deftest test-multi-conj!
  (testing "equivalence of multi-maps built with conj!"
    (is (= {:a #{1} :b #{2 3}} (-> (transient EMPTY_MULTI_MAP)
                                   (conj! [:a 1])
                                   (conj! [:b 2])
                                   (conj! [:b 3])
                                   persistent!)))
    (let [om (apply multi-map (make-kv-range 20))
          om2 (apply multi-map (make-kv-range 1100))
          tom (persistent! (reduce conj!
                                   (transient EMPTY_MULTI_MAP)
                                   om))
          tom-2 (persistent! (reduce conj!
                                     (transient (apply multi-map (flatten (take 10 om))))
                                     (drop 10 om)))
          tom2 (persistent! (reduce conj!
                                    (transient EMPTY_MULTI_MAP)
                                    om2))
          tom2-2 (persistent! (reduce conj!
                                      (transient (apply multi-map (flatten (take 550 om2))))
                                      (drop 550 om2)))]
      (is (= om tom))
      (is (= (seq om) (seq tom)))
      (is (= om tom-2))
      (is (= (seq om) (seq tom-2)))
      (is (= om2 tom2))
      (is (= (seq om2) (seq tom2)))
      (is (= om2 tom2-2))
      (is (= (seq om2) (seq tom2-2))))))

(deftest test-ordered-into
  (testing "equivalent of maps built with into"
    (let [data1 [[:a 1] [:b 2]]
          rng (make-kv-range 20)
          rngv (map vec (partition 2 rng))]
      (is (= {:a 1 :b 2} (into EMPTY_MAP data1)))
      (is (= {:a 1 :b 2} (persistent! (reduce conj! (transient EMPTY_MAP) data1))))
      (is (= (apply ordered-map rng)
             (into EMPTY_MAP rngv)))
      (is (= rngv (seq (into EMPTY_MAP rngv))))
      (is (= (apply ordered-map rng)
             (into (apply ordered-map (flatten (take 20 rng)))
                   (drop 10 rngv))))
      (is (= rngv (seq (into (apply ordered-map (flatten (take 20 rng)))
                             (drop 10 rngv))))))))

(deftest test-multi-into
  (testing "equivalent of maps built with into"
    (let [data1 [[:a 1] [:b 2]]
          rng (make-kv-range 20)
          rngv (map vec (partition 2 rng))]
      (is (= {:a #{1} :b #{2}} (into EMPTY_MULTI_MAP data1)))
      (is (= {:a 1 :b 2} (persistent! (reduce conj! (transient EMPTY_MULTI_MAP) data1))))
      (is (= (apply multi-map rng)
             (into EMPTY_MULTI_MAP rngv)))
      (is (= (set rngv) (set (into EMPTY_MULTI_MAP rngv))))
      (is (= (apply multi-map rng)
             (into (apply multi-map (flatten (take 20 rng)))
                   (drop 10 rngv))))
      (is (= (set rngv) (set (into (apply multi-map (flatten (take 20 rng)))
                                   (drop 10 rngv))))))))

(deftest test-ordered-internal-mutability
  (testing "That small amounts of data do not change the object"
    (let [tm (transient EMPTY_MAP)]
      (assoc! tm :a 1)
      (assoc! tm :b 2)
      (conj! tm [:c 3])
      (is (= {:a 1 :b 2 :c 3} (persistent! tm))))))

(deftest test-multi-internal-mutability
  (testing "That small amounts of data do not change the object"
    (let [tm (transient EMPTY_MULTI_MAP)]
      (assoc! tm :a 1)
      (assoc! tm :b 2)
      (conj! tm [:b 3])
      (is (= {:a #{1} :b #{2 3}} (persistent! tm))))))

(deftest test-ordered-without
  (testing "without removes data correctly"
    (is (= {:a 1 :c 3} (-> (ordered-map :a 1 :b 2 :c 3)
                           transient
                           (dissoc! :b)
                           persistent!)))
    (let [rng (make-kv-range 1100)
          rkeys (map make-key (take 550 (shuffle (range 1100))))]
      (is (= (apply dissoc (apply ordered-map rng) rkeys)
             (-> (apply ordered-map rng)
                 transient
                 (#(apply dissoc! % rkeys))
                 persistent!))))))

(defn entry [a b] (first (seq (multi-map a b))))

(deftest test-multi-without
  (testing "without removes data correctly"
    (is (= {:a #{1} :c #{3}} (-> (multi-map :a 1 :b 2 :c 3)
                                 transient
                                 (dissoc! :b)
                                 persistent!)))
    (is (= {:a #{1}} (-> (multi-map :a 1 :b 2 :b 3)
                         transient
                         (dissoc! :b)
                         persistent!)))
    (is (= {:a #{1} :b #{3}} (-> (multi-map :a 1 :b 2 :b 3)
                                 transient
                                 (dissoc! [:b 2])
                                 persistent!)))
    (is (= {:a #{1}} (-> (multi-map :a 1 :b 2 :b 3)
                         transient
                         (dissoc! [:b 2])
                         (dissoc! [:b 3])
                         persistent!)))
    (is (= {:a #{1}} (-> (multi-map :a 1 :b 2 :b 3)
                         transient
                         (dissoc! [:b 2])
                         (dissoc! [:b 3])
                         (dissoc! [:b 2])
                         persistent!)))
    (is (= {:a #{1} :b #{3}} (-> (multi-map :a 1 :b 2 :b 3)
                                 transient
                                 (dissoc! (entry :b 2))
                                 persistent!)))
    (is (= {:a #{1}} (-> (multi-map :a 1 :b 2 :b 3)
                         transient
                         (dissoc! (entry :b 2))
                         (dissoc! (entry :b 3))
                         persistent!)))
    (is (= {:a #{1}} (-> (multi-map :a 1 :b 2 :b 3)
                         transient
                         (dissoc! (entry :b 2))
                         (dissoc! (entry :b 3))
                         (dissoc! (entry :b 2))
                         persistent!)))
    (let [rng (make-kv-range 1100)
          rkeys (map make-key (take 550 (shuffle (range 1100))))]
      (is (= (apply dissoc (apply multi-map rng) rkeys)
             (-> (apply multi-map rng)
                 transient
                 (#(apply dissoc! % rkeys))
                 persistent!))))))

(deftest test-ordered-count
  (testing "count operation on transient maps"
    (is (= 1 (count (transient (ordered-map :a 1)))))
    (is (= 3 (count (transient (ordered-map :a 1 :b 2 :c 3)))))
    (let [rng (make-kv-range 1100)
          rkeys (map make-key (take 550 (shuffle (range 1100))))]
      (is (= 550
             (-> (apply ordered-map rng)
                 transient
                 (#(apply dissoc! % rkeys))
                 count))))))

(deftest test-multi-count
  (testing "count operation on transient multi maps"
    (is (= 1 (count (transient (multi-map :a 1)))))
    (is (= 3 (count (transient (multi-map :a 1 :b 2 :c 3)))))
    (is (= 3 (count (transient (multi-map :a 1 :b 2 :c 3 :b 2)))))
    (is (= 4 (count (transient (multi-map :a 1 :b 2 :c 3 :b 3)))))
    (let [rng (make-kv-range 1100)
          rkeys (map make-key (take 550 (shuffle (range 1100))))]
      (is (= 550
             (-> (apply multi-map rng)
                 transient
                 (#(apply dissoc! % rkeys))
                 count))))))

(deftest test-ordered-in-and-out
  (testing "checking if data is consistent if swapping between additions and removals"
    (is (= {:a 1 :c 3 :d 4} (-> (ordered-map :a 1 :b 2 :c 3)
                                transient
                                (dissoc! :b)
                                (assoc! :d 4)
                                persistent!)))
    (let [rng (make-kv-range 1100)
          reordered-vals (take 550 (shuffle (range 1100)))
          rkeys (map make-key reordered-vals)
          ikvs (take 550 (interleave rkeys reordered-vals))
          o1 (-> (apply ordered-map rng)
                 (#(apply dissoc % rkeys))
                 (#(apply assoc % ikvs)))
          to1 (-> (apply ordered-map rng)
                  transient
                  (#(apply dissoc! % rkeys))
                  (#(apply assoc! % ikvs))
                  persistent!)]
      (is (= o1 to1))
      (is (= (seq o1) (seq to1)))
      (is (= 825 (count to1)))
      (is (= 825 (count (seq to1)))))))

(deftest test-multi-in-and-out
  (testing "checking if data is consistent if swapping between additions and removals"
    (is (= {:a #{1} :c #{3} :d #{4}} (-> (multi-map :a 1 :b 2 :c 3)
                                         transient
                                         (dissoc! :b)
                                         (assoc! :d 4)
                                         persistent!)))
    (is (= {:a #{1} :c #{3} :d #{5 6}} (-> (multi-map :a 1 :b 2 :c 3 :d 4)
                                           transient
                                           (dissoc! :b)
                                           (assoc! :d 5)
                                           (dissoc! (entry :d 4))
                                           (assoc! :d 6)
                                           persistent!)))
    (let [rng (make-kv-range 1100)
          reordered-vals (take 550 (shuffle (range 1100)))
          rkeys (map make-key reordered-vals)
          ikvs (take 550 (interleave rkeys reordered-vals))
          o1 (-> (apply multi-map rng)
                 (#(apply dissoc % rkeys))
                 (#(apply assoc % ikvs)))
          to1 (-> (apply multi-map rng)
                  transient
                  (#(apply dissoc! % rkeys))
                  (#(apply assoc! % ikvs))
                  persistent!)]
      (is (= o1 to1))
      (is (= (seq o1) (seq to1)))
      (is (= 825 (count to1)))
      (is (= 825 (count (seq to1)))))
    (let [rng (make-multi-kv-range 1100)
          reordered-vals (take 550 (shuffle (range 1100)))
          rkeys (map make-key reordered-vals)
          rkvs (map entry rkeys (even-only reordered-vals))
          ikvs (take 550 (interleave rkeys (even-only reordered-vals)))
          o1 (-> (apply multi-map rng)
                 (#(apply dissoc % rkvs))
                 (#(apply assoc % ikvs)))
          to1 (-> (apply multi-map rng)
                  transient
                  (#(apply dissoc! % rkvs))
                  (#(apply assoc! % ikvs))
                  persistent!)]
      (is (= o1 to1))
      (is (= (seq o1) (seq to1)))
      (is (= 825 (count to1)))
      (is (= 825 (count (seq to1)))))))

;; Transitive ordered sets

(defn make-k-range [n] (map make-key (range n)))

(def k10 (make-k-range 10))
(def k20 (make-k-range 20))
(def k550 (make-k-range 550))
(def k1100 (make-k-range 1100))

(deftest test-set-round-trip
  (testing "equivalence of sets that round-trip through transience"
    (is (= #{:a} (persistent! (transient (ordered-set :a)))))
    (is (= (apply hash-set k20)
           (persistent! (transient (apply ordered-set k20)))))))

(deftest test-set-conj!
  (testing "equivalence of sets built with conj!"
    (is (= #{:a :b} (-> (transient EMPTY_SET)
                        (conj! :a)
                        (conj! :b)
                        persistent!)))
    (let [os (apply ordered-set k20)
          os2 (apply ordered-set k1100)
          p11 (persistent! (reduce conj! (transient EMPTY_SET) k20))
          p12 (persistent! (reduce conj! (transient (apply ordered-set k10)) (drop 10 k20)))
          p21 (persistent! (reduce conj! (transient EMPTY_SET) k1100))
          p22 (persistent! (reduce conj! (transient (apply ordered-set k550)) (drop 550 k1100)))]
      (is (= os p11))
      (is (= k20 (seq p11)))
      (is (= os p12))
      (is (= k20 (seq p12)))
      (is (= os2 p21))
      (is (= k1100 (seq p21)))
      (is (= os2 p22))
      (is (= k1100 (seq p22))))))

(deftest test-set-into
  (testing "equivalent of sets built with into"
    (let [data1 [:a :b]]
      (is (= #{:a :b} (into EMPTY_SET data1)))
      (is (= #{:a :b} (persistent! (reduce conj! (transient EMPTY_SET) data1))))
      (is (= (apply ordered-set k20)
             (into EMPTY_SET k20)))
      (is (= k20 (seq (into EMPTY_SET k20))))
      (is (= (apply ordered-set k20)
             (into (apply ordered-set k10)
                   (drop 10 k20))))
      (is (= k20 (seq (into (apply ordered-set k10)
                            (drop 10 k20))))))))

(deftest test-set-internal-mutability
  (testing "That adding small amounts of data in the set do not change the object"
    (let [ts (transient EMPTY_SET)]
      (conj! ts :a)
      (conj! ts :b)
      (conj! ts :c)
      (is (= #{:a :b :c} (persistent! ts))))))

(deftest test-set-without
  (testing "without removes data from sets correctly"
    (is (= #{:a :c} (-> (ordered-set :a :b :c)
                        transient
                        (disj! :b)
                        persistent!)))
    (let [rkeys (take 550 (shuffle k1100))
          result-set (-> (apply ordered-set k1100)
                         transient
                         (#(apply disj! % rkeys))
                         persistent!)]
      (is (= (apply disj (apply ordered-set k1100) rkeys) result-set))
      (is (= (remove (set rkeys) k1100)
             (seq result-set))))))

(deftest test-set-count
  (testing "count operation on transient sets"
    (is (= 1 (count (transient (ordered-set :a)))))
    (is (= 3 (count (transient (ordered-set :a :b :c)))))
    (let [rkeys (take 550 (shuffle k1100))]
      (is (= 550
             (-> (apply ordered-set k1100)
                 transient
                 (#(apply disj! % rkeys))
                 count))))))

(deftest test-set-in-and-out
  (testing "checking if set data is consistent if swapping between additions and removals"
    (is (= #{:a :c :d} (-> (ordered-set :a :b :c)
                           transient
                           (disj! :b)
                           (conj! :d)
                           persistent!)))
    (let [rkeys (take 550 (shuffle k1100))
          ikeys (take 275 rkeys)
          o1 (-> (apply ordered-set k1100)
                 (#(apply disj % rkeys))
                 (#(apply conj % ikeys)))
          to1 (-> (apply ordered-set k1100)
                  transient
                  (#(apply disj! % rkeys))
                  (#(reduce conj! % ikeys))
                  persistent!)]
      (is (= o1 to1))
      (is (= (concat (remove (set rkeys) k1100) ikeys) (seq to1)))
      (is (= 825 (count to1)))
      (is (= 825 (count (seq to1)))))))

#?(:cljs (cljs.test/run-tests))

;; TIARA Is A Recursive Acronym
