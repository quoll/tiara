(ns tiara.compare
  (:require [tiara.data]
            [flatland.ordered.map]
            [linked.core]
            [criterium.core :refer [bench]]))
;; (add-lib 'org.flatland/ordered {:mvn/version "1.15.12"})
;; (add-lib 'frankiesardo/linked {:mvn/version "1.3.0"})

(def size 1000000)
(def data (shuffle (range size)))

(defn benchmarks!
  [& args]
  (println "\n***************")
  (println "** Insertion **")
  (println "***************\n")
  (println ">>> Linked")
  (bench (into (linked.core/map) (map (fn [n] [n n]) data)))
  (println ">>> Ordered")
  (bench (into (flatland.ordered.map/ordered-map) (map (fn [n] [n n]) data)))
  (println ">>> Tiara")
  (bench (into (tiara.data/ordered-map) (map (fn [n] [n n]) data)))
  (let [linked (into (linked.core/map) (map (fn [n] [n n]) data))
        ordered (into (flatland.ordered.map/ordered-map) (map (fn [n] [n n]) data))
        tiara (into (tiara.data/ordered-map) (map (fn [n] [n n]) data))]
    (println "\n*******************")
    (println "** Random Access **")
    (println "*******************\n")
    (println ">>> Linked")
    (bench (reduce (fn [acc n] (+ acc (get linked n))) 0 data))
    (println ">>> Ordered")
    (bench (reduce (fn [acc n] (+ acc (get ordered n))) 0 data))
    (println ">>> Tiara")
    (bench (reduce (fn [acc n] (+ acc (get tiara n))) 0 data))
    (println "\n***************************")
    (println "** Sequential Processing **")
    (println "***************************\n")
    (println ">>> Linked")
    (bench (reduce + 0 (vals linked)))
    (println ">>> Ordered")
    (bench (reduce + 0 (vals ordered)))
    (println ">>> Tiara")
    (bench (reduce + 0 (vals tiara)))
    (println "\n************************")
    (println "** Processing Seq API **")
    (println "************************\n")
    (println ">>> Linked")
    (bench (reduce (fn [a _] (inc a)) 0 (seq linked)))
    (println ">>> Ordered")
    (bench (reduce (fn [a _] (inc a)) 0 (seq ordered)))
    (println ">>> Tiara")
    (bench (reduce (fn [a _] (inc a)) 0 (seq tiara)))))

