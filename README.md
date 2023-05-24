# Tiara
A small data structure library.

### deps.edn

Add the following dependency to the :deps map in deps.edn:

```clojure
org.clojars.quoll/tiara {:mvn/version "0.2.0"}
```

## Usage
At this point, Tiara only includes an ordered map and an ordered set. These have the same O(1) access characteristics as hashmaps/hashsets, but maintain insertion order. However, `dissoc` and `disj` will be o(n) in the worst case.

Ordered Maps behave the same as other maps, but are only created using the `ordered-map` function.

Ordered Sets are the same as other sets, but can be created using `ordered-set` when passing the set's contents as arguments, or `oset` when passing the contents in a seq.

```clojure
(require '[tiara.data :refer [ordered-map ordered-set oset]])

;; maps
(def m (ordered-map :a 1 :b 2 :c 3 :d 4 :e 5))
(def h (hash-map    :a 1 :b 2 :c 3 :d 4 :e 5))

(= h m)  ;; => true

(seq m)  ;; => ([:a 1] [:b 2] [:c 3] [:d 4] [:e 5])

(assoc m :f 6)  ;; => {:a 1, :b 2, :c 3, :d 4, :e 5, :f 6}
  ;; but existing keys do not change location:
(assoc m :c 7)  ;; => {:a 1, :b 2, :c 7, :d 4, :e 5}
  ;; to append, then remove first:
(assoc (dissoc m :c) :c 7)  ;; => {:a 1, :b 2, :d 4, :e 5, :c 7}


;; sets
(def os (ordered-set :a :b :c :d :e :f :g :h :i))
(def hs (hash-set :a :b :c :d :e :f :g :h :i))

(= os hs)  ;; => true

(seq os)  ;; => (:a :b :c :d :e :f :g :h :i)

(conj os :j)  ;; => #{:a :b :c :d :e :f :g :h :i :j}
(disj os :b)  ;; => #{:a :c :d :e :f :g :h :i}

  ;; adding again does not change location
(conj os :d)  ;; => #{:a :b :c :d :e :f :g :h :i}
  ;; to move to the end, remove and add
(conj (disj os :d) :d)  ;; => #{:a :b :c :e :f :g :h :i :d}

```

## License

Copyright © 2023 Paula Gearon

Distributed under the Eclipse Public License version 2.0.
