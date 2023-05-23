# Tiara
A small data structure library.

### deps.edn

Add the following dependency to the :deps map in deps.edn:

```clojure
io.github.quoll/tiara {:git/tag "v0.1.0" :git/sha "500bad1"}
```

## Usage
At this point, Tiara only includes an ordered map. This has the same O(1) access characteristics as a hashmap, but maintains insertion order. However, `dissoc` will be o(n) in the worst case.

Ordered Maps behave the same as other maps, but are only created using the `ordered-map` function.

```clojure
(require '[tiara.data :refer [ordered-map]])

(def m (ordered-map :a 1 :b 2 :c 3 :d 4 :e 5))
(def h (hash-map    :a 1 :b 2 :c 3 :d 4 :e 5))

(= h m) ;; returns true

(seq m) ;; => ([:a 1] [:b 2] [:c 3] [:d 4] [:e 5])

(assoc m :f 6) ;; => {:a 1, :b 2, :c 3, :d 4, :e 5, :f 6}
  ;; but existing keys do not change location:
(assoc m :c 7) ;; => {:a 1, :b 2, :c 7, :d 4, :e 5}
  ;; to append, then remove first:
(assoc (dissoc m :c) :c 7) ;; => {:a 1, :b 2, :d 4, :e 5, :c 7}
```
## TODO
- Ordered sets

## License

Copyright © 2023 Paula Gearon

Distributed under the Eclipse Public License version 2.0.
