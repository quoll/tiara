# Tiara
A small data structure library of maps and sets in Clojure and ClojureScript.

Include ordered maps and sets, and multimaps.

### deps.edn

Add the following dependency to the :deps map in deps.edn:

```clojure
org.clojars.quoll/tiara {:mvn/version "0.5.0"}
```

### Leiningen/Boot
```clojure
[org.clojars.quoll/tiara "0.5.0"]
```

## Usage
Tiara includes a multimap, an ordered map and an ordered set.

### Ordered Map and Ordered Set

The ordered structures have the same O(1) access characteristics as hashmaps/hashsets, but maintain insertion order. However, `dissoc` and `disj` will be o(n) in the worst case.

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

### Multimap
Multimaps offer a convenience of mapping keys to sets of values, rather than to a single value.

Calling `assoc` works as usual, with the exception that values are not replaced when the same key is added multiple times:

```clojure
(require '[tiara.data :refer [multi-map]])

(def mm (multi-map :a 1 :b 2 :b 3))

(= mm {:a #{1} :b #{2 3}})  ;; => true
(seq mm)                    ;; => ([:a 1] [:b 2] [:b 3])

(get mm :a)                 ;; => #{1}
(get mm :b)                 ;; => #{2 3}
```

#### Reversing
Sometimes it can be useful to reverse the direction of a map. For instance, if a map encodes a directed graph structure, and you want to reverse the direction of the edges.

```clojure
(def reverser (map (fn [[k v]] [v k])))

(def mm (into (multi-map) reverser {:a :b, :b :e, :c :d, :d :e}))
;; mm => {:b :a, :d :c, :e :d, :e :b}
(into {} reverser mm)
;;    => {:a :b, :b :e, :c :d, :d :e}
```

#### Equality
Equality comes with some caveats, however. A multi-map can appear first in an equality statement, but not second:

```clojure
(= mm {:a #{1} :b #{2 3}})  ;; => true
(= {:a #{1} :b #{2 3}} mm)  ;; => false
```
This can only be addressed by monkey-patching `clojure.core/=`, but no one wants that. (See [`tiara.data-test`](https://github.com/quoll/tiara/blob/f763bf47e4200815e885425c73c3290ba3c64409/test/tiara/data_test.cljc#L235-L259) for an example of how to do this).

#### Removals
A standard `dissoc` will remove everything associated with a key. This makes removing a single value for a given key a little awkward:

```clojure
(def mm (multi-map :a 1 :b 2 :b 3))
(let [s (get mm :b)]
  (-> mm
      (dissoc :b)
      (assoc :b (disj s 3))))
```

Instead, the `dissoc` function can also accept a key/value pair (or a `MapEntry` object):

```clojure
(dissoc mm [:b 3])
```

This is the same type of argument accepted by `conj`. Symmetry might suggest using `disj` for removing, but that would require multi maps to be sets, which would seem to be semantically dissimilar.

**Note:** This is up for debate. I've implemented `MultiMap` as a set with `disj` and it works fine. But it means that instances become instances of `IPersistentSet` which has some small impact on behavior. For instance, `(set (multi-map :a 1))` will return the original multimap, rather than a set of the key/value entries. Also, `print-method` needs `IPersistentMap` to be prioritized over `IPersistentSet`.

#### Lack of Updates
For now, updates are not possible. This is because `clojure.core/update` retrieves a value, modifies it, and uses `assoc` to add it back in. However, since the values are sets, then updating functions need to process the entire set. Re-associating a new set is difficult, as this would become a new value along with all the other values referenced by that key.

Updating must be done manually via:
```clojure
(let [s (get mm :b)]
  (-> mm
      (dissoc :b)
      (assoc :b (map update-fn s))))
```

One future option is to include updated functions for accomplishing this more easily. Another approach is to use an internal Set type, and for insertions of sets of this type to replace the set rather than being added as an element to it. Feedback would be appreciated.

## Performance
Overall, the Tiara implementations of Ordered Maps and Ordered Sets are similar or better than similar libraries. See the [performance](doc/performance.md) document for a comparison of the architecture and performance of Tiara and similar libraries.

## MultiMaps
Multi maps are a common data structure, and one that I have needed on many occasions. While mapping keys to sets of values is very common, this suffers from a lack of composability, and cannot be used with operations like `into` that work on transient variations. This multi-map implementation was written specifically to address these two issues.


## Future Work
- MultiMaps may use internal set implementations to allow operations like `update` to work.

## License

Copyright © 2023-2025 Paula Gearon

Distributed under the Eclipse Public License version 2.0.
