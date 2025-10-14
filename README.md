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

## Motivation
### Ordered Sets and Maps
I often encounter cases where I need to append data to a structure that will be written out in the same order it was read. However, duplicates of a simple piece of data should not be duplicated, or for more complex data, they should be merged with previously encountered information. These ordered sets and maps are ideal for this operation.

While it would be possible to use Java's [LinkedHashMap](https://docs.oracle.com/en/java/javase/20/docs/api/java.base/java/util/LinkedHashMap.html) and [LinkedHashSets](https://docs.oracle.com/en/java/javase/20/docs/api/java.base/java/util/LinkedHashSet.html), these are not immutable structures, and hence are not ideal for functional programming. I suspected that an immutable equivalent may have existed, but the required functionality did not appear difficult and I thought it would be an interesting learning experience to write my own version.

In fact, there were already existing libraries (found by [Tom Dalziel](https://github.com/tomdl89)):
- [Ordered](https://github.com/clj-commons/ordered)
- [Linked](https://github.com/frankiesardo/linked)

#### Comparison
All 3 implementations use a backing hashmap. Hashmaps use a hash-trie structure to find entries by key, though these are amalgamated over several entries, with one tree node being shared by up to 32 entries. Since these are common to all 3 systems, and incur a small overhead per entry, the overall space used by these maps can be considered as roughly the same between systems.

##### Tiara
Tiara uses a backing hashmap and a backing vector. When a key/value is stored, then a [`MapEntry`](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/MapEntry.java) is created and stored at the end of the vector. The size of the vector (and hence, the location of this new entry) is then stored with the key in the hashmap.

Each entry therefore uses a standard `MapEntry` for the key/location pair in the backing map, and a second `MapEntry` for the `key/value` pair in the vector.

Looking up a key will access the backing map for the location in the vector, and the key/value pair in the vector can be found at that location. This is a single indirection to the map entry. If the entry is requested via `find` then this can be returned directly from the vector.

Removing an entry is an expensive operation. The location in the vector is found, and a new vector is formed from the subvecs before and after that point. However, every index in the hashmap that referred to a location after the removal point needs to be decremented. These entries can be found by the keys from the second subvec, but the operation is still O(n). Removing every entry would be O(n^2). Fortunately, most uses of maps do not require significant removals, so I decided this was acceptable. However, in the case where a lot of removals are necessary, this would not be the right library to use.

Finally, the seq of the map is already available as the vector, so this can be returned with no work required.

##### Ordered
This is a similar structure to Tiara, using a hashmap for fast indexing, and a vector to store the order.

The hashmap entries consist of a nested [`MapEntry`](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/MapEntry.java) pair. Maps usually store a single key/value entry, but in this case the value is another `MapEntry`. This inner `MapEntry` contains the value for the key, and the index to find the value in the vector. The vector also contains another `MapEntry` object, this time containing the key/value pair. This therefore needs 3 `MapEntry` objects per insertion.

Looking up a key will access the backing hashmap to find the inner `MapEntry`, and then look up the value inside that object. This is also a single indirection. However, finding a full entry (using `find`) requires the construction of a new `MapEntry` object to return.

Removing an entry is done by removing the entry from the backing hashmap, and setting the position in the vector to a `nil` value. If a lot of removals are done, this will make the vector sparse. To account for this, a new `Compactable` protocol was introduced, with a `compact` function to remove all the `nil` entries and reindex the entries. This allows the cost of removal to be postponed and multiple removals can be compacted at once, though the operation must be manually selected. Tiara could do this, but I have opted to stick with immediate compaction, since most of my use cases do not need this.

The seq of an Ordered map is based on filtering the nils out of the backing vector. The filtering is lazy, and not a significant cost when compared to whatever is using the seq.

##### Linked
This is a significantly different data structure. Each entry is given a node that contains the value, along with references to `left` and `right`. These are the key values to find nodes before and after them, which is an indirection for creating a doubly-linked list. (Indirection is needed for doubly-linked lists in functional data structures, since second updates to nodes will not update any references to the node). The linked list is circular, to allow iteration both forward and back, starting from the stored head of the list.

Storage requires the default `MapEntry` in the backing hashmap, along with a `Node` object that will go to the tail position of the list. This node will set the `right` pointer to the head, and the `left` reference to the previous `tail`. The head must set its `left` to point to the new tail, and the previous tail must update its `right` reference to point to this new tail. Due to indirection, this means looking up the existing `head` and `tail` and then setting their `left` and `right` references, respectively. This is a lot of updates to the backing map, and incurs a significant cost.

Looking up a key retrieves the node with the value, which can be returned immediately. If the entry has been requested (with `find`) then a new `MapEntry` must be constructed.

Removal is similar to insertion, in that the node can be found by a lookup in the tree, and the keys for the nodes to its `left` and `right` can be found. These nodes can be set to refer to each other, thereby removing the target node from the linked list. Finally, the node can be removed from the backing map.

The ordered seq can be built out of the nodes using their linked list. However, due to each node holding only the key of the next node in the list, this means that every step in the iteration requires a map lookup.

### MultiMaps
Multi maps are a common data structure, and one that I have needed on many occasions. While mapping keys to sets of values is very common, this suffers from a lack of composability, and cannot be used with operations like `into` that work on transient variations. This multi-map implementation was written specifically to address these two issues.

## Tests
Some simple [Criterium](https://github.com/hugoduncan/criterium) tests demonstrate the performance differences between the 3 ordered structure implementations. To minimize the non-map working being performed, only numbers are being stored as keys and values.

To avoid access patterns, the initial data was generated with:

```clojure
(def data (shuffle (range 1000000)))
```

The numbers presented here are on JDK 21, running on a 14-inch MacBook Pro, Nov 2023, with an M3 Max chip. Three benchmarks were executed for each operation on each library. The best time out of the 3 was selected, though the results were generally consistent.

### Inserting Data
```clojure
(bench (into (linked.core/map) (map (fn [n] [n n]) data)))
(bench (into (flatland.ordered.map/ordered-map) (map (fn [n] [n n]) data)))
(bench (into (tiara.data/ordered-map) (map (fn [n] [n n]) data)))
```

|Library|Time (ms)|Std Dev (ms)|
|-------|---------|------------|
|Linked|839.4|36.3|
|Ordered|365.4|106.3|
|Tiara|330.3|33.8|

Ordered used to be the fastest here, by a slim margin over Tiara. However, Tiara did not include Transient versions of the data Structures. Since these were introduced, the timing has improved by about 30%. Linked was the slowest by a long way, which reflects the multiple updates required for each insertion. Note that Ordered has a standard deviation of 29%.

**However**, after saving each of these maps (shown in the next test), attempting to run this benchmark on Ordered led to Heap Exhaustion! This problem did not affect either Linked or Tiara. Restarting the repl with a larger heap allowed this test to proceed. This high memory usage also explains the high standard deviation, since heap access (and potentially garbage collection) becomes an issue.

### Random Access
Using the same data, the maps were saved:
```clojure
(def linked (into (linked.core/map) (map (fn [n] [n n]) data)))
(def ordered (into (flatland.ordered.map/ordered-map) (map (fn [n] [n n]) data)))
(def tiara (into (tiara.data/ordered-map) (map (fn [n] [n n]) data)))
```
Using the randomized data as keys, each value was looked up and added to an accumulator. This operation therefore accessed every member of the map:
```clojure
(bench (reduce (fn [acc n] (+ acc (get linked n))) 0 data))
(bench (reduce (fn [acc n] (+ acc (get ordered n))) 0 data))
(bench (reduce (fn [acc n] (+ acc (get tiara n))) 0 data))
```

|Library|Time (ms)|Std Dev (ms)|
|-------|---------|------------|
|Linked|119.3|5.4|
|Ordered|112.6|13.5|
|Tiara|115.0|5.4|

All three are relatively close with Tiara being 4% slower than Ordered. This has changed since 2023, when Tiara used to be slightly faster. Ordered used to be quite a bit slower than the other 2, but has since become the fastest.

### Seq Processing
The reason for this data structure is to be able to return data in order, so the next test was to look at that. Accessing the vals means pulling data out of the ordering, which may be different to getting the full seq, so I looked at both. I started by looking at the vals:
```clojure
(bench (reduce + 0 (vals linked)))
(bench (reduce + 0 (vals ordered)))
(bench (reduce + 0 (vals tiara)))
```

|Library|Time (ms)|Std Dev (ms)|
|-------|---------|------------|
|Linked|332.5|15.7|
|Ordered|48.2|1.3|
|Tiara|18.9|0.4|

Ordered and Tiara are about the same here. The small difference could just be noise, but there did seem to be a consistent difference. This would be due to the wrapping `keep identity` operation over the vector that filters out any `nil` values in Ordered.

Linked is the outlier, due to its need to access the map in each step along the linked list.

### Seq API
Next, I looked at seqs, iterating over each entry, but ignoring the values:
```clojure
(bench (reduce (fn [a _] (inc a)) 0 (seq linked)))
(bench (reduce (fn [a _] (inc a)) 0 (seq ordered)))
(bench (reduce (fn [a _] (inc a)) 0 (seq tiara)))
```

|Library|Time (ms)|Std Dev (ms)|
|-------|---------|------------|
|Linked|322.2|28.5|
|Ordered|6.0|0.1|
|Tiara|2.4|0.1|

Tiara has an advantage in this test, since it already has the seq ready to return, while Ordered needed to create `MapEntry` objects for each element. Ordered still does well though.

### Result
When not removing items, Tiara seems to have slightly better performance than Ordered, and quite a lot better than Linked. While slightly slower (4%) in random access, this is not a common use case for this kind of data structure.

Despite no changes in code, previous tests showed Tiara to be better than Ordered for random access. The difference here is the testing platform. While the newer CPU is likely to benefit all platforms equally, the newer JDK shows much better memory management performance. This allows Ordered to run without heap exhaustion (and used to require a JDK flag for an expanded heap before it could run), and has significantly improved the allocation/deallocation of the `MapEntry` objects that it uses.

Both Linked and Tiara seem to have better memory consumption profiles than Ordered, though JDK 21 appears to remove this as an issue.

For access patterns that do not require significant removals, then Tiara should provide some benefits.


## Future Work
- MultiMaps may use internal set implementations to allow operations like `update` to work.

## License

Copyright © 2023-2025 Paula Gearon

Distributed under the Eclipse Public License version 2.0.
