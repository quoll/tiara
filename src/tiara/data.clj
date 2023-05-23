(ns ^{:doc "Divergent data structures"
      :author "Paula Gearon"}
  tiara.data
  (:import [clojure.lang AFn MapEntry IPersistentMap IHashEq IObj MapEquivalence]
           [java.util Map]))

;; Note: keySet is unordered. This could be implemented with an ordered set, but is only used
;; when the map is used as a Java object, and so the function is basically never used.
(defn- vec-map
  "Creates an object that implements the required map interfaces, implemented by wrapping
  a hashmap and a vector. This does not extend the abstract class APersistentMap as this
  includes lots of functionality that would be duplicated by the wrapped objects."
  ([] (vec-map {} [] {}))
  ([mp lst idx]
    (proxy [AFn IPersistentMap IHashEq IObj MapEquivalence Map] []
      (assoc [k v]
        (let [new-mp (assoc mp k v)
              kv (find new-mp k)]
          (if (contains? mp k)
            (vec-map new-mp (assoc lst (idx k) kv) idx)
            (vec-map new-mp (conj lst kv) (assoc idx k (count lst))))))
      (assocEx [k v]
        (if (contains? mp k)
          (throw (ex-info "Key already present" {:key k}))
          (assoc this k v)))
      (without [k]
        (if (contains? mp k)
          (vec-map (dissoc mp k) (vec (remove #(= k (first %)) lst)) (dissoc idx k))
          this))
      (containsKey [k] (contains? mp k))
      (entryAt [k] (find mp k))
      (count [] (count lst))
      (iterator [] (.iterator lst))
      (cons [[k v :as o]] (.assoc this k v))
      (empty [] (vec-map))
      (equiv [o] (.equiv mp o))
      (valAt
        ([k] (get mp k))
        ([k not-found] (get mp k not-found)))
      (seq [] (seq lst))
      (hasheq [] (.hasheq mp))
      (invoke
        ([k] (get mp k))
        ([k not-found] (get mp k not-found)))
      (containsValue [v] (some #(= % v) (map val lst)))
      (equals [o] (.equals mp o))
      (get [v] (get mp v))
      (isEmpty [] (empty? lst))
      (size [] (count lst))
      (hashCode [] (hash mp))
      (keySet [] (.keySet mp))
      (values [] (map second lst))
      (withMeta [meta] (vec-map (with-meta mp meta) lst idx))
      (meta [] (meta mp))
      (clear [] (throw (UnsupportedOperationException.)))
      (put [k v] (throw (UnsupportedOperationException.)))
      (putAll [m] (throw (UnsupportedOperationException.)))
      (remove [k] (throw (UnsupportedOperationException.))))))

(defn vreverse
  "Reverses a vector into a vector. Lists are reversed as usual."
  [v]
  (if (vector? v)
    (mapv #(nth v %) (range (dec (count v)) -1 -1))
    (reverse v)))

(defn ordered-map
  "Returns a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] (vec-map))
  ([& keyvals]
   (let [kv-vec (vreverse
                  (second
                    (reduce
                      (fn [[seen? acc] [k v]]
                        (if (seen? k) [seen? acc] [(conj seen? k) (conj acc (MapEntry/create k v))]))
                      [#{} []] (reverse (partition 2 keyvals)))))]
     (vec-map
       (apply hash-map (flatten kv-vec))
       kv-vec
       (apply hash-map (interleave (map first kv-vec) (range)))))))

