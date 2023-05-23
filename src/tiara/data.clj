(ns ^{:doc "Divergent data structures"
      :author "Paula Gearon"}
  tiara.data
  (:import [clojure.lang AFn APersistentMap MapEntry IPersistentMap IHashEq IObj MapEquivalence]
           [java.util Map]))

(def ^:const magic 7)

(definline indirect
  "This get operation is used in multiple places, and can be inlined.
  Possibly redundant with (indirect-nd index entry-vector k nil)"
  [index entry-vector k]
  `(when-let [n# (~index ~k)]
     (val (nth ~entry-vector n#))))

(definline indirect-nf
  "This alternative get operation is used in multiple places, and can be inlined."
  [index entry-vector k not-found]
  `(if-let [n# (~index ~k)]
     (val (nth ~entry-vector n#))
     ~not-found))

(definterface Debug
  (dp []))

(defn- vec-map
  "Creates an object that implements the required map interfaces, implemented by wrapping
  a hashmap and a vector. This does not extend the abstract class APersistentMap as this
  includes lots of functionality that would be duplicated by the wrapped objects."
  ([] (vec-map [] {}))
  ([lst idx]
    (proxy [AFn IPersistentMap IHashEq IObj MapEquivalence Map Debug] []
      (assoc [k v]
        (if-let [n (idx k)]
          (if (= (val (nth lst n)) v)
            this
            (vec-map (assoc lst n (MapEntry/create k v)) idx))
          (vec-map (conj lst (MapEntry/create k v)) (assoc idx k (count lst)))))
      (assocEx [k v]
        (if (contains? idx k)
          (throw (ex-info "Key already present" {:key k}))
          (.assoc this k v)))
      (dp []
        (println " lst:" lst)
        (println " idx:" idx))
      (without [k]
        (if-let [split (get idx k)]
          (vec-map (into (subvec lst 0 split) (subvec lst (inc split)))
                   (reduce (fn [index n]
                             (let [k (key (nth lst n))]
                               (update index k dec)))
                           (dissoc idx k)
                           (range (inc split) (count lst))))
          this))
      (containsKey [k] (contains? idx k))
      (entryAt [k] (when-let [n (idx k)] (nth lst n)))
      (count [] (count lst))
      (iterator [] (.iterator lst))
      (cons [[k v]] (.assoc this k v))
      (empty [] (vec-map))
      (equiv [o]
        (if (instance? IPersistentMap o)
          (and (instance? MapEquivalence o) (APersistentMap/mapEquals this o))
          (APersistentMap/mapEquals this o)))
      (valAt
        ([k] (indirect idx lst k))
        ([k not-found] (indirect-nf idx lst k not-found)))
      (seq [] (seq lst))
      (hasheq [] (+ magic (.hasheq lst)))
      (invoke
        ([k] (indirect idx lst k))
        ([k not-found] (indirect-nf idx lst k not-found)))
      (containsValue [v] (some #(= % v) (map val lst)))
      (equals [o] (.equiv this o))
      (get [k] (indirect idx lst k))
      (isEmpty [] (empty? lst))
      (size [] (count lst))
      (hashCode [] (+ magic (hash lst)))
      (keySet [] (map key lst))
      (values [] (map val lst))
      (withMeta [meta] (vec-map lst (with-meta idx meta)))
      (meta [] (meta idx))
      (clear [] (throw (UnsupportedOperationException.)))
      (put [k v] (throw (UnsupportedOperationException.)))
      (putAll [m] (throw (UnsupportedOperationException.)))
      (remove [k] (throw (UnsupportedOperationException.))))))

(def EMPTY_MAP (vec-map))

(defn vreverse
  "Reverses a vector into a vector. Lists are reversed as usual."
  [v]
  (if (vector? v)
    (mapv #(nth v %) (range (dec (count v)) -1 -1))
    (reverse v)))

(defn ordered-map
  "Returns a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] EMPTY_MAP)
  ([& keyvals]
   (let [kv-vec (vreverse
                  (second
                    (reduce
                      (fn [[seen? acc] [k v]]
                        (if (seen? k) [seen? acc] [(conj seen? k) (conj acc (MapEntry/create k v))]))
                      [#{} []] (reverse (partition 2 keyvals)))))]
     (vec-map
       kv-vec
       (apply hash-map (interleave (map first kv-vec) (range)))))))

