(ns ^{:doc "Divergent data structures"
      :author "Paula Gearon"}
  tiara.data
  (:import [clojure.lang IFn APersistentMap APersistentSet MapEntry ISeq
            IPersistentMap IPersistentSet IHashEq IObj MapEquivalence]
           [java.util Map Set Collection]))

(def ^:const magic 7)
(def ^:const set-magic 11)

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

(deftype VecMap [lst idx]
  IFn
  (invoke [this k] (indirect idx lst k))
  (invoke [this k not-found] (indirect-nf idx lst k not-found))
  (invoke [this a b & rest] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (indirect idx lst (first s))
                      2 (indirect-nf idx lst (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Map.invoke"))))

  IPersistentMap
  (assoc [this k v]
    (if-let [n (idx k)]
      (if (= (val (nth lst n)) v)
        this
        (VecMap. (assoc lst n (MapEntry/create k v)) idx))
      (VecMap. (conj lst (MapEntry/create k v)) (assoc idx k (count lst)))))
  (assocEx [this k v]
    (if (contains? idx k)
      (throw (ex-info "Key already present" {:key k}))
      (.assoc this k v)))
  (without [this k]
    (if-let [split (get idx k)]
      (VecMap. (into (subvec lst 0 split) (subvec lst (inc split)))
               (reduce (fn [index n]
                         (let [k (key (nth lst n))]
                           (update index k dec)))
                       (dissoc idx k)
                       (range (inc split) (count lst))))
      this))
  (iterator [this] (.iterator ^Collection (seq this)))
  (containsKey [this k] (contains? idx k))
  (entryAt [this k] (when-let [n (idx k)] (nth lst n)))
  (count [this] (count lst))
  (cons [this [k v]] (.assoc this k v))
  (empty [this] (VecMap. [] {}))
  (equiv [this o]
    (if (instance? IPersistentMap o)
      (and (instance? MapEquivalence o) (APersistentMap/mapEquals this o))
      (APersistentMap/mapEquals this o)))
  (seq [this] (seq lst))
  (valAt [this k] (indirect idx lst k))
  (valAt [this k not-found] (indirect-nf idx lst k not-found))

  IHashEq
  (hasheq [this] (+ magic (.hasheq ^IHashEq lst)))
  
  IObj
  (withMeta [this meta] (VecMap. lst (.withMeta ^IObj idx meta)))
  (meta [this] (.meta ^IObj idx))

  MapEquivalence

  Map
  (clear [this] (throw (UnsupportedOperationException.)))
  (containsValue [this v] (some #(= % v) (map val lst)))
  (equals [this o] (.equiv this o))
  (get [this k] (indirect idx lst k))
  (hashCode [this] (+ magic (hash lst)))
  (isEmpty [this] (empty? lst))
  (keySet [this] (set (map key lst)))
  (size [this] (count lst))
  (values [this] (map val lst))
  (put [this k v] (throw (UnsupportedOperationException.)))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (remove [this k] (throw (UnsupportedOperationException.))))

(def EMPTY_MAP (VecMap. [] {}))

(defn ordered-map
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] EMPTY_MAP)
  ([& keyvals]
   (let [m (apply hash-map keyvals)
         ks (if (= (* 2 (count m)) (count keyvals))
              (into [] (take-nth 2) keyvals)
              (into () (distinct) (into () (take-nth 2) keyvals)))]
     (VecMap.
       (mapv #(find m %) ks)
       (zipmap ks (range))))))

(deftype VecSet [^VecMap om]
  IFn
  (invoke [this k] (.invoke om k))
  (invoke [this k not-found] (.invoke om k not-found))
  (invoke [this a b & rest] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (.invoke om (first s))
                      2 (.invoke om (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Map.invoke"))))

  IPersistentSet
  (disjoin [this k] (VecSet. (.without om k)))
  (contains [this k] (.containsKey om k))
  (get [this k] (.get om k))
  (count [this] (.count om))
  (cons [this o] (VecSet. (.assoc om o o)))
  (empty [this] (VecSet. EMPTY_MAP))
  (equiv [this o] (APersistentSet/setEquals this o))
  (seq [this] (keys om))

  Set
  (add [this e] (throw (UnsupportedOperationException.)))
  (addAll [this c] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (containsAll [this c] (every? om c))
  (equals [this o] (APersistentSet/setEquals this o))
  (hashCode [this] (+ set-magic (.hashCode om)))
  (isEmpty [this] (.isEmpty om))
  (iterator [this] (.iterator ^Collection (keys om)))
  (remove [this o] (throw (UnsupportedOperationException.)))
  (removeAll [this c] (throw (UnsupportedOperationException.)))
  (retainAll [this c] (throw (UnsupportedOperationException.)))
  (size [this] (.count om))
  (toArray [this] (clojure.lang.RT/seqToArray (keys om)))

  IHashEq
  (hasheq [this] (+ set-magic (.hasheq om)))

  IObj
  (withMeta [this meta] (VecSet. (.withMeta om meta)))
  (meta [this] (.meta om)))

(def EMPTY_SET (VecSet. EMPTY_MAP))

(defn ordered-set
  "Creates a set object that remembers the insertion order, similarly to a java.util.LinkedHashSet"
  ([] EMPTY_SET)
  ([& s]
   (VecSet. (apply ordered-map (mapcat #(repeat 2 %) s)))))

(defn oset
  "Convenience function to create an ordered set from a seq"
  [s]
  (apply ordered-set s))
