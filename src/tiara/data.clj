(ns tiara.data
  "Divergent data structures"
  {:author "Paula Gearon"}
  (:import [clojure.lang IFn APersistentMap APersistentSet MapEntry ISeq
            IPersistentMap IPersistentSet IHashEq IObj MapEquivalence
            IEditableCollection ITransientVector ITransientMap ITransientSet]
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

(declare transient-ordered-map)

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

  IEditableCollection
  (asTransient [this] (transient-ordered-map lst idx))

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

(defn vreverse
  "Reverses a vector into a vector. Lists are reversed as usual."
  [v]
  (if (vector? v)
    (mapv #(nth v %) (range (dec (count v)) -1 -1))
    (reverse v)))


(defn ordered-map
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] EMPTY_MAP)
  ([& keyvals]
   (let [kv-vec (vreverse
                  (second
                    (reduce
                      (fn [[seen? acc] [k v]]
                        (if (seen? k) [seen? acc] [(conj seen? k) (conj acc (MapEntry/create k v))]))
                      [#{} []] (reverse (partition 2 keyvals)))))]
     (VecMap.
       kv-vec
       (apply hash-map (interleave (map first kv-vec) (range)))))))

(definline transiable-subvec
  "Get a subvec into a vector that can be made transient, skipping some of the steps
   of instance checking that the usual API imposes."
  [v start end]
  `(clojure.lang.LazilyPersistentVector/createOwning (clojure.lang.RT/toArray (subvec ~v ~start ~end))))

(deftype TransientVecMap [^ITransientVector lst ^ITransientMap idx]
  ITransientMap
  (assoc [this k v]
    (if-let [n (get idx k)]
      (if (= (val (nth lst n)) v)
        this
        (let [nlst (.assoc lst n (MapEntry/create k v))]
          (if (identical? nlst lst)
            this
            (TransientVecMap. nlst idx))))
      (let [len (count lst)
            nlst (.conj lst (MapEntry/create k v))
            nidx (.assoc idx k len)]
        (if (and (identical? nlst lst) (identical? nidx idx))
          this
          (TransientVecMap. nlst nidx)))))
  (without [this k]
    (if-let [split (get idx k)]
      (let [plst (persistent! lst)]
        (TransientVecMap. (reduce conj! (transient (transiable-subvec plst 0 split)) (subvec plst (inc split)))
                          (reduce (fn [index n]
                                    (let [k (key (nth plst n))]
                                      (assoc! index k (dec (get index k)))))
                                  (dissoc! idx k)
                                  (range (inc split) (count plst)))))
      this))
  (valAt [this key]
    (when-let [n (idx key)]
      (val (nth lst n))))
  (valAt [this key not-found]
    (if-let [n (idx key)]
      (val (nth lst n))
      not-found))
  (count [this] (count lst))
  (persistent [this] (VecMap. (.persistent lst) (.persistent idx)))
  (conj [this [k v]] (.assoc this k v))

  IFn
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  (invoke [this a b & rest] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (.valAt this (first s))
                      2 (.valAt this (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Map.invoke")))))

(defn transient-ordered-map
  [^IEditableCollection lst ^IEditableCollection idx]
  (TransientVecMap. (.asTransient lst) (.asTransient idx)))

(declare transient-ordered-set)

(deftype VecSet [^VecMap om]
  IFn
  (invoke [this k] (.invoke om k))
  (invoke [this k not-found] (.invoke om k not-found))
  (invoke [this a b & rest] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (.invoke om (first s))
                      2 (.invoke om (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Set.invoke"))))

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
  (meta [this] (.meta om))

  IEditableCollection
  (asTransient [this] (transient-ordered-set om)))

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

(deftype TransientVecSet [^TransientVecMap om]
  ITransientSet
  (count [this] (.count om))
  (get [this k] (.get om k))
  (disjoin [this k]
    (let [nom (.without om k)]
      (if (identical? nom om)
        this
        (TransientVecSet. nom))))
  (conj [this k]
    (let [nom (.assoc om k k)]
      (if (identical? nom om)
        this
        (TransientVecSet. nom))))
  (contains [this k]
    (boolean (.get (:lst om) k)))  ;; ensures that nil members are reported correctly
  (persistent [this]
    (VecSet. (.persistent om)))

  IFn
  (invoke [this k] (.get this k))
  (invoke [this k not-found] (.get this k not-found))
  (invoke [this a b & rest] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (.get this (first s))
                      2 (.get this (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Set.invoke")))))

(defn transient-ordered-set
  [^IEditableCollection os]
  (TransientVecSet. (.asTransient os)))
