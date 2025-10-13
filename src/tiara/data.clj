(ns tiara.data
  "Divergent data structures"
  {:author "Paula Gearon"}
  (:import [clojure.lang IFn APersistentMap APersistentSet MapEntry
            IPersistentMap IPersistentSet IHashEq IObj MapEquivalence
            IEditableCollection ITransientVector ITransientMap ITransientSet]
           [java.util Map Map$Entry Set Collection]))

(def ^:const magic 7)
(def ^:const set-magic 11)

(definline indirect
  "This get operation is used in multiple places, and can be inlined.
  Possibly redundant with (indirect-nf index entry-vector k nil)"
  [index entry-vector k]
  `(when-let [n# (~index ~k)]
     (.val ^MapEntry (nth ~entry-vector n#))))

(definline indirect-nf
  "This alternative get operation is used in multiple places, and can be inlined."
  [index entry-vector k not-found]
  `(if-let [n# (~index ~k)]
     (.val ^MapEntry (nth ~entry-vector n#))
     ~not-found))

(declare transient-ordered-map)

(deftype VecMap [lst idx ^int _hash]
  IFn
  (invoke [_ k] (indirect idx lst k))
  (invoke [_ k not-found] (indirect-nf idx lst k not-found))
  (invoke [_ _ _ & _] (throw (UnsupportedOperationException.)))
  (applyTo [_ s] (case (count s)
                   1 (indirect idx lst (first s))
                   2 (indirect-nf idx lst (first s) (second s))
                   (throw (clojure.lang.ArityException. (count s) "Map.invoke"))))

  IPersistentMap
  (assoc [this k v]
    (if-let [n (idx k)]
      (if (= (.val ^MapEntry (nth lst n)) v)
        this
        (VecMap. (assoc lst n (MapEntry/create k v)) idx 0))
      (VecMap. (conj lst (MapEntry/create k v)) (assoc idx k (count lst)) 0)))
  (assocEx [this k v]
    (if (contains? idx k)
      (throw (ex-info "Key already present" {:key k}))
      (.assoc this k v)))
  (without [this k]
    (if-let [split (get idx k)]
      (VecMap. (into (subvec lst 0 split) (subvec lst (inc split)))
               (reduce (fn [index n]
                         (let [k (.key ^MapEntry (nth lst n))]
                           (update index k dec)))
                       (dissoc idx k)
                       (range (inc split) (count lst)))
               0)
      this))
  (iterator [_] (.iterator ^Collection lst))
  (containsKey [_ k] (contains? idx k))
  (entryAt [_ k] (when-let [n (idx k)] (nth lst n)))
  (count [_] (count lst))
  (cons [this [k v]] (.assoc this k v))
  (empty [_] (VecMap. [] (.withMeta ^IObj {} (.meta ^IObj idx)) 0))
  (equiv [this o]
    (if (instance? IPersistentMap o)
      (and (instance? MapEquivalence o) (APersistentMap/mapEquals this o))
      (APersistentMap/mapEquals this o)))
  (seq [_] (seq lst))
  (valAt [_ k] (indirect idx lst k))
  (valAt [_ k not-found] (indirect-nf idx lst k not-found))

  IEditableCollection
  (asTransient [_] (transient-ordered-map lst idx))

  IHashEq
  (hasheq [this]
    (if (zero? _hash)
      (set! (. this _hash) (clojure.lang.Murmur3/hashUnordered this))
      _hash))
  
  IObj
  (withMeta [_ meta] (VecMap. lst (.withMeta ^IObj idx meta) 0))
  (meta [_] (.meta ^IObj idx))

  MapEquivalence

  Map
  (clear [_] (throw (UnsupportedOperationException.)))
  (containsValue [_ v] (some #(= % v) (map val lst)))
  (equals [this o] (.equiv this o))
  (get [_ k] (indirect idx lst k))
  (hashCode [_] (+ magic (hash lst)))
  (isEmpty [_] (empty? lst))
  (keySet [_] (set (map key lst)))
  (size [_] (count lst))
  (values [_] (map val lst))
  (put [_ _ _] (throw (UnsupportedOperationException.)))
  (putAll [_ _] (throw (UnsupportedOperationException.)))
  (remove [_ _] (throw (UnsupportedOperationException.)))

  Object
  (toString [this] (clojure.lang.RT/printString this)))

(def EMPTY_MAP (VecMap. [] {} 0))

(defn ordered-map
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] EMPTY_MAP)
  ([& keyvals]
   (let [m (apply hash-map keyvals)
         ks (if (= (* 2 (count m)) (count keyvals))
              (take-nth 2 keyvals) 
              (into [] (comp (take-nth 2) (distinct)) keyvals))]
     (VecMap.
      (mapv #(find m %) ks)
      (zipmap ks (range))
      0))))

(definline transiable-subvec
  "Get a subvec into a vector that can be made transient, skipping some of the steps
   of instance checking that the usual API imposes."
  [v start end]
  `(clojure.lang.LazilyPersistentVector/createOwning (clojure.lang.RT/toArray (subvec ~v ~start ~end))))

(deftype TransientVecMap [^ITransientVector lst ^ITransientMap idx]
  ITransientMap
  (assoc [this k v]
    (if-let [n (idx k)]
      (if (= (.val ^MapEntry (nth lst n)) v)
        this
        (let [nlst (.assoc ^ITransientVector lst n (MapEntry/create k v))]
          (if (identical? nlst lst)
            this
            (TransientVecMap. nlst idx))))
      (let [len (count lst)
            nlst (.conj ^ITransientVector lst (MapEntry/create k v))
            nidx (.assoc ^ITransientMap idx k (Long. len))]
        (if (and (identical? nlst lst) (identical? nidx idx))
          this
          (TransientVecMap. nlst nidx)))))
  (without [this k]
    (if-let [split (get idx k)]
      (let [plst (persistent! lst)]
        (TransientVecMap. (reduce conj! (transient (transiable-subvec plst 0 split)) (subvec plst (inc split)))
                          (reduce (fn [index n]
                                    (let [k (key (nth plst n))]
                                      (assoc! index k (dec (index k)))))
                                  (dissoc! idx k)
                                  (range (inc split) (count plst)))))
      this))
  (valAt [_ key]
    (when-let [n (idx key)]
      (val (nth lst n))))
  (valAt [_ key not-found]
    (if-let [n (idx key)]
      (val (nth lst n))
      not-found))
  (count [_] (count lst))
  (persistent [_] (VecMap. (.persistent lst) (.persistent idx) 0))
  (conj [this [k v]] (.assoc this k v))

  IFn
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  (invoke [_ _ _ & _] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (.valAt this (first s))
                      2 (.valAt this (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Map.invoke")))))

(defn- transient-ordered-map
  [^IEditableCollection lst ^IEditableCollection idx]
  (TransientVecMap. (.asTransient lst) (.asTransient idx)))

(declare transient-ordered-set)

(deftype VecSet [^VecMap om ^int _hash]
  IFn
  (invoke [_ k] (.invoke om k))
  (invoke [_ k not-found] (.invoke om k not-found))
  (invoke [_ _ _ & _] (throw (UnsupportedOperationException.)))
  (applyTo [_ s] (case (count s)
                   1 (.invoke om (first s))
                   2 (.invoke om (first s) (second s))
                   (throw (clojure.lang.ArityException. (count s) "Set.invoke"))))

  IPersistentSet
  (disjoin [_ k] (VecSet. (.without om k) 0))
  (contains [_ k] (.containsKey om k))
  (get [_ k] (.get om k))
  (count [_] (.count om))
  (cons [_ o] (VecSet. (.assoc om o o) 0))
  (empty [_] (VecSet. EMPTY_MAP 0))
  (equiv [this o] (APersistentSet/setEquals this o))
  (seq [_] (keys om))

  Set
  (add [_ _] (throw (UnsupportedOperationException.)))
  (addAll [_ _] (throw (UnsupportedOperationException.)))
  (clear [_] (throw (UnsupportedOperationException.)))
  (containsAll [_ c] (every? om c))
  (equals [this o] (APersistentSet/setEquals this o))
  (hashCode [_] (+ set-magic (.hashCode om)))
  (isEmpty [_] (.isEmpty om))
  (iterator [_] (clojure.lang.SeqIterator. (keys om)))
  (remove [_ _] (throw (UnsupportedOperationException.)))
  (removeAll [_ _] (throw (UnsupportedOperationException.)))
  (retainAll [_ _] (throw (UnsupportedOperationException.)))
  (size [_] (.count om))
  (toArray [_] (clojure.lang.RT/seqToArray (keys om)))

  IHashEq
  (hasheq [this]
    (if (zero? _hash)
      (set! (. this _hash) (clojure.lang.Murmur3/hashUnordered (keys om)))
      _hash))

  IObj
  (withMeta [_ meta] (VecSet. (.withMeta om meta) _hash))
  (meta [_] (.meta om))

  IEditableCollection
  (asTransient [_] (transient-ordered-set om))

  Object
  (toString [this] (clojure.lang.RT/printString this)))

(def EMPTY_SET (VecSet. EMPTY_MAP 0))

(defn ordered-set
  "Creates a set object that remembers the insertion order, similarly to a java.util.LinkedHashSet"
  ([] EMPTY_SET)
  ([& s]
   (VecSet. (apply ordered-map (mapcat #(repeat 2 %) s)) 0)))

(defn oset
  "Convenience function to create an ordered set from a seq"
  [s]
  (apply ordered-set s))

(deftype TransientVecSet [^TransientVecMap om]
  ITransientSet
  (count [_] (.count ^ITransientMap om))
  (get [_ k] (.valAt ^ITransientMap om k))
  (disjoin [this k]
    (let [nom (.without ^ITransientMap om k)]
      (if (identical? nom om)
        this
        (TransientVecSet. nom))))
  (conj [this k]
    (let [nom (.assoc ^ITransientMap om k k)]
      (if (identical? nom om)
        this
        (TransientVecSet. nom))))
  (contains [_ k]
    (boolean (.valAt ^ITransientVector (:lst om) k)))  ;; ensures that nil members are reported correctly
  (persistent [_]
    (VecSet. (.persistent ^TransientVecMap om) 0))

  IFn
  (invoke [this k] (.get ^ITransientSet this k))
  (invoke [_ k not-found] (.valAt ^ITransientMap om k not-found))
  (invoke [_ _ _ & _] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (.get ^ITransientSet this (first s))
                      2 (.valAt ^ITransientMap om (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Set.invoke")))))

(defn transient-ordered-set
  [^IEditableCollection os]
  (TransientVecSet. (.asTransient os)))

(declare transient-multi-map)

(defn- convert-to-multi
  "Utility function to convert a standard map to a multi-map structure."
  [m]
  (into {} (map (fn [[k v]] [k #{v}])) m))

(deftype MultiMap [m ^int _hash ^int count*]
  IFn
  (invoke [_ k] (m k))
  (invoke [_ k not-found] (m k not-found))
  (invoke [_ _ _ & _] (throw (UnsupportedOperationException.)))
  (applyTo [_ s] (case (count s)
                   1 (m (first s))
                   2 (m (first s) (second s))
                   (throw (clojure.lang.ArityException. (count s) "Map.invoke"))))

  IPersistentMap
  (assoc [this k v]
    (if-let [n (m k)]
      (if (contains? n v)
        this
        (MultiMap. (assoc m k (conj n v)) 0 (inc count*)))
      (MultiMap. (assoc m k #{v}) 0 (inc count*))))
  (assocEx [this k v]
    (when-let [vs (contains? m k)]
      (when (contains? vs v)
        (throw (ex-info "Entry is already present" {:key k :value v}))))
    (.assoc this k v))
  (without [this entry]
    (if-let [[k v] (cond
                     (instance? Map$Entry entry) [(.getKey ^Map$Entry entry) (.getValue ^Map$Entry entry)]
                     (instance? MapEntry entry) [(first entry) (second entry)]
                     (and (vector? entry) (= 2 (count entry))) [(first entry) (second entry)])]
      (if-let [vs (m k)]
        (if (contains? vs v)
          (let [vsm (disj vs v)]
            (if (seq vsm)
              (MultiMap. (assoc m k vsm) 0 (dec count*))
              (MultiMap. (dissoc m k) 0 (dec count*))))
          this)
        this)
      (if-let [vs (get m entry)]
        (MultiMap. (dissoc m entry) 0 (- count* (count vs)))
        this)))
  (iterator [_] (.iterator ^Collection (for [[k vs] m v vs] (MapEntry/create k v))))
  (containsKey [_ k] (contains? m k))
  (entryAt [_ k] (.entryAt m k))
  (count [_] count*)
  (cons [this [k v]] (.assoc this k v))
  (empty [_] (MultiMap. (.withMeta ^IObj {} (.meta ^IObj m)) 0 0))
  (equiv [_ o]
    (cond
      (instance? MultiMap o) (APersistentMap/mapEquals m (.m o))
      (instance? IPersistentMap o) (when (instance? MapEquivalence o)
                                     (or (APersistentMap/mapEquals m o)
                                         (APersistentMap/mapEquals m (convert-to-multi o))))
      :else (APersistentMap/mapEquals m o)))
  (seq [_] (for [[k vs] m v vs] (MapEntry/create k v)))
  (valAt [_ k] (m k))
  (valAt [_ k not-found] (m k not-found))

  IEditableCollection
  (asTransient [_] (transient-multi-map m count*))

  IHashEq
  (hasheq [this]
    (if (zero? _hash)
      (set! (. this _hash) (clojure.lang.Murmur3/hashUnordered this))
      _hash))

  IObj
  (withMeta [_ meta] (MultiMap. (.withMeta ^IObj m meta) _hash count*))
  (meta [_] (.meta ^IObj m))

  MapEquivalence

  Map
  (clear [_] (throw (UnsupportedOperationException.)))
  (containsValue [_ v] (some #(contains? % v) (vals m)))
  (equals [this o] (.equiv this o))
  (get [_ k] (m k))
  (hashCode [_] (+ magic (hash m)))
  (isEmpty [_] (empty? m))
  (keySet [_] (set (keys m)))
  (size [this] (.count this))
  (values [_] (apply concat (vals m)))
  (put [_ _ _] (throw (UnsupportedOperationException.)))
  (putAll [_ _] (throw (UnsupportedOperationException.)))
  (remove [_ _] (throw (UnsupportedOperationException.)))

  Object
  (toString [this] (clojure.lang.RT/printString this)))

(deftype TransientMultiMap [^ITransientMap m count*]
  ITransientMap
  (assoc [this k v]
    (if-let [vs (get m k)]
      (if (contains? vs v)
        this
        (TransientMultiMap. (assoc! m k (conj vs v)) (inc count*)))
      (TransientMultiMap. (assoc! m k #{v}) (inc count*))))
  (without [this entry]
    (if-let [[k v] (cond
                     (instance? Map$Entry entry) [(.getKey ^Map$Entry entry) (.getValue ^Map$Entry entry)]
                     (instance? MapEntry entry) [(key entry) (val entry)]
                     (and (vector? entry) (= 2 (count entry))) [(first entry) (second entry)])]
      (if-let [vs (get m k)]
        (if (contains? vs v)
          (let [vsm (disj vs v)]
            (if (seq vsm)
              (TransientMultiMap. (assoc! m k vsm) (dec count*))
              (TransientMultiMap. (dissoc! m k) (dec count*))))
          this)
        this)
      (if-let [vs (get m entry)]
        (TransientMultiMap. (dissoc! m entry) (- count* (count vs)))
        this)))
  (valAt [_ key] (get m key))
  (valAt [_ key not-found] (get m key not-found))
  (count [_] count*)
  (persistent [_] (MultiMap. (.persistent m) 0 count*))
  (conj [this [k v]] (.assoc this k v))

  IFn
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  (invoke [_ _ _ & _] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (.valAt this (first s))
                      2 (.valAt this (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Map.invoke")))))

(defn- transient-multi-map
  ([^IEditableCollection m]
   (transient-multi-map m (apply + (map count (vals m)))))
  ([^IEditableCollection m count*]
   (TransientMultiMap. (.asTransient m) count*)))

(def EMPTY_MULTI_MAP (MultiMap. {} 0 0))

(defn multi-map
  "Creates a map object that accepts multiple values per key"
  ([] EMPTY_MULTI_MAP)
  ([& keyvals]
   (let [kvs (partition 2 keyvals)]
     (into EMPTY_MULTI_MAP (map #(apply MapEntry/create %)) kvs))))
