(ns tiara.data
  "Divergent data structures"
  {:author "Paula Gearon"}
  (:require-macros [tiara.indirect :refer [indirect indirect-nf]]))

(def ^:const magic 7)
(def ^:const set-magic 11)

(def ^:private sentinel (deref #'cljs.core/lookup-sentinel))
(declare transient-ordered-map)

(deftype VecMapIterator [^:mutable s]
  Object        
  (hasNext [_] (not (nil? s)))
  (next [_]
    (let [ret (first s)]
      (set! s (next s))
      ret)))

(deftype VecMap [lst idx]
  Object
  (toString [this] (pr-str* this))
  (equiv [this other] (-equiv this other))
  (keys [this] (map key lst))
  (entries [this] (seq lst))
  (values [this] (map val lst))
  (has [this k] (contains? idx k))
  (get [this k not-found] (-lookup this k not-found))
  (forEach [coll f]
    (doseq [[k v] coll]
      (f v k)))
  ICloneable
  (-clone [this] (VecMap. lst idx))
  IMeta
  (-meta [this] (.-meta idx))
  IWithMeta
  (-with-meta [this meta] (VecMap. lst (-with-meta idx meta)))
  ICollection
  (-conj [this [k v]] (-assoc this k v))
  IEmptyableCollection
  (-empty [this] (VecMap. [] (with-meta {} (meta idx))))
  IEquiv
  (-equiv [this o]
    (boolean
     (and (map? o)
          (not (record? o))
          (= (count lst) (count o))
          (every? (fn [[k v]] (let [ov (get o k sentinel)]
                                (when-not (identical? ov sentinel)
                                  (= ov v))))
                  lst))))
  IHash
  (-hash [this] (+ magic (hash lst)))
  IIterable
  (-iterator [this] (VecMapIterator. lst))
  ISequable
  (-seq [this] (seq lst))
  IDrop
  (-drop [this n] (drop n lst))
  ICounted
  (-count [this] (count lst))
  ILookup
  (-lookup [this k] (when-let [n (idx k)]
                      (val (nth lst n))))
  (-lookup [this k not-found] (if-let [n (idx k)]
                                (val (nth lst n))
                                not-found))
  IAssociative
  (-assoc [this k v]
    (if-let [n (idx k)]
      (if (= (val (nth lst n)) v)
        this
        (VecMap. (assoc lst n (MapEntry. k v nil)) idx))
      (VecMap. (conj lst (MapEntry. k v nil)) (assoc idx k (count lst)))))
  (-contains-key? [this k] (contains? idx k))
  IFind
  (-find [this k] (when-let [n (idx k)] (nth lst n)))
  IMap
  (-dissoc [this k]
    (if-let [split (get idx k)]
      (VecMap. (into (subvec lst 0 split) (subvec lst (inc split)))
               (reduce (fn [index n]
                         (let [k (key (nth lst n))]
                           (update index k dec)))
                       (dissoc idx k)
                       (range (inc split) (count lst))))
      this))
  IKVReduce
  (-kvreduce [this f init] (reduce (fn [i [k v]] (f i k v)) init lst))
  IReduce
  (-reduce [this f] (#'cljs.core/iter-reduce this f))
  (-reduce [this f init] (#'cljs.core/iter-reduce this f init))
  IFn
  (invoke [this k] (-lookup this k))
  (invoke [this k not-found] (-lookup this k not-found))

  IEditableCollection
  (-as-transient [this] (transient-ordered-map lst idx)))

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
                        (if (seen? k) [seen? acc] [(conj seen? k) (conj acc (MapEntry. k v nil))]))
                      [#{} []] (reverse (partition 2 keyvals)))))]
     (VecMap.
       kv-vec
       (apply hash-map (interleave (map first kv-vec) (range)))))))

(defn- transiable-subvec
  "Get a subvec into a vector that can be made transient."
  [v start end]
  (into [] (subvec v start end)))

;; TODO: from here down
(deftype TransientVecMap [lst idx]
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
  (asTransient [this] (transient-ordered-set om))

  Object
  (toString [this] (clojure.lang.RT/printString this)))

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
  (count [this] (.count ^ITransientMap om))
  (get [this k] (.valAt ^ITransientMap om k))
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
  (contains [this k]
    (boolean (.valAt ^ITransientVector (:lst om) k)))  ;; ensures that nil members are reported correctly
  (persistent [this]
    (VecSet. (.persistent ^TransientVecMap om)))

  IFn
  (invoke [this k] (.get ^ITransientSet this k))
  (invoke [this k not-found] (.valAt ^ITransientMap om k not-found))
  (invoke [this a b & rest] (throw (UnsupportedOperationException.)))
  (applyTo [this s] (case (count s)
                      1 (.get ^ITransientSet this (first s))
                      2 (.valAt ^ITransientMap om (first s) (second s))
                      (throw (clojure.lang.ArityException. (count s) "Set.invoke")))))

(defn transient-ordered-set
  [^IEditableCollection os]
  (TransientVecSet. (.asTransient os)))
