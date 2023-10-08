(ns tiara.data
  "Divergent data structures"
  {:author "Paula Gearon"})

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
  (has [this k] (-contains-key? idx k))
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

  ISeqable
  (-seq [this] (seq lst))
  
  ;; IDrop  ;; can introduce this with the next release after CLJS 1.11.60
  ;; (-drop [this n] (drop n lst))

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
  (-kv-reduce [this f init] (reduce (fn [i [k v]] (f i k v)) init lst))

  IReduce
  (-reduce [this f] (#'cljs.core/iter-reduce this f))
  (-reduce [this f init] (#'cljs.core/iter-reduce this f init))

  IFn
  (-invoke [this k] (-lookup this k))
  (-invoke [this k not-found] (-lookup this k not-found))

  IPrintWithWriter
  (-pr-writer [this writer opts] (print-map this pr-writer writer opts))

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

(deftype TransientVecMap [^:mutable lst ^:mutable idx]
  ICounted
  (-count [this] (count lst))
  
  ILookup
  (-lookup [this key]
    (when-let [n (idx key)]
      (val (nth lst n))))
  (-lookup [this key not-found]
    (if-let [n (idx key)]
      (val (nth lst n))
      not-found))

  ITransientCollection
  (-conj! [this o]
    (cond
      (map-entry? o) (-assoc! this (key o) (val o))
      (vector? o) (-assoc! this (nth o 0) (nth o 1))
      :else (loop [es (seq o) coll this]
              (if-let [e (first es)]
                (recur (next es) (-assoc! coll (key e) (val e)))
                coll))))
  (-persistent! [this]
    (VecMap. (persistent! lst) (persistent! idx)))

  ITransientAssociative
  (-assoc! [this k v]
    (if-let [n (get idx k)]
      (if (= (val (nth lst n)) v)
        this
        (let [nlst (-assoc! lst n (MapEntry. k v nil))]
          (if (identical? nlst lst)
            this
            (do
              (set! lst nlst)
              this))))
      (let [len (count lst)
            nlst (-conj! lst (MapEntry. k v nil))
            nidx (-assoc! idx k len)]
        (if (and (identical? nlst lst) (identical? nidx idx))
          this
          (do
            (set! lst nlst)
            (set! idx nidx)
            this)))))

  ITransientMap
  (-dissoc! [this k]
    (if-let [split (get idx k)]
      (let [plst (persistent! lst)
            nlst (reduce conj! (transient (transiable-subvec plst 0 split)) (subvec plst (inc split)))
            nidx (reduce (fn [index n]
                           (let [k (key (nth plst n))]
                             (assoc! index k (dec (get index k)))))
                         (dissoc! idx k)
                         (range (inc split) (count plst)))]
        (set! lst nlst)
        (set! idx nidx)
        this)
      this))

  IFn
  (-invoke [this k] (-lookup this k))
  (-invoke [this k not-found] (-lookup this k not-found)))

(defn- transient-ordered-map
  [lst idx]
  (TransientVecMap. (transient lst) (transient idx)))

(declare transient-ordered-set)

(deftype VecSet [om]
  Object
  (toString [this] (pr-str* this))
  (equiv [this o] (-equiv this o))
  (keys [_] (keys om))
  (entries [_] (keys om))
  (values [_] (vals om))
  (has [_ k] (-contains-key? om k))
  (forEach [_ f] (doseq [[k v] om] (f v k)))

  ICloneable
  (-clone [_] (VecSet. om))

  IIterable
  (-iterator [_] (VecMapIterator. (keys om)))

  IWithMeta
  (-with-meta [this new-meta]
    (if (identical? new-meta (-meta om))
      this
      (VecSet. (with-meta om new-meta))))

  IMeta
  (-meta [_] (-meta om))

  ICollection
  (-conj [_ o] (VecSet. (assoc om o o)))

  IEmptyableCollection
  (-empty [coll] (-with-meta EMPTY_MAP (-meta om)))

  IEquiv
  (-equiv [this other]
    (and
     (set? other)
     (== (-count om) (count other))
     ^boolean
     (try
       (reduce-kv
         #(or (contains? other %2) (reduced false))
         true om)
       (catch js/Error ex
         false))))

  IHash
  (-hash [_] (+ set-magic (-hash om)))

  ISeqable
  (-seq [_] (keys om))

  ICounted
  (-count [_] (-count om))

  ILookup
  (-lookup [this v] (-lookup this v nil))
  (-lookup [_ v not-found]
    (if-let [entry (-find om v)]
      (key entry)
      not-found))

  ISet
  (-disjoin [_ v] (VecSet. (-dissoc om v)))

  IFn
  (-invoke [this k] (-lookup this k))
  (-invoke [this k not-found] (-lookup this k not-found))

  IPrintWithWriter
  (-pr-writer [coll writer opts] (pr-sequential-writer writer pr-writer "#{" " " "}" opts coll)) 

  IEditableCollection
  (-as-transient [coll] (transient-ordered-set om)))

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

(deftype TransientVecSet [^:mutable om]
  ITransientCollection
  (-conj! [this o]
    (set! om (assoc! om o o))
    this)
  (-persistent! [this] (VecSet. (persistent! om)))

  ITransientSet
  (-disjoin! [this v]
    (set! om (dissoc! om v))
    this)

  ICounted
  (-count [this] (count om))

  ILookup
  (-lookup [this v] (-lookup this v nil))

  (-lookup [this v not-found]
    (let [e (-lookup om v sentinel)]
      (if (identical? e sentinel)
        not-found
        (key e))))

  IFn
  (-invoke [this k] (-lookup this k nil))
  (-invoke [this k not-found] (-lookup this k not-found)))

(defn transient-ordered-set
  [os]
  (TransientVecSet. (-as-transient os)))
