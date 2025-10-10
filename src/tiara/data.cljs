(ns tiara.data
  "Divergent data structures"
  {:author "Paula Gearon"})

(def ^:const magic 7)
(def ^:const set-magic 11)

(def ^:private sentinel (deref #'cljs.core/lookup-sentinel))

(declare transient-ordered-map transient-ordered-set transient-multi-map)

(deftype MapEntryIterator [^:mutable s]
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
  (keys [_] (map -key lst))
  (entries [_] (-seq lst))
  (values [_] (map -val lst))
  (has [_ k] (-contains-key? idx k))
  (get [this k not-found] (-lookup this k not-found))
  (forEach [coll f]
    (doseq [[k v] coll]
      (f v k)))

  ICloneable
  (-clone [_] (VecMap. lst idx))

  IMeta
  (-meta [_] (-meta idx))

  IWithMeta
  (-with-meta [_ meta] (VecMap. lst (-with-meta idx meta)))

  ICollection
  (-conj [this [k v]] (-assoc this k v))

  IEmptyableCollection
  (-empty [_] (VecMap. [] (-with-meta {} (-meta idx))))

  IEquiv
  (-equiv [_ o]
    (boolean
     (and (map? o)
          (not (record? o))
          (= (-count lst) (count o))
          (every? (fn [[k v]] (let [ov (get o k sentinel)]
                                (when-not (identical? ov sentinel)
                                  (= ov v))))
                  lst))))

  IHash
  (-hash [_] (+ magic (hash lst)))

  IIterable
  (-iterator [_] (MapEntryIterator. lst))

  ISeqable
  (-seq [_] (-seq lst))
  
  IDrop
  (-drop [_ n] (-drop n lst))

  ICounted
  (-count [_] (-count lst))

  ILookup
  (-lookup [_ k] (when-let [n (-lookup idx k)]
                      (val (-nth lst n))))
  (-lookup [_ k not-found] (if-let [n (-lookup idx k)]
                                (-val (-nth lst n))
                                not-found))
  IAssociative
  (-assoc [this k v]
    (if-let [n (-lookup idx k)]
      (if (= (-val (-nth lst n)) v)
        this
        (VecMap. (-assoc lst n (MapEntry. k v nil)) idx))
      (VecMap. (conj lst (MapEntry. k v nil)) (-assoc idx k (count lst)))))
  (-contains-key? [_ k] (-contains-key? idx k))

  IFind
  (-find [_ k] (when-let [n (idx k)] (-nth lst n)))

  IMap
  (-dissoc [this k]
    (if-let [split (-lookup idx k)]
      (VecMap. (into (subvec lst 0 split) (subvec lst (inc split)))
               (reduce (fn [index n]
                         (let [k (key (nth lst n))]
                           (update index k dec)))
                       (-dissoc idx k)
                       (range (inc split) (count lst))))
      this))

  IKVReduce
  (-kv-reduce [_ f init] (reduce (fn [i [k v]] (f i k v)) init lst))

  IReduce
  (-reduce [this f] (#'cljs.core/iter-reduce this f))
  (-reduce [this f init] (#'cljs.core/iter-reduce this f init))

  IFn
  (-invoke [this k] (-lookup this k))
  (-invoke [this k not-found] (-lookup this k not-found))

  IPrintWithWriter
  (-pr-writer [this writer opts] (print-map this #'cljs.core/pr-writer writer opts))

  IEditableCollection
  (-as-transient [_] (transient-ordered-map lst idx)))

(def EMPTY_MAP (VecMap. [] {}))

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
       (zipmap ks (range))))))

(defn- transiable-subvec
  "Get a subvec into a vector that can be made transient."
  [v start end]
  (into [] (subvec v start end)))

(deftype TransientVecMap [^:mutable lst ^:mutable idx]
  ICounted
  (-count [_] (-count lst))

  ILookup
  (-lookup [_ key]
    (when-let [n (-lookup idx key)]
      (val (-nth lst n))))
  (-lookup [_ key not-found]
    (if-let [n (-lookup idx key)]
      (val (-nth lst n))
      not-found))

  ITransientCollection
  (-conj! [this o]
    (cond
      (map-entry? o) (-assoc! this (-key o) (-val o))
      (vector? o) (-assoc! this (-nth o 0) (-nth o 1))
      :else (loop [es (-seq o) coll this]
              (if-let [e (first es)]
                (recur (next es) (-assoc! coll (-key e) (-val e)))
                coll))))
  (-persistent! [_]
    (VecMap. (persistent! lst) (persistent! idx)))

  ITransientAssociative
  (-assoc! [this k v]
    (if-let [n (.get idx k)]
      (when (not= (val (-nth lst n)) v)
        (let [nlst (-assoc! lst n (MapEntry. k v nil))]
          (when-not (identical? nlst lst)
            (set! lst nlst))))
      (let [len (count lst)
            nlst (-conj! lst (MapEntry. k v nil))
            nidx (-assoc! idx k len)]
        (when-not (and (identical? nlst lst) (identical? nidx idx))
          (set! lst nlst)
          (set! idx nidx))))
    this)

  ITransientMap
  (-dissoc! [this k]
    (when-let [split (.get idx k)]
      (let [plst (-persistent! lst)
            nlst (reduce conj! (transient (transiable-subvec plst 0 split)) (subvec plst (inc split)))
            nidx (reduce (fn [index n]
                           (let [k (-key (nth plst n))]
                             (.assoc! index k (dec (get index k)))))
                         (.dissoc! idx k)
                         (range (inc split) (count plst)))]
        (set! lst nlst)
        (set! idx nidx)))
    this)

  IFn
  (-invoke [this k] (-lookup this k))
  (-invoke [this k not-found] (-lookup this k not-found)))

(defn- transient-ordered-map
  [lst idx]
  (TransientVecMap. (transient lst) (transient idx)))

(deftype VecSet [om]
  Object
  (toString [this] (pr-str* this))
  (equiv [this o] (-equiv this o))
  (keys [_] (.keys om))
  (entries [_] (.keys om))
  (values [_] (.values om))
  (has [_ k] (-contains-key? om k))
  (forEach [_ f] (doseq [[k v] om] (f v k)))

  ICloneable
  (-clone [_] (VecSet. om))

  IIterable
  (-iterator [_] (MapEntryIterator. (keys om)))

  IWithMeta
  (-with-meta [this new-meta]
    (if (identical? new-meta (-meta om))
      this
      (VecSet. (-with-meta om new-meta))))

  IMeta
  (-meta [_] (-meta om))

  ICollection
  (-conj [_ o] (VecSet. (assoc om o o)))

  IEmptyableCollection
  (-empty [_] (-with-meta EMPTY_MAP (-meta om)))

  IEquiv
  (-equiv [_ other]
    (and
     (set? other)
     (== (-count om) (count other))
     ^boolean
     (try
       (reduce-kv
         #(or (contains? other %2) (reduced false))
         true om)
       (catch js/Error _
         false))))

  IHash
  (-hash [_] (+ set-magic (-hash om)))

  ISeqable
  (-seq [_] (.keys om))

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
  (-pr-writer [coll writer opts] (pr-sequential-writer writer #'cljs.core/pr-writer "#{" " " "}" opts coll)) 

  IEditableCollection
  (-as-transient [_] (transient-ordered-set om)))

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
    (set! om (-assoc! om o o))
    this)
  (-persistent! [_] (VecSet. (persistent! om)))

  ITransientSet
  (-disjoin! [this v]
    (set! om (dissoc! om v))
    this)

  ICounted
  (-count [_] (-count om))

  ILookup
  (-lookup [this v] (-lookup this v nil))

  (-lookup [_ v not-found]
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

(deftype MultiMap [m count*]
  Object
  (toString [this] (pr-str* this))
  (equiv [this other] (-equiv this other))
  (keys [_] (keys m))
  (entries [this] (-seq this))
  (values [_] (apply concat (vals m)))
  (has [_ k] (-contains-key? m k))
  (get [this k not-found] (-lookup this k not-found))
  (forEach [_ f]
    (doseq [[k vs] m v vs]
      (f v k)))

  ICloneable
  (-clone [_] (MultiMap. m count*))

  IMeta
  (-meta [_] (-meta m))

  IWithMeta
  (-with-meta [_ meta] (MultiMap. (-with-meta m meta) count*))

  ICollection
  (-conj [this [k v]] (-assoc this k v))

  IEmptyableCollection
  (-empty [_] (MultiMap. (with-meta {} (-meta m)) 0))

  IEquiv
  (-equiv [this o]
    (boolean
     (and (map? o)
          (not (record? o))
          (= (-count this) (count o))
          (every? (fn [[k v]] (let [ov (get o k sentinel)]
                                (when-not (identical? ov sentinel)
                                  (= ov v))))
                  m))))

  IHash
  (-hash [_] (+ magic (-hash m)))

  IIterable
  (-iterator [this] (MapEntryIterator. (-seq this)))

  ISeqable
  (-seq [_] (for [[k vs] m v vs] (MapEntry. k v nil)))

  IDrop
  (-drop [_ n] (when-let [s (-seq m)]
                 (drop n s)))

  ICounted
  (-count [_] count*)

  ILookup
  (-lookup [_ k] (-lookup m k))
  (-lookup [_ k not-found] (-lookup m k not-found))
  IAssociative
  (-assoc [this k v]
    (if-let [vs (-lookup m k)]
      (if (contains? vs v)
        this
        (MultiMap. (-assoc m k (-conj vs v)) (inc count*)))
      (MultiMap. (-assoc m k #{v}) (inc count*))))
  (-contains-key? [_ k] (-contains-key? m k))

  IFind
  (-find [_ k] (-find m k))

  IMap
  (-dissoc [this entry]
    (if-let [[k v] (cond
                     (map-entry? entry) [(-key entry) (-val entry)]
                     (and (vector? entry) (= 2 (count entry))) [(first entry) (second entry)])]
      (if-let [vs (-lookup m k)]
        (if (contains? vs v)
          (let [vsn (-disjoin vs v)]
            (MultiMap. (if (seq vsn) (-assoc m k vsn) (-dissoc m k)) (dec count*)))
          this)
        this)
      (if-let [vs (-lookup m entry)]
        (MultiMap. (-dissoc m entry) (- count* (-count vs)))
        this)))

  IKVReduce
  (-kv-reduce [this f init] (reduce (fn [a [k v]] (f a k v)) init (-seq this)))

  IReduce
  (-reduce [this f] (#'cljs.core/iter-reduce this f))
  (-reduce [this f init] (#'cljs.core/iter-reduce this f init))

  IFn
  (-invoke [this k] (-lookup this k))
  (-invoke [this k not-found] (-lookup this k not-found))

  IPrintWithWriter
  (-pr-writer [this writer opts] (print-map this #'cljs.core/pr-writer writer opts))

  IEditableCollection
  (-as-transient [_] (transient-multi-map m)))

(deftype TransientMultiMap [^:mutable m ^:mutable count*]
  ICounted
  (-count [_] count*)

  ILookup
  (-lookup [_ key]
    (-lookup m key))
  (-lookup [_ key not-found]
    (-lookup m key not-found))

  ITransientCollection
  (-conj! [this [k v]]
    (-assoc! this k v))
  (-persistent! [_]
    (MultiMap. (-persistent! m) count*))

  ITransientAssociative
  (-assoc! [this k v]
    (if-let [vs (-lookup m k)]
      (let [nvs (-conj vs v)]
        (when-not (identical? vs nvs)
          (let [nm (-assoc! m k nvs)]
            (set! m nm)
            (set! count* (inc count*)))))
      (let [nm (-assoc! m k #{v})]
        (set! m nm)
        (set! count* (inc count*))))
    this)

  ITransientMap
  (-dissoc! [this entry]
    (if-let [[k v] (cond
                     (map-entry? entry) [(-key entry) (-val entry)]
                     (and (vector? entry) (= 2 (count entry))) [(first entry) (second entry)])]
      (when-let [vs (m k)]
        (let [vsn (-disjoin vs v)]
          (when-not (identical? vsn vs)
            (let [nm (if (seq vsn)
                       (-assoc! m k vsn)
                       (-dissoc! m k))]
              (set! m nm)
              (set! count* (dec count*))))))
      (when-let [vs (-lookup m entry)]
        (let [nm (-dissoc! m entry)]
          (set! m nm)
          (set! count* (- count* (-count vs))))))
    this)

  IFn
  (-invoke [this k] (-lookup this k))
  (-invoke [this k not-found] (-lookup this k not-found)))

(defn transient-multi-map
  [m]
  (TransientMultiMap. m (count m)))

(def EMPTY_MULTI_MAP (MultiMap. {} 0))

(defn multi-map
  "Creates a map object that accepts multiple values per key"
  ([] EMPTY_MULTI_MAP)
  ([& keyvals]
   (let [kvs (partition 2 keyvals)]
     (into EMPTY_MULTI_MAP (map #(MapEntry. (key %) (val %) nil)) kvs))))
