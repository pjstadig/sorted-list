(ns name.stadig.sorted-list
  (:require
   [clojure.core.rrb-vector :as v])
  (:import
   (clojure.lang
     Counted
     IDrop
     #_IEditableCollection
     IFn
     IHashEq
     IKVReduce
     ILookup
     IMeta
     IObj
     IPersistentCollection
     IPersistentStack
     IReduce
     IReduceInit
     ;; ITransientCollection
     ;; ITransientVector
     Indexed
     Reversible
     Seqable
     Sequential)
   (java.lang Runnable)
   (java.io Serializable)
   (java.util Collection List RandomAccess)
   (java.util.concurrent Callable)))

(set! *warn-on-reflection* true)

(declare ->SortedList)

(defn- index-of
  [vv cmp o]
  (letfn [(index-of* [start end]
            (if (> start end)
              (if (and (< -1 start (count vv))
                    (zero? (cmp o (nth vv start))))
                start
                (- -1 start))
              (let [mid (quot (+ start end) 2)
                    c   (cmp o (nth vv mid))]
                (cond
                  (neg? c)
                  (index-of* start (dec mid))
                  (zero? c)
                  (let [i (index-of* start (dec mid))]
                    (if (neg? i)
                      mid
                      i))
                  :else
                  (index-of* (inc mid) end)))))]
    (index-of* 0 (dec (count vv)))))

(defn- last-index-of
  [vv cmp o]
  (letfn [(last-index-of* [start end]
            (if (> start end)
              (if (and (< -1 start (count vv))
                    (zero? (cmp o (nth vv start))))
                start
                (- -1 start))
              (let [mid (quot (+ start end) 2)
                    c   (cmp o (nth vv mid))]
                (cond
                  (neg? c)
                  (last-index-of* start (dec mid))
                  (zero? c)
                  (let [i (last-index-of* (inc mid) end)]
                    (if (neg? i)
                      mid
                      i))
                  :else
                  (last-index-of* (inc mid) end)))))]
    (last-index-of* 0 (dec (count vv)))))

(defn- insert-at
  [vv i o]
  (v/catvec (v/subvec vv 0 i) [o] (v/subvec vv i (count vv))))

;; I would like to implement a transient version, but rrb-vectors are not splitable and catable as
;; transients.
#_
(deftype TransientSortedList
    [vv cmp]
  Counted
  (count [_]
    (.count ^Counted vv))

  IFn
  (invoke [_ o]
    (.invoke ^IFn vv o))

  ILookup
  (valAt [_ k]
    (.valAt ^ILookup vv k))
  (valAt [_ k not-found]
    (.valAt ^ILookup vv k not-found))

  ITransientCollection
  (conj [_ o]
    (let [i (index-of vv cmp o)]
      (if (neg? i)
        (TransientSortedList. (insert-at vv (- -1 i) o) cmp)
        (TransientSortedList. (insert-at vv i o) cmp))))
  (persistent [_]
    (->SortedList (.persistent ^ITransientCollection vv) cmp))

  ITransientVector
  (assocN [_ _ _]
    ;; We support pop from IPersistentStack, but there's no ITransientStack only ITransientVector
    (throw (UnsupportedOperationException.)))
  (pop [_]
    (.pop ^ITransientVector vv))

  Indexed
  (nth [_ i]
    (.nth ^Indexed vv i))
  (nth [_ i not-found]
    (.nth ^Indexed vv i not-found))

  Object
  (equals [_ o]
    (.equals vv o))
  (hashCode [_]
    (.hashCode vv))
  (toString [_]
    (.toString vv))

  Runnable
  (run [_]
    (.run ^Runnable vv))

  Callable
  (call [_]
    (.call ^Callable vv)))

(deftype SortedList
    [vv cmp]
  Counted
  (count [_]
    (.count ^Counted vv))

  ;; IEditableCollection
  ;; (asTransient [_]
  ;;   (->TransientSortedList (.asTransient ^IEditableCollection vv) cmp))

  IFn
  (invoke [_ k]
    (.invoke ^IFn vv k))

  IHashEq
  (hasheq [_]
    (.hasheq ^IHashEq vv))

  IKVReduce
  (kvreduce [_ f init]
    (reduce-kv f init vv))

  ILookup
  (valAt [_ k]
    (.valAt ^ILookup vv k))
  (valAt [_ k not-found]
    (.valAt ^ILookup vv k not-found))

  IMeta
  (meta [_]
    (.meta ^IMeta vv))

  IObj
  (withMeta [_ meta]
    (SortedList. (.withMeta ^IObj vv meta) cmp))

  IPersistentCollection
  (cons [_ o]
    (let [i (index-of vv cmp o)]
      (if (neg? i)
        (SortedList. (insert-at vv (- -1 i) o) cmp)
        (SortedList. (insert-at vv i o) cmp))))
  (empty [_]
    (SortedList. (.empty ^IPersistentCollection vv) cmp))
  (equiv [_ o]
    (.equiv ^IPersistentCollection vv o))

  IPersistentStack
  (peek [_]
    (.peek ^IPersistentStack vv))
  (pop [_]
    (.pop ^IPersistentStack vv))

  IReduce
  (reduce [_ f]
    (.reduce ^IReduce vv f))

  IReduceInit
  (reduce [_ f start]
    (.reduce ^IReduceInit vv f start))

  Indexed
  (nth [_ i]
    (.nth ^Indexed vv i))
  (nth [_ i not-found]
    (.nth ^Indexed vv i not-found))

  Reversible
  (rseq [_]
    (.rseq ^Reversible vv))

  Seqable
  (seq [_]
    (.seq ^Seqable vv))

  Sequential
  ;; marker interface

  Serializable
  ;; marker interface

  Comparable
  (compareTo [_ o]
    (.compareTo ^Comparable vv o))

  Iterable
  (iterator [_]
    (.iterator ^Iterable vv))

  Object
  (equals [_ o]
    (.equals vv o))
  (hashCode [_]
    (.hashCode vv))
  (toString [this]
    (.toString (.seq this)))

  Runnable
  (run [_]
    (.run ^Runnable vv))

  Collection
  (add [_ o]
    (.add ^Collection vv o))
  (addAll [_ c]
    (.addAll ^Collection vv c))
  (clear [_]
    (.clear ^Collection vv))
  (contains [_ o]
    (.contains ^Collection vv o))
  (containsAll [_ c]
    (.containsAll ^Collection vv c))
  (isEmpty [_]
    (.isEmpty ^Collection vv))
  (^boolean remove [_ ^Object o]
   (.remove ^Collection vv o))
  (removeAll [_ c]
    (.removeAll ^Collection vv c))
  (retainAll [_ c]
    (.retainAll ^Collection vv c))
  (size [_]
    (.size ^Collection vv))
  (toArray [_]
    (.toArray ^Collection vv))
  (^"[Ljava.lang.Object;" toArray [_ ^"[Ljava.lang.Object;" a]
   (.toArray ^Collection vv a))

  List
  (add [_ i v]
    (.add ^List vv i v))
  (addAll [_ i c]
    (.addAll ^List vv i c))
  (get [_ i]
    (.get ^List vv i))
  (indexOf [_ o]
    (let [i (index-of vv cmp o)]
      (if (neg? i)
        -1
        i)))
  (lastIndexOf [_ o]
    (let [i (last-index-of vv cmp o)]
      (if (neg? i)
        -1
        i)))
  (listIterator [_]
    (.listIterator ^List vv))
  (listIterator [_ i]
    (.listIterator ^List vv i))
  (remove [_ ^int i]
    (.remove ^List vv i))
  (set [_ i o]
    (.set ^List vv i o))
  (subList [_ from to]
    (.subList ^List vv from to))

  RandomAccess
  ;; marker interface

  Callable
  (call [_]
    (.call ^Callable vv)))

(defn sorted-list-by
  [comparator & values]
  (into (->SortedList [] comparator) values))

(defn sorted-list
  [& values]
  (apply sorted-list-by compare values))
