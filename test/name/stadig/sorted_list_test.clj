(ns name.stadig.sorted-list-test
  (:require
   [clojure.test :refer [deftest is]]
   [name.stadig.sorted-list :refer [sorted-list]]))

(deftest t-conj
  (is (= [1 2 3] (conj (sorted-list 2 3) 1)))
  (is (= [1 2 3] (conj (sorted-list) 2 3 1))))

(deftest t-get
  (is (= 2 (get (sorted-list 2 3 1) 1))))

(deftest t-nth
  (is (= 2 (nth (sorted-list 2 3 1) 1))))

(deftest t-fn
  (is (= 2 ((sorted-list 2 3 1) 1))))

(deftest t-count
  (is (= 3 (count (sorted-list 2 3 1)))))

(deftest t-kvreduce
  (is (= [[0 1] [1 2] [2 3]] (reduce-kv (fn [r k v] (conj r [k v])) [] (sorted-list 2 3 1)))))

(deftest t-meta
  (is (= [[1 2 3] {:a :b}] ((juxt identity meta) (with-meta (sorted-list 2 3 1) {:a :b})))))

(deftest t-empty
  (is (= [3 4 5] (conj (empty (sorted-list 2 3 1)) 3 5 4))))

(deftest t-peek
  (is (= 3 (peek (sorted-list 2 3 1)))))

(deftest t-pop
  (is (= [1 2] (pop (sorted-list 2 3 1)))))
