(ns fun-ds.bin-heap
  (:require [clojure.core.match :refer [match]]))

;; binomial leftist heap

;;tree {:rank rank :val val :children [x & xs]}
;;heap [tree] 

(defn node [rank val children]
  {:rank rank :val val :children children})

(defn link [t1 t2]
  (match
   [t1 t2]
    [{:rank r :val v1 :children ts1}
     {:val v2 :children ts2}] (if (<= v1 v2)
                                (node (+ r 1) v1 (into [t2] ts1))
                                (node (+ r 1) v2 (into [t1] ts2)))))

(defn rank [tree]
  (:rank tree))

(defn root [tree]
  (:val tree))

(defn ins-tree [t ts]
  (loop [t t
         ts ts]
    (match
     [ts]
      [[]] [t]
      [[t' & ts']] (if (< (rank t) (rank t'))
                     (into [t] ts)
                     (recur (link t t') ts')))))
(defn insert [x heap]
  (ins-tree (node 0 x []) heap))

(defn mmerge [h1 h2]
  (match
   [h1 h2]
    [h []] h
    [[] h] h
    [[t1 & ts1]
     [t2 & ts2]] (cond
                   (< (rank t1)
                      (rank t2)) (into [t1] (mmerge ts1 h2))
                   (> (rank t1)
                      (rank t2)) (into [t2] (mmerge ts2 h1))
                   :else (ins-tree (link t1 t2) (mmerge ts1 ts2)))))

(defn remove-min-tree [heap]
  (match [heap]
    [[t]] [t []]
    [[t & ts]] (let [[t' ts'] (remove-min-tree ts)]
                 (if (<= (root t) (root t'))
                   [t ts]
                   [t' (into [t] ts')]))))

(defn find-min [heap]
  (root (first (remove-min-tree heap))))

(defn delete-min [heap]
  (let [[t ts] (remove-min-tree heap)]
    (mmerge (vec (reverse (:children t))) ts)))

(defn find-min' [heap]
  (match [heap]
    [[t]] (root t)
    [[t & ts]] (min (root t) (find-min' ts))))

(let [t1 (node 0 2 [])
      t2 (node 0 1 [])]
  (find-min' (mmerge [t1] [t2])))

;------------------

;;tree {:val val :children [x & xs]}
;;heap [(rank tree)] 

(defn node2 [val children]
  {:val val :children children})

(defn link2 [t1 t2]
  (match
   [t1 t2]
    [{:val v1 :children ts1}
     {:val v2 :children ts2}] (if (<= v1 v2)
                                (node2 v1 (into [t2] ts1))
                                (node2 v2 (into [t1] ts2)))))

(defn root [tree]
  (:val tree))

(defn insert2 [x heap])
(defn mmerge2 [heap])
(defn find-min2 [heap])
(defn delete-min2 [heap])
