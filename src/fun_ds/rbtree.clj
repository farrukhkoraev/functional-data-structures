(ns fun-ds.rbtree
  (:require
   [clojure.core.match :refer [match]]
   [clojure.math]))

; color: :red :black
; tree: nil | [color, left, node, right]
; nil tree are black
; Invariant 1. No red node has a red child.
; Invariant 2. Every path from the root to an empty node contains the same number of black nodes.

(def empty-tree nil)

(defn member? [x, tree]
  (match [tree]
    [nil] false
    [[_ left y right]] (cond
                         (< x y) (member? x left)
                         (> x y) (member? x right)
                         :else true)))

(defn balance [color left value right]
  (match [[color left value right]]
    [(:or [:black [:red [:red a x b] y c] z d]
          [:black [:red a x [:red b y c]] z d]
          [:black a x [:red [:red b y c] z d]]
          [:black a x [:red b y [:red c z d]]])] [:red
                                                  [:black a x b]
                                                  y
                                                  [:black c z d]]
    [_] [color left value right]))

(defn insert [x tree]
  (letfn
   [(ins-to [tree]
      (match [tree]
        [nil] [:red nil x nil]
        [[color left y right]] (cond
                                 (< x y) (balance color (ins-to left) y right)
                                 (> x y) (balance color  left y (ins-to right))
                                 :else tree)))]
    (let [[_ l y r] (ins-to tree)]
      [:black l y r])))

; ex 3.9
(defn from-ord-list [xs]
  (letfn
   [(rm-dup [xs]
      (->> xs
           (reduce
            (fn [acc x] (if (or (empty? acc) (not= (first acc) x)) (conj acc x) acc))
            ())
           reverse
           vec))
    (balance [xs]
      (match [xs]
        [([[:red v lt]] :seq)] (list [:black v lt])
        [([[:red v1 lt1] [:red v2 lt2] [:black v3 lt3] & rst] :seq)] (cons [:black v1 lt1]
                                                                           (balance (cons
                                                                                     [:red v2 [:black lt3 v3 lt2]]
                                                                                     rst)))
        [_]  xs))
    (trsfm [xs ts]
      (loop [xs xs
             ts ts]
        (if (empty? xs) ts
            (recur (rest xs)
                   (balance (cons [:red (first xs) nil] ts))))))
    (mk-tree [rtree ts]
      (match [ts]
        [([] :seq)] rtree
        [([[clr v ltree] & rst] :seq)] (mk-tree
                                        [clr ltree v rtree]
                                        rst)))]
    (let [[_ lt v rt] (mk-tree nil (trsfm (rm-dup xs) '()))]
      [:black lt v rt])))

; ex 3.10
(defn lbalance [color left value right]
  (match [[color left value right]]
    [[:black [:red [:red a x b] y c] z d]] [:red
                                            [:black a x b]
                                            y
                                            [:black c z d]]
    [[:black [:red a x [:red b y c]] z d]] [:red
                                            [:black a x b]
                                            y
                                            [:black c z d]]
    [_] [color left value right]))

(defn rbalance [color left value right]
  (match [[color left value right]]
    [[:black a x [:red [:red b y c] z d]]] [:red
                                            [:black a x b]
                                            y
                                            [:black c z d]]
    [[:black a x [:red b y [:red c z d]]]] [:red
                                            [:black a x b]
                                            y
                                            [:black c z d]]
    [_] [color left value right]))

(defn insert2a [x tree]
  (letfn
   [(ins-to [tree]
      (match [tree]
        [nil] [:red nil x nil]
        [[color left y right]] (cond
                                 (< x y) (lbalance color (ins-to left) y right)
                                 (> x y) (rbalance color  left y (ins-to right))
                                 :else tree)))]
    (let [[_ l y r] (ins-to tree)]
      [:black l y r])))

(defn insert2b [x tree]
  (letfn
   [(ins-to [tree]
      (match [tree]
        [nil] [:red nil x nil]
        [[:black left y right]] (cond
                                  (< x y) (lbalance :black (ins-to left) y right)
                                  (> x y) (rbalance :black  left y (ins-to right))
                                  :else tree)
        [[:red left y right]] (cond
                                (< x y) [:red (ins-to left) y right]
                                (> x y) [:red  left y (ins-to right)]
                                :else tree)))]

    (let [[_ l y r] (ins-to tree)]
      [:black l y r])))

(comment (->> empty-tree
              (insert2b 3)
              (insert2b 4)
              (insert2b 5)
              (insert2b 0)
              (insert2b 1))
         (from-ord-list (range 1 10))
         )
