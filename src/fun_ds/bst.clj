(ns fun-ds.bst
  (:require [clojure.core.match :refer [match]]))

(defn member? [x tree]
  (letfn
   [(inner [c tree]
      (match [tree]
        [nil] (= x c)
        [[left y right]] (if (< (compare x y) 0)
                           (inner c left)
                           (inner y right))))]
    (match [tree]
      [[]] false
      [[_ y _]] (inner y tree))))

(defn insert [x tree]
  (letfn
   [(inner [tree]
      (match [tree]
        [nil] [nil x nil]
        [[left y right]] (cond
                           (< x y) [(insert x left) y right]
                           (> x y) [left y (insert x right)]
                           :else (throw Exception))))]
    (try
      (inner tree)
      (catch Exception _
        tree))))

(defn get [x tree]
  (letfn
   [(inner [c tree]
      (match [tree]
        [nil] (if (= x c) x nil)
        [[left y right]] (if (< (compare x y) 0)
                           (inner c left)
                           (inner y right))))]
    (match [tree]
      [[]] false
      [[_ y _]] (inner y tree))))


