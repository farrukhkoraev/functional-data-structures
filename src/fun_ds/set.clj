(ns fun-ds.set
  (:require [clojure.core.match :refer [match]]))

(defn empty [] nil)

(defn member? [x tree]
  (letfn
   [(inner [c tree]
      (match [tree]
        [nil] (= x c)
        [[left y right]] (if (< x y)
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
                           (< x y) [(inner left) y right]
                           (> x y) [left y (inner right)]
                           :else (throw Exception))))]
    (try
      (inner tree)
      (catch Exception _
        tree))))

