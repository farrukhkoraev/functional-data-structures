(ns fun-ds.map
  (:require
   [fun-ds.bst :as t]
   [clojure.core.match :refer [match]]))

;; [left [k v] right]

(defn empty [] nil)

(defn bind [k v tree]
  (letfn
   [(inner [tree]
      (match [tree]
        [nil] [nil [k v] nil]
        [[left val right]] (cond
                             (< k (first val)) [(inner left) val right]
                             (> k (first val)) [left val (inner right)]
                             :else (if (= v (second val)) (throw Exception)
                                       [left [k v] right]))))]
    (try
      (inner tree)
      (catch Exception _
        tree))))

(defn lookup [k map]
  (letfn
   [(inner [c tree]
      (match [tree]
        [nil] (if (= k (first c)) (second c) nil)
        [[left y right]] (if (< k (first y))
                           (inner c left)
                           (inner y right))))]
    (match [map]
      [[]] false
      [[_ y _]] (inner y map))))

(let [m (empty)
      m (bind 1 3 m)
      m (bind 0 4 m)]
  (lookup 0 m))

