(ns fun-ds.heap
  (:require [clojure.core.match :refer [match]]))

;;heap {:rank rank :node node :left left :right right}
;;heap nil

(def empty nil)

(defn rank [heap]
  (if (nil? heap) 0
      (:rank heap)))

(defn make-t [x h1 h2]
  (let [r1 (rank h1)
        r2 (rank h2)
        new-heap (fn [r val left right]
                   {:rank r
                    :val val
                    :left left
                    :right right})]

    (if (>= r1 r2) (new-heap (+ r1 1) x h1 h2)
        (new-heap (+ r1 1) x h2 h1))))

(defn merge-t [left right]
  (match [left right]
    [h nil] h
    [nil h] h
    [{:val x1 :left l1 :right r1}
     {:val x2 :left l2 :right r2}] (if (< (compare x1 x2) 0)
                                     (make-t x1 l1 (merge-t r1 right))
                                     (make-t x2 l2 (merge-t left r2)))))

(defn insert [x h]
  (merge-t (make-t x empty empty) h))

(defn find-min [h]
  (:val h))

(defn delete-min [h]
  (if (nil? h) nil
      (merge-t (:left h) (:right h))))

;; insert without merge
(defn insert1 [x h]
  (match [h]
    [nil] (make-t x empty empty)
    [{:val y
      :left left
      :right right}] (if (<= x y)
                       (make-t x left (insert1 y right))
                       (make-t y left (insert1 x right)))))

(defn from-list [xs]
  (let
   [iter1 (fn [hs acc]
            (loop [hs hs
                   acc acc]
              (match [hs]
                [[]] acc
                [[h]] (conj acc h)
                [[fst snd & rest]] (recur
                                    rest
                                    (conj acc (merge-t fst snd))))))
    iter2 (fn [hs]
            (loop [hs (iter1 hs [])]
              (if (< (count hs) 2) hs
                  (recur (iter1 hs [])))))]

    (->> xs
         (map (fn [x] (make-t x nil nil)))
         vec
         iter2
         first)))

(from-list [1 2 3 4 5])
