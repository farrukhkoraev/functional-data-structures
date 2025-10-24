(ns fun-ds.llazy
  (:require [clojure.core.match :refer [match]]))

(defn ddelay [exp]
  (with-meta (fn [] exp)))

(defn fforce [dly]
  (dly))

; (defmacro lazyfn [name arg & body]
;   `(defn ~name ~arg
;      (ddelay (fforce ~@body))))
;
; (macroexpand-1 '(lazyfn lazy-add [a b]
;                         (+ a b)))
;
; (macroexpand-1
;  '(lazyfn simple-rec [n]
;           (if (= n 0) n
;               (simple-rec (- n 1)))))
;
;-------------------------------------------------------------------------
(def empty-stream nil)

(defn cons-stream
  ([fst rst]
   (list fst  (ddelay rst)))
  ([fst]
   (list fst (ddelay empty-stream))))

(defn empty-stream? [s]
  (or (nil? s) (empty? s)))

(defn head [stream] (first stream))

(defn tail [stream]
  (if (= (count stream) 2)
    (fforce (second stream))
    nil))

(defn concat-stream [s1 s2]
  (if (empty-stream? s1)
    s2
    (cons-stream (head s1)
                 (concat-stream (tail s1) s2))))

(defn take [n stream]
  (if (or (= n 0) (empty-stream? stream)) empty-stream
      (cons-stream (head stream)
                   (take (- n 1)  (tail stream)))))

(defn drop [n stream]
  (if (or (= 0 n) (empty-stream? stream)) stream
      (drop (- n 1) (tail stream))))

(defn reverse-stream [stream]
  (letfn
   [(inner [old new]
      (if (empty-stream? old)
        new
        (inner (tail old)
               (cons-stream (head old) new))))]
    (inner stream empty-stream)))

; ex 4.2
(defn sort-stream [s]
  (letfn
   [; insert x into stream. largest first
    (insert [x s]
      (if (or (empty-stream? s) (>= x (head s)))
        (cons-stream x s)
        (cons-stream (head s) (insert x (tail s)))))

    ; build stream in decreasing order
    (inner [old new]
      (if (empty-stream? old)
        new
        (inner (tail old)
               (insert (head old) new))))]
    (reverse-stream (inner s empty-stream))))

(defn ins [x s]
  (if (or (empty-stream? s) (>= x (head s)))
    (cons-stream x s)
    (cons-stream (head s) (ins x (tail s)))))

(defn to-vector [s]
  (loop [s s
         res []]
    (if (empty-stream? s) res
        (recur (tail s)
               (conj res (head s))))))

(to-vector (sort-stream (concat-stream
                         (cons-stream 4 (cons-stream 2))
                         (cons-stream 2 (cons-stream 3)))))



