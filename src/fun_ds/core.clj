(ns fun-ds.core
  (:require [clojure.core.match :refer [match]]))

(defn -main []
  (let [x true]

    (match [x]
      [true] (println "true")
      [false] (println "false"))))
