(ns fingertrees.performance-comparison
  (:require [clojure.data.finger-tree :as chouser])
  (:use [fingertrees.tree :reload true]
        [fingertrees.node :reload true]
        [fingertrees.core :reload true]))

(let [n 20000
      runs 50]
  (time
   (dotimes [_ runs]
     (->> (reduce conj (chouser/double-list) (range n))
          (iterate pop)
          (take n)
          (map peek))))
  (time
   (dotimes [_ runs]
     (->> (reduce conj-r (new-tree) (range n))
          (iterate tail-l)
          (take n)
          (map head-l))p))
  (println "---------------"))

(let [n 20000
      runs 50]
  (time
   (dotimes [_ runs]
     (->> (chouser/app3 (reduce conj (chouser/double-list) (range n))
                        []
                        (reduce conj (chouser/double-list) (range n (* n 2))))
          (iterate pop)
          (take (* n 2))
          (map peek))))
  (time
   (dotimes [_ runs]
     (->> (conc (reduce conj-r (new-tree) (range n))
                (reduce conj-r (new-tree) (range n (* n 2))))
          (iterate tail-l)
          (take (* n 2))
          (map head-l))))
  (println "---------------"))
