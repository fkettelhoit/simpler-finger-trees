(ns fingertrees.performance-comparison
  (:require [clojure.data.finger-tree :as chouser]
            [criterium.core :as criterium])
  (:use [fingertrees.tree ;:reload true
         ]
        [fingertrees.node ;:reload true
         ]
        [fingertrees.core ;:reload true
         ]))

(defn chouser-conj [n]
  (->> (reduce conj (chouser/double-list) (range n))
       (iterate pop)
       (take n)
       (map peek)))

(defn simpler-conj [n]
  (->> (reduce conj-r (new-tree) (range n))
       (iterate tail-l)
       (take n)
       (map head-l)))

(defn vector-conj [n]
  (->> (reduce conj [] (range n))
       (iterate pop)
       (take n)
       (map peek)))

(defn chouser-conc [n]
  (->> (chouser/app3 (reduce conj (chouser/double-list) (range n))
                     []
                     (reduce conj (chouser/double-list) (range n (* n 2))))
       (iterate pop)
       (take (* n 2))
       (map peek)))

(defn simpler-conc [n]
  (->> (conc (reduce conj-r (new-tree) (range n))
             (reduce conj-r (new-tree) (range n (* n 2))))
       (iterate tail-l)
       (take (* n 2))
       (map head-l)))

(defn bench-multiple [n & fns]
  (println "------------------------")
  (println "-- Starting Benchmark --")
  (println "------------------------")
  (doseq [f fns]
    (criterium/quick-bench (f n))
    (println "------------------------"))
  (println "-- Benchmark finished --")
  (println "------------------------"))

(bench-multiple
 20000
 chouser-conj simpler-conj vector-conj)

(bench-multiple
 20000
 chouser-conc simpler-conc)