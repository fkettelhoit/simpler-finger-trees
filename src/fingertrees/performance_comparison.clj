(ns fingertrees.performance-comparison
  (:require [clojure.data.finger-tree :as chouser]
            [criterium.core :as cr]) 
  (:use [fingertrees.tree :reload true]
        [fingertrees.node :reload true]
        [fingertrees.core :reload true]))

(defn conj-chouser [n]
  (->> (reduce conj (chouser/double-list) (range n))
       (iterate pop)
       (take n)
       (map peek)))

(defn conj-simpler [n]
  (->> (reduce conj-r (new-tree) (range n))
       (iterate tail-l)
       (take n)
       (map head-l)))

(defn conj-vector [n]
  (->> (reduce conj [] (range n))
       (iterate pop)
       (take n)
       (map peek)))

(defn conc-chouser [n]
  (->> (chouser/app3 (reduce conj (chouser/double-list) (range n))
                     []
                     (reduce conj (chouser/double-list) (range n (* n 2))))
       (iterate pop)
       (take (* n 2))
       (map peek)))

(defn conc-simpler [n]
  (->> (conc (reduce conj-r (new-tree) (range n))
             (reduce conj-r (new-tree) (range n (* n 2))))
       (iterate tail-l)
       (take (* n 2))
       (map head-l)))

(defn run [pfx]
  (let [re (re-pattern (str \^ pfx \-))]
    (reduce
      (fn [m [n f]] (assoc m n (first (:mean (cr/quick-benchmark (f 50))))))
      {}
      (filter
        (fn [[k v]] (re-find re (str k)))
        (ns-publics *ns*)))))
