(ns fingertrees.core
  (:use [fingertrees.tree :reload true]
        [fingertrees.node :reload true]))

(defn new-tree []
  (->Seed nil (->Node0)))

(defn conj-and-peek [head-fn tail-fn conj-fn n]
  (->> (reduce conj-fn (new-tree) (range n))
       (iterate tail-fn)
       (take n)
       (map head-fn)))

(assert
 (= (conj-and-peek head-l tail-l conj-l 1000)
    (range 999 -1 -1)))

(assert
 (= (->> (conc (reduce conj-r (new-tree) (range 500))
               (reduce conj-r (new-tree) (range 500 1000)))
         (iterate tail-l)
         (take 1000)
         (map head-l))
    (range 1000)))

(assert
 (= (conj-and-peek head-l tail-l conj-r 1000)
    (range 1000)))