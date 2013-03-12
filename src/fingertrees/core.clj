(ns fingertrees.core
  (:use [fingertrees.tree :reload true]
        [fingertrees.node :reload true]))

;;; Ok, lets start to break down finger trees into simpler components.
;;; First of all we need to distinguish between the simple containers
;;; that hold the elements and the tree structure.
;;; 
;;; The containers could be any data structure (such as vectors,
;;; lists, etc.) and are not finger tree specific. They simply
;;; determine the constant time factors we get.
;;;
;;; The tree structure takes these containers and keeps them either in
;;; branches (left and right) or in the trunk. Containers in the
;;; branches can only store elements (or sub-containers), but never
;;; "blossom" and become a tree themselves. Such a transformation into
;;; a full blown tree is possible for the container in the trunk
;;; though.

#_(deftype Node4 [v]
  FingerTree
  (conj-l [_ x] (->Node4 (vec (cons x v))))
  (head-l [_] (first v))
  (tail-l [_] (->Node4 (vec (rest v))))
  (p [_] (str "<Node " (apply str (interpose " " (map p v))) ">"))
  FingerNode
  (is-full [_] (> (count v) 3))
  (is-empty [_] (empty? v))
  (new-empty [_] (Node4. []))
  (split [_] [(->Node4 (subvec v 0 2)) (->Node4 (subvec v 2))]))

(defn new-tree []
  (->Seed (->Node0)))

(defn conj-and-peek [head-fn tail-fn conj-fn n]
  (->> (reduce conj-fn (new-tree) (range n))
       (iterate tail-fn)
       (take n)
       (map head-fn)))

(assert
 (= (conj-and-peek head-l tail-l conj-l 1000)
    (range 999 -1 -1)))

(assert
 (= (conj-and-peek head-l tail-l conj-r 1000)
    (range 1000)))