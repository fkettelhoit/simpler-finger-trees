(ns fingertrees.core)

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

(defprotocol FingerTree
  (conj-l [_ x])
  (head-l [_])
  (tail-l [_])
  (p [_]))

(defprotocol FingerNode
  (full [_])
  (is-empty [_])
  (split [_]))


  ;(let [[l m r] (split right)] (Tree. l (Seed. m) r))

(deftype Seed [node]
  FingerTree
  (conj-l [_ x]
    (if (not (full node))
      (->Seed (conj-l node x))
      (let [[l t r] (split node)]
        (->Tree (conj-l l x) (->Seed t) r))))
  (head-l [_]
    (if (is-empty node)
      nil
      (head-l node)))
  (tail-l [_]
    (if (is-empty node)
      (pop []) ; find a better error message here
      (->Seed (tail-l node))))
  (p [_] (str "<Seed" (p node) ">")))

(deftype Tree [left trunk right]
  FingerTree
  (conj-l [_ x]
    (if (not (full left))
      (->Tree (conj-l left x) trunk right)
      (let [[l t r] (split left)]
        (->Tree (conj-l l x) (conj-l trunk r) right))))
  (head-l [_]
    (head-l left)) ; guaranteed to have at least 1 elem
  (tail-l [_]
    (cond
     (not (is-empty (tail-l left))) (->Tree (tail-l left) trunk right)
     (not (nil? (head-l trunk))) (->Tree (head-l trunk) (tail-l trunk) right)
     :else (->Seed right)))
  (p [_] (str "<Tree" (p left) "|" (p trunk) "|" (p right) ">")))

(deftype Node4 [v]
  FingerTree
  (conj-l [_ x] (->Node4 (vec (cons x v))))
  (head-l [_] (first v))
  (tail-l [_] (->Node4 (vec (rest v))))
  (p [_] (str "<Node " (apply str (interpose " " (map p v))) ">"))
  FingerNode
  (full [_] (> (count v) 3))
  (is-empty [_] (empty? v))
  (split [_] [(->Node4 (subvec v 0 2)) (->Node4 []) (->Node4 (subvec v 2))]))

(extend-type Object
  FingerTree
  (p [this] (str this)))

(defn new-tree []
  (Seed. (Node4. [])))

(defn conj-and-peek [head-fn tail-fn conj-fn n]
  (map head-fn (take n (iterate tail-fn
                                (reduce conj-fn (new-tree) (range n))))))

(p (reduce conj-l (new-tree) (range 4)))

(map p (take 5 (iterate tail-l (reduce conj-l (new-tree) (range 5)))))

(conj-and-peek head-l tail-l conj-l 200)

(.v ((split (reduce conj-l (Node4. []) (range 4))) 2))

(p (reduce conj-l (new-tree) (range 20)))

(p (reduce conj-l (new-tree) (range 7)))

(->
 (reduce conj-l (new-tree) (range 7))
 tail-l
 tail-l
 tail-l
 p)
