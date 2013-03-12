(ns fingertrees.tree)

(defprotocol FingerTree
  (conj-l [_ x])
  (conj-r [_ x])
  (head-l [_])
  (tail-l [_])
  (conc [_ tree])
  (p [_]))

(defprotocol FingerTreeHelper
  (conc-reverse [_ tree]))

(defprotocol FingerNode
  (is-full [_])
  (is-empty [_])
  (new-empty [_])
  (split [_])
  (as-seq [_]))

;;; declare the types so we can use the "->Type" constructors
(deftype Seed [node])
(deftype Tree [left trunk right])

(deftype Seed [node]
  FingerTree
  (conj-l [_ x]
    (if (not (is-full node))
      (->Seed (conj-l node x))
      (let [[l r] (split node)]
        (->Tree (conj-l l x) (->Seed (new-empty node)) r))))
  (conj-r [_ x]
    (if (not (is-full node))
      (->Seed (conj-r node x))
      (let [[l r] (split node)]
        (->Tree l (->Seed (new-empty node)) (conj-r r x)))))
  (head-l [_]
    (if (not (is-empty node))
      (head-l node)))
  (tail-l [_]
    (if (not (is-empty node))
      (->Seed (tail-l node))))
  (conc [_ tree] (reduce conj-l tree (reverse (as-seq node))))
  (p [_] (str "<Seed" (p node) ">"))
  FingerTreeHelper
  (conc-reverse [_ tree] (reduce conj-r tree (as-seq node))))

(deftype Tree [left trunk right]
  FingerTree
  (conj-l [_ x]
    (if (not (is-full left))
      (->Tree (conj-l left x) trunk right)
      (let [[l r] (split left)]
        (->Tree (conj-l l x) (conj-l trunk r) right))))
  (conj-r [_ x]
    (if (not (is-full right))
      (->Tree left trunk (conj-r right x))
      (let [[l r] (split right)]
        (->Tree left (conj-r trunk l) (conj-r r x)))))
  (head-l [_]
    (head-l left)) ; guaranteed to have at least 1 elem
  (tail-l [_]
    (cond
     (not (is-empty (tail-l left))) (->Tree (tail-l left) trunk right)
     (not (nil? (head-l trunk))) (->Tree (head-l trunk) (tail-l trunk) right)
     :else (->Seed right)))
  (conc [this tree] (conc-reverse tree this))
  (p [_] (str "<Tree" (p left) "|" (p trunk) "|" (p right) ">"))
  FingerTreeHelper
  (conc-reverse [_ tree]
    (let [[l1 t1 r1] [(.left tree) (.trunk tree) (.right tree)]
          [l2 t2 r2] [left trunk right]
          middle-trunk (->Seed (conj-r (conj-r (new-empty r1) r1) l2))]
      (->Tree l1 (conc (conc t1 middle-trunk) t2) r2))))

(extend-type Object
  FingerTree
  (p [this] (str this)))
