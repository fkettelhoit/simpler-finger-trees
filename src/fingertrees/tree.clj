(ns fingertrees.tree)

(defprotocol FingerTree
  (conj-l [_ x])
  (head-l [_])
  (tail-l [_])
  (p [_]))

(defprotocol FingerNode
  (is-full [_])
  (is-empty [_])
  (new-empty [_])
  (split [_]))

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
  (head-l [_]
    (if (not (is-empty node))
      (head-l node)))
  (tail-l [_]
    (if (not (is-empty node))
      (->Seed (tail-l node))))
  (p [_] (str "<Seed" (p node) ">")))

(deftype Tree [left trunk right]
  FingerTree
  (conj-l [_ x]
    (if (not (is-full left))
      (->Tree (conj-l left x) trunk right)
      (let [[l r] (split left)]
        (->Tree (conj-l l x) (conj-l trunk r) right))))
  (head-l [_]
    (head-l left)) ; guaranteed to have at least 1 elem
  (tail-l [_]
    (cond
     (not (is-empty (tail-l left))) (->Tree (tail-l left) trunk right)
     (not (nil? (head-l trunk))) (->Tree (head-l trunk) (tail-l trunk) right)
     :else (->Seed right)))
  (p [_] (str "<Tree" (p left) "|" (p trunk) "|" (p right) ">")))

(extend-type Object
  FingerTree
  (p [this] (str this)))
