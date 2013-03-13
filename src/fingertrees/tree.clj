(ns fingertrees.tree
  (:require [clojure.walk :as walk]))

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

;; (defmacro deftree [& [measure-fn combine-fn]]
;;   (let [measure-is-used (and measure-fn combine-fn)
;;         measure-fields (if measure-is-used
;;                          ['measure-fn 'combine-fn 'v]
;;                          [])
;;         measure-args (if measure-is-used
;;                        [measure-fn combine-fn]
;;                        [])
;;         new-seed (fn [node] `(->Seed ~@measure-args node))
;;         new-tree (fn [l t r] `(->Tree ~@measure-args l t r))]
;;     `(do
;;        (deftype ~'Seed [~@measure-fields ~'node])
;;        (deftype ~'Tree [~@measure-fields ~'left ~'trunk ~'right])
;;        (deftype ~'Seed [~@measure-fields ~'node]
;;          FingerTree
;;          (conj-l [~'_ ~'x]
;;            (if (not (is-full ~'node))
;;              ~(new-seed `(conj-l ~'node ~'x))
;;              (let [[~'l ~'r] (split ~'node)]
;;                ~(new-tree `(conj-l ~'l ~'x) (new-seed `(new-empty ~'node)) 'r))))
;;          (conj-r [~'_ ~'x]
;;            (if (not (is-full ~'node))
;;              ~(new-seed `(conj-r ~'node ~'x))
;;              (let [[~'l ~'r] (split ~'node)]
;;                ~(new-tree 'l (new-seed `(new-empty ~'node)) `(conj-r ~'r ~'x)))))
;;          (head-l [~'_]
;;            (if (not (is-empty ~'node))
;;              (head-l ~'node)))
;;          (tail-l [~'_]
;;            (if (not (is-empty ~'node))
;;              ~(new-seed `(tail-l ~'node))))
;;          (conc [~'_ ~'tree] (reduce conj-l ~'tree (reverse (as-seq ~'node))))
;;          (p [~'_] (str "<Seed" (p ~'node) ">"))
;;          FingerTreeHelper
;;          (conc-reverse [~'_ ~'tree] (reduce conj-r ~'tree (as-seq ~'node))))
;;        (deftype ~Tree [~'left ~'trunk ~'right]
;;          FingerTree
;;          (conj-l [~'_ ~'x]
;;            (if (not (is-full ~'left))
;;              ~(new-tree `(conj-l ~'left ~'x) 'trunk 'right)
;;              (let [[~'l ~'r] (split ~'left)]
;;                ~(new-tree `(conj-l ~'l ~'x) `(conj-l ~'trunk ~'r) 'right))))
;;          (conj-r [~'_ ~'x]
;;            (if (not (is-full ~'right))
;;              ~(new-tree 'left 'trunk `(conj-r ~'right ~'x))
;;              (let [[~'l ~'r] (split ~'right)]
;;                ~(new-tree 'left `(conj-r ~'trunk ~'l) `(conj-r ~'r ~'x)))))
;;          (head-l [~'_]
;;            (head-l ~'left)) ; guaranteed to have at least 1 elem
;;          (tail-l [~'_]
;;            (cond
;;             (not (is-empty (tail-l ~'left)))
;;             ~(new-tree `(tail-l ~'left) 'trunk 'right)
;;             (not (nil? (head-l ~trunk)))
;;             ~(new-tree `(head-l ~trunk) `(tail-l ~trunk) right)
;;             :else
;;             ~(new-seed right)))
;;          (conc [~this ~tree] (conc-reverse ~tree ~this))
;;          (p [~'_] (str "<Tree" (p ~left) "|" (p ~trunk) "|" (p ~right) ">"))
;;          FingerTreeHelper
;;          (conc-reverse [~'_ ~tree]
;;            (let [[~'l1 ~'t1 ~'r1] [(.left ~tree) (.trunk ~tree) (.right ~tree)]
;;                  [~'l2 ~'t2 ~'r2] [~left ~trunk ~right]
;;                  ~'m ~(new-seed `(conj-r (conj-r (new-empty ~'r1) ~'r1) ~'l2))]
;;              ~(new-tree 'l1 `(conc (conc ~'t1 ~'m) ~'t2) 'r2)))))))

(def seed-template
  `(deftype Seed [node]
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
     (conc-reverse [_ tree] (reduce conj-r tree (as-seq node)))))

(def tree-template
  `(deftype Tree [left trunk right]
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
         (->Tree l1 (conc (conc t1 middle-trunk) t2) r2)))))

(defn extract-deftype-names [form]
  (let [[_ _ fields & rest] (vec form)
        params (mapcat second (filter seq? rest))]
    (into (set fields) params)))

(defn unqualify-names [form qualified-names]
  (let [unqualified-names (map (comp symbol name) qualified-names)]
    (walk/prewalk-replace (zipmap qualified-names unqualified-names) form)))

(defn unqualify-let [form]
  (walk/prewalk (fn [x] (if (and (seq? x) (= (first x) `let))
                         (let [[_ bindings & body] x
                               names (flatten (map first (partition 2 bindings)))]
                           (unqualify-names x names))
                         x))
                form))

(defn unqualify-deftype [template]
  (let [symbol-names (extract-deftype-names template)
        deftype-name (second template)
        short-name (-> (name deftype-name)
                       (clojure.string/split #"\.")
                       last symbol)]
    (-> (replace {deftype-name short-name} template)
        (unqualify-names symbol-names)
        (unqualify-let))))

(defn deftype-head [form]
  (let [[_ name fields & rest] form]
    `(deftype ~name ~fields)))

(defmacro deftree []
  (let [[seed tree] (map unqualify-deftype [seed-template tree-template])]
    `(do
       ~(deftype-head seed)
       ~(deftype-head tree)
       ~seed
       ~tree)))

#_(clojure.pprint/pprint (macroexpand-1 '(deftree)))

(deftree)

(extend-type Object
  FingerTree
  (p [this] (str this)))
