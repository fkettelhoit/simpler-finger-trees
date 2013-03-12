(ns fingertrees.node
  (:use [fingertrees [tree :only [FingerTree FingerNode]]]))

(defmacro predefine-nodes [capacity]
  `(do
     ~@(for [size (range (+ capacity 2))]
         (let [name (symbol (str "Node" size))
               fields (map #(symbol (str "n" %)) (range size))]
           `(deftype ~name [~@fields])))))

#_(clojure.pprint/pprint (macroexpand-1 '(predefine-nodes 4)))

(defmacro node-ctor [& items]
  (let [name (symbol (str "Node" (count items) "."))]
    `(~name ~@items)))

(defmacro define-nodes [capacity]
  `(do
     ~@(for [size (range (inc capacity))]
         (let [name (symbol (str "Node" size))
               half (+ (quot size 2) (rem size 2))
               fields (map #(symbol (str "n" %)) (range size))
               [fields-l fields-r] (split-at half fields)]
           `(deftype ~name [~@fields]
              FingerTree
              (conj-l [_ x#] (node-ctor x# ~@fields))
              (conj-r [_ x#] (node-ctor ~@fields x#))
              (head-l [_] ~(first fields))
              (tail-l [_] (node-ctor ~@(rest fields)))
              (p [_]
                (str "<Node "
                     (apply str (interpose " " (map
                                                fingertrees.tree/p
                                                ~(vec fields))))
                     ">"))
              FingerNode
              (new-empty [_] (node-ctor))
              (is-full [_] ~(if (= size capacity) true false))
              (is-empty [_] ~(if (= size 0) true false))
              (split [_] [(node-ctor ~@fields-l) (node-ctor ~@fields-r)]))))))

#_(clojure.pprint/pprint (macroexpand-1 '(define-nodes 4)))

(predefine-nodes 32)
(define-nodes 32)
