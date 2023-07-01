(ns saguywalker.evaluator
  (:require [clojure.pprint :as pprint]
            [saguywalker.ast :as ast]
            [saguywalker.object :as object]
            [saguywalker.token :as token]))

(def TRUE (object/boolean-obj true))
(def FALSE (object/boolean-obj false))

(defn- native-bool-to-bool-obj [b]
  (if b
    TRUE
    FALSE))

(defn eval-node [node]
  (let [node-type (get-in node [:token :type])]
    (cond
      ;; statements
      (contains? node :statements) (reduce (fn [_ stmt]
                                             (eval-node stmt))
                                           nil
                                           (:statements node))
      (contains? node :expression) (eval-node (:expression node))
      ;; expressions
      (or (= node-type token/TRUE)
          (= node-type token/FALSE)) (native-bool-to-bool-obj (:value node))
      (= node-type token/INT) (object/integer-obj (:value node))
      :else nil)))

