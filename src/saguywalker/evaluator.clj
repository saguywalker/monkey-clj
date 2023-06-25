(ns saguywalker.evaluator
  (:require [clojure.pprint :as pprint]
            [saguywalker.ast :as ast]
            [saguywalker.object :as object]
            [saguywalker.token :as token]))

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
      (= node-type token/INT) {:value (:value node)}
      :else nil)))
