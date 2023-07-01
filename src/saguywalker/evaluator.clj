(ns saguywalker.evaluator
  (:require [clojure.pprint :as pprint]
            [saguywalker.ast :as ast]
            [saguywalker.object :as object]
            [saguywalker.token :as token]))

(def TRUE (object/boolean-obj true))
(def FALSE (object/boolean-obj false))
(def NULL object/null-obj)

(defn- native-bool-to-bool-obj [b]
  (if b
    TRUE
    FALSE))

(defn eval-bang-operator-expression
  [right]
  (cond
    (= right TRUE) FALSE
    (or (= right FALSE)
        (= right NULL)) TRUE
    :else FALSE))

(defn eval-minus-operator-expression
  [right]
  (if (= (:type right) object/INTEGER-OBJ)
    (object/integer-obj (- (:value right)))
    NULL))

(defn eval-prefix-expression
  [operator right]
  (cond
    (= operator "!") (eval-bang-operator-expression right)
    (= operator "-") (eval-minus-operator-expression right)
    :else NULL))

(defn eval-integer-infix-expression
  [operator left right]
  (let [left-value (:value left)
        right-value (:value right)]
    (cond
      (= operator "+") (object/integer-obj (+ left-value right-value))
      (= operator "-") (object/integer-obj (- left-value right-value))
      (= operator "*") (object/integer-obj (* left-value right-value))
      (= operator "/") (object/integer-obj (/ left-value right-value))
      (= operator "<") (native-bool-to-bool-obj (< left-value right-value))
      (= operator ">") (native-bool-to-bool-obj (> left-value right-value))
      (= operator "==") (native-bool-to-bool-obj (== left-value right-value))
      (= operator "!=") (native-bool-to-bool-obj (not= left-value right-value))
      :else NULL)))

(defn eval-infix-expression
  [operator left right]
  (cond
    (and (= (:type left) object/INTEGER-OBJ)
         (= (:type right) object/INTEGER-OBJ)) (eval-integer-infix-expression
                                                operator
                                                left
                                                right)
    :else NULL))

(defn eval-node [node]
  (let [node-type (get-in node [:token :type])]
    (cond
      ;; statements
      (contains? node :statements) (reduce (fn [_ stmt]
                                             (eval-node stmt))
                                           nil
                                           (:statements node))
      (ast/infix? node) (eval-infix-expression (:operator node)
                                               (eval-node (:left node))
                                               (eval-node (:right node)))
      (ast/prefix? node) (eval-prefix-expression (:operator node)
                                                 (eval-node (:right node)))
      (contains? node :expression) (eval-node (:expression node))
      ;; expressions
      (or (= node-type token/TRUE)
          (= node-type token/FALSE)) (native-bool-to-bool-obj (:value node))
      (= node-type token/INT) (object/integer-obj (:value node))
      :else NULL)))

