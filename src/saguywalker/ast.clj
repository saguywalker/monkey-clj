(ns saguywalker.ast
  (:require [saguywalker.token :as token]))

(defn let-stmt->name-string [stmt]
  (get-in stmt [:name :token :literal]))

(defn let-stmt->value-string [stmt]
  (get-in stmt [:name :value :token :literal]))

(defn- let-stmt->string [stmt]
  (str (token/token-literal stmt)
       " "
       (let-stmt->name-string stmt)
       " = "
       (let-stmt->value-string stmt) ";"))

(defn expression-stmt->return-value [stmt]
  (get-in stmt [:expression :value]))

(defn- return-stmt->string [stmt]
  (str (token/token-literal stmt)
       " "
       (expression-stmt->return-value stmt) ";"))

(defn integer-literal->value [integer-literal]
  (:value integer-literal))

(defn- infix? [expression]
  (and (map? expression)
       (contains? expression :left)
       (contains? expression :right)))

(defn- prefix? [expression]
  (and (map? expression)
       (not (infix? expression))
       (contains? expression :operator)))

(defn- expression->string [exp]
  (cond
    (infix? exp) (str "("
                      (expression->string (:left exp))
                      " "
                      (:operator exp)
                      " "
                      (expression->string (:right exp))
                      ")")
    (prefix? exp) (str "("
                       (:operator exp)
                       (expression->string (:right exp))
                       ")")
    :else (str (:value exp))))

(defn- expression-stmt->string [stmt]
  (expression->string (:expression stmt)))

(defn stmt->string [stmt]
  (let [token-type (get-in stmt [:token :type])]
    (cond
      (= token-type token/LET) (let-stmt->string stmt)
      (= token-type token/RETURN) (return-stmt->string stmt)
      :else (expression-stmt->string stmt))))

(defn program->string [program]
  (reduce (fn [acc stmt]
            (str acc (stmt->string stmt)))
          ""
          (:statements program)))

(defn token-literal-from-program [program]
  (let [statements (:statements program)]
    (if (pos? (count statements))
      (token/token-literal (first statements))
      "")))
