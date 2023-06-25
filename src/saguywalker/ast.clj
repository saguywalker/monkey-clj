(ns saguywalker.ast
  (:require [clojure.string :as string]
            [saguywalker.token :as token]))

(defn expression-stmt->return-value [stmt]
  (get-in stmt [:expression :value]))

(defn- return-stmt->string [stmt]
  (str (token/token-literal stmt)
       " "
       (expression-stmt->return-value stmt) ";"))

(defn integer-literal->value [integer-literal]
  (:value integer-literal))

(defn- if-exp? [exp]
  (and (map? exp)
       (contains? exp :condition)
       (contains? exp :consequence)))

(defn- infix? [expression]
  (and (map? expression)
       (contains? expression :left)
       (contains? expression :right)))

(defn- prefix? [expression]
  (and (map? expression)
       (not (infix? expression))
       (contains? expression :operator)))

(defn- fn-exp? [exp]
  (and (map? exp)
       (contains? exp :parameters)
       (contains? exp :body)))

(defn- call-exp? [exp]
  (and (map? exp)
       (contains? exp :function)
       (contains? exp :arguments)))

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
    (if-exp? exp) (str "if"
                       (expression->string (:condition exp))
                       " "
                       (reduce (fn [acc e]
                                 (conj acc (expression->string e)))
                               []
                               (:consequence exp))
                       (when-let [alt (:alternative exp)]
                         (str "else "
                              (reduce (fn [acc e]
                                        (conj acc (expression->string e)))
                                      []
                                      alt))))
    (fn-exp? exp) (let [params (string/join ", "
                                            (map expression->string
                                                 (:parameters exp)))
                        tok-literal (token/token-literal exp)
                        body (reduce (fn [acc e]
                                       (conj acc (expression->string e)))
                                     []
                                     (:body exp))]
                    (str tok-literal
                         "("
                         params
                         ") "
                         body))
    (call-exp? exp) (let [args (string/join ", "
                                            (map expression->string
                                                 (:arguments exp)))]
                      (str (expression->string (:function exp))
                           "("
                           args
                           ")"))
    :else (str (:value exp))))

(defn- expression-stmt->string [stmt]
  (expression->string (:expression stmt)))

(defn let-stmt->name-string [stmt]
  (get-in stmt [:name :token :literal]))

(defn- let-stmt->string [stmt]
  (str (token/token-literal stmt)
       " "
       (let-stmt->name-string stmt)
       " = "
       (expression->string (:value stmt)) ";"))

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


