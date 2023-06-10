(ns saguywalker.ast
  (:require [saguywalker.token :as token]))


(defn token-literal-from-program [program]
  (let [statements (:statements program)]
    (if (> (count statements) 0)
      (token/token-literal (first statements))
      "")))
