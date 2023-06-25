(ns saguywalker.ast-test
  (:require [clojure.test :refer :all]
            [saguywalker.ast :as ast]
            [saguywalker.token :as token]))

;;(deftest test-string
;;  (let [program {:statements [{:token {:type token/LET
;;                                       :literal "let"}
;;                               :name {:token {:type token/IDENT
;;                                              :literal "myVar"}
;;                                      :value {:token {:type token/IDENT
;;                                                      :literal "anotherVar"}
;;                                              :value "anotherVar"}}}]}]
;;    (is (= (ast/program->string program) "let myVar = anotherVar;"))))
