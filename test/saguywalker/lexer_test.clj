(ns saguywalker.lexer-test
  (:require [clojure.test :refer :all]
            [saguywalker.token :as token]))

(deftest next-token-test
  (testing "next token test"
    (let [input (atom "`=+(){},;`")
          tests [{:type token/ASSIGN :literal "="}
                 {:type token/PLUS :literal "+"}
                 {:type token/LPAREN :literal "("}
                 {:type token/RPAREN :literal ")"}
                 {:type token/LBRACE :literal "{"}
                 {:type token/RBRACE :literal "}"}
                 {:type token/COMMA :literal ","}
                 {:type token/SEMICOLON :literal ";"}
                 {:type token/EOF :literal ""}]]
      (doseq [tt tests]
        (let [result (token/next-token input)]
          (is (= (:type result) (:type tt)))
          (is (= (:literal result) (:literal tt))))))))

