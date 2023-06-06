(ns saguywalker.lexer-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [saguywalker.token :as token]
            [saguywalker.lexer :as lexer]))

(deftest next-token-test
  (testing "next token test"
    (let [input (lexer/new-lexer "=+(){},;")
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
        (let [result (lexer/next-token input)]
          (is (= (:type result) (:type tt)))
          (is (= (:literal result) (:literal tt))))))))

(deftest next-token-with-actual-code-test
  (testing "next token test with actual code"
    (let [input (lexer/new-lexer
                 (string/escape "let five = 5;
                                 let ten = 10;
                                 let add = fn(x, y) {
                                   x + y;
                                 };
                                 let result = add(five, ten);
                                 !-/*5;
                                 5 < 10 > 5;"
                                {}))
          tests [{:type token/LET :literal "let"}
                 {:type token/IDENT :literal "five"}
                 {:type token/ASSIGN :literal "="}
                 {:type token/INT :literal "5"}
                 {:type token/SEMICOLON :literal ";"}
                 {:type token/LET :literal "let"}
                 {:type token/IDENT :literal "ten"}
                 {:type token/ASSIGN :literal "="}
                 {:type token/INT :literal "10"}
                 {:type token/SEMICOLON :literal ";"}
                 {:type token/LET :literal "let"}
                 {:type token/IDENT :literal "add"}
                 {:type token/ASSIGN :literal "="}
                 {:type token/FUNCTION :literal "fn"}
                 {:type token/LPAREN :literal "("}
                 {:type token/IDENT :literal "x"}
                 {:type token/COMMA :literal ","}
                 {:type token/IDENT :literal "y"}
                 {:type token/RPAREN :literal ")"}
                 {:type token/LBRACE :literal "{"}
                 {:type token/IDENT :literal "x"}
                 {:type token/PLUS :literal "+"}
                 {:type token/IDENT :literal "y"}
                 {:type token/SEMICOLON :literal ";"}
                 {:type token/RBRACE :literal "}"}
                 {:type token/SEMICOLON :literal ";"}
                 {:type token/LET :literal "let"}
                 {:type token/IDENT :literal "result"}
                 {:type token/ASSIGN :literal "="}
                 {:type token/IDENT :literal "add"}
                 {:type token/LPAREN :literal "("}
                 {:type token/IDENT :literal "five"}
                 {:type token/COMMA :literal ","}
                 {:type token/IDENT :literal "ten"}
                 {:type token/RPAREN :literal ")"}
                 {:type token/SEMICOLON :literal ";"}
                 {:type token/BANG :literal "!"}
                 {:type token/MINUS :literal "-"}
                 {:type token/SLASH :literal "/"}
                 {:type token/ASTERISK :literal "*"}
                 {:type token/INT :literal "5"}
                 {:type token/SEMICOLON :literal ";"}
                 {:type token/INT :literal "5"}
                 {:type token/LT :literal "<"}
                 {:type token/INT :literal "10"}
                 {:type token/GT :literal ">"}
                 {:type token/INT :literal "5"}
                 {:type token/SEMICOLON :literal ";"}
                 {:type token/EOF :literal ""}]]
      (doseq [tt tests]
        (let [result (lexer/next-token input)]
          (is (= (:type result) (:type tt)))
          (is (= (:literal result) (:literal tt))))))))

