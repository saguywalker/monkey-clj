(ns saguywalker.parser
  (:require [saguywalker.ast :as ast]
            [saguywalker.lexer :as lexer]
            [saguywalker.token :as token]))

(defn next-token [parser-atom]
  (let [peek-tok (:peek-token @parser-atom)
        lexer-atom (:lexer @parser-atom)]
    (swap! parser-atom assoc :current-token peek-tok)
    (swap! parser-atom assoc :peek-token (lexer/next-token lexer-atom))))

(defn new-parser [lexer-atom]
  (let [parser-atom (atom {:lexer lexer-atom
                           :current-token 0
                           :peek-token 0})]
    (next-token parser-atom)
    (next-token parser-atom)
    parser-atom))

(defn parse-program [parser-atom]
  nil)

(comment
  (def my-test (lexer/new-lexer "let x = 37;"))
  @my-test-parser
  (def my-test-parser (new-parser my-test))
  (next-token my-test-parser))
