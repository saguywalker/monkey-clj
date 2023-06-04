(ns saguywalker.token)

(def ILLEGAL "ILLEGAL")
(def EOF "EOF")

;; Identifiders + Literals
(def IDENT "IDENT")
(def INT "INT")

;; Operators
(def ASSIGN "=")
(def PLUS "+")

;; Delimiters
(def COMMA ",")
(def SEMICOLON ";")
(def LPAREN "(")
(def RPAREN ")")
(def LBRACE "{")
(def RBRACE "}")

;; Keywords
(def FUNCTION "FUNCTION")
(def LET "LET")

(defn next-token [input-atom]
  {:type ILLEGAL
   :literal "dunno"})

