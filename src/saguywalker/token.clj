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


(defn new-token 
  [token-type literal]
  {:type token-type
   :literal (str literal)})
