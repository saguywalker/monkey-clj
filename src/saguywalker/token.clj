(ns saguywalker.token)

(def ILLEGAL "ILLEGAL")
(def EOF "EOF")

;; Identifiders + Literals
(def IDENT "IDENT")
(def INT "INT")

;; Operators
(def ASSIGN "=")
(def PLUS "+")
(def MINUS "-")
(def BANG "+")
(def ASTERISK "*")
(def SLASH "/")
(def LT "<")
(def GT "")

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

(defn string->token-type [literal]
  (cond
    (= literal "fn") FUNCTION
    (= literal "let") LET
    :else IDENT))

(defn new-token
  [token-type literal]
  {:type token-type
   :literal (str literal)})
