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
(def TRUE "TRUE")
(def FALSE "FALSE")
(def IF "IF")
(def ELSE "ELSE")
(def RETURN "RETURN")

(defn string->token-type [literal]
  (cond
    (= literal "fn") FUNCTION
    (= literal "let") LET
    (= literal "true") TRUE
    (= literal "false") FALSE
    (= literal "if") IF
    (= literal "else") ELSE
    (= literal "return") RETURN
    :else IDENT))

(defn new-token
  [token-type literal]
  {:type token-type
   :literal (str literal)})
