(ns saguywalker.lexer
  (:require [saguywalker.token :as token]))

;; Lexer {:input :position :read-position :ch}

(defn- read-char [lexer-atom]
  (let [lexer @lexer-atom
        input (:input lexer)
        read-position (:read-position lexer)]
    (swap! lexer-atom assoc :ch (if (>= read-position (count input))
                                  0
                                  (nth input read-position)))
    (swap! lexer-atom assoc :position read-position)
    (swap! lexer-atom update :read-position inc)
    lexer-atom))

(defn new-lexer [input]
  (read-char (atom {:input input
                    :position 0
                    :read-position 0
                    :ch nil})))

(defn- letter? [ch]
  (or (Character/isLetter ch)
      (= ch \_)))

(defn- read-identifier [lexer-atom]
  (let [position (:position @lexer-atom)]
    (while (letter? (:ch @lexer-atom))
      (read-char lexer-atom))
    (let [lexer @lexer-atom]
      (token/new-token token/IDENT
                       (subs (:input lexer)
                             position
                             (:position lexer))))))

(defn next-token [lexer-atom]
  (let [ch (:ch @lexer-atom)
        tok (cond
              (= ch \=) (token/new-token token/ASSIGN ch)
              (= ch \;) (token/new-token token/SEMICOLON ch)
              (= ch \() (token/new-token token/LPAREN ch)
              (= ch \)) (token/new-token token/RPAREN ch)
              (= ch \,) (token/new-token token/COMMA ch)
              (= ch \+) (token/new-token token/PLUS ch)
              (= ch \{) (token/new-token token/LBRACE ch)
              (= ch \}) (token/new-token token/RBRACE ch)
              (= ch 0) (token/new-token token/EOF "")
              :else (if (letter? ch)
                      (read-identifier lexer-atom)
                      (token/new-token token/ILLEGAL ch)))]
    (when-not (letter? ch) (read-char lexer-atom))
    tok))

(comment
  (def lexer-test (new-lexer "()test{}"))
  (next-token lexer-test)
  @lexer-test
  (next-token (new-lexer "()"))
  (next-token (new-lexer "hello world"))
  (read-char (new-lexer ""))
  (read-char (new-lexer "0"))
  (read-char (new-lexer "hello"))
  (def my-test-1 (new-lexer "`=+(){},;`"))
  (next-token my-test-1))

