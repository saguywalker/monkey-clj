(ns saguywalker.repl
  (:require [saguywalker.lexer :as lexer]
            [saguywalker.token :as token]))

(def PROMPT ">> ")

(defn- loop-read-token [lexer-atom]
  (let [tok (lexer/next-token lexer-atom)]
    (when (not= token/EOF (:type tok))
      (println tok)
      (recur lexer-atom))))

(defn start []
    (print PROMPT)
    (flush)
    (let [line (read-line)
          lexer-atom (lexer/new-lexer line)]
      (loop-read-token lexer-atom)
      (recur)))

(comment
  (def test-lexer (lexer/new-lexer "let hello = 1337;"))
  (loop-read-token test-lexer))
