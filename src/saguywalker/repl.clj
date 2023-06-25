(ns saguywalker.repl
  (:require  [clojure.pprint :as pp]
             [saguywalker.ast :as ast]
             [saguywalker.evaluator :as evaluator]
             [saguywalker.lexer :as lexer]
             [saguywalker.object :as object]
             [saguywalker.token :as token]
             [saguywalker.parser :as parser]))

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
        lexer-atom (lexer/new-lexer line)
        parser-atom (parser/new-parser lexer-atom)
        program (parser/parse-program parser-atom)
        evaluated (evaluator/eval-node program)]
    (when (not= (count (:errors program)) 0)
      (pp/pprint (:errors program)))
    (pp/pprint (object/inspect evaluated))
    (recur)))

(comment
  (def test-lexer (lexer/new-lexer "let hello = 1337;"))
  (loop-read-token test-lexer))
