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

(defn- expect-peek [parser-atom token-type]
  (if (= token-type
         (get-in @parser-atom [:peek-token :type]))
    (do
      (next-token parser-atom)
      true)
    false))

(defn- parse-let-statement [parser-atom]
  (let [current-token (:current-token @parser-atom)]
    (if (not (expect-peek parser-atom token/IDENT))
      nil
      (let [next-current-token (:current-token @parser-atom)]
        (if (not  (expect-peek parser-atom token/ASSIGN))
          nil
          (do
            (while (not= (get-in @parser-atom [:current-token :type])
                         token/SEMICOLON)
              (next-token parser-atom))
            {:token current-token
             :name {:token next-current-token
                    :value (:literal next-current-token)}}))))))

(defn- parser-statement [parser-atom]
  (let [token-type (get-in @parser-atom [:current-token :type])]
    (cond
      (= token-type token/LET) (let [result (parse-let-statement parser-atom)]
                                 result)
      :else nil)))

(defn- parse-statements  [parser-atom]
  (filter some?
          (loop [stmts []]
            (if (= token/EOF
                   (get-in @parser-atom [:current-token :type]))
              stmts
              (let [stmt (parser-statement parser-atom)]
                (next-token parser-atom)
                (recur (conj stmts stmt)))))))

(defn parse-program [parser-atom]
  {:statements (parse-statements parser-atom)})

(comment
  (def my-test (lexer/new-lexer "let x = 37;"))
  @my-test-parser
  (def my-test-parser (new-parser my-test))
  (next-token my-test-parser))