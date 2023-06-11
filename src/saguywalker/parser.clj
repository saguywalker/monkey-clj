(ns saguywalker.parser
  (:require   [saguywalker.lexer :as lexer]
              [saguywalker.token :as token]))

(def LOWEST 1)
(def EQUALS 2)
(def LESSGREATER 3)
(def SUM 4)
(def PRODUCT 5)
(def PREFIX 6)
(def CALL 7)

(def precedences
  {token/EQ EQUALS
   token/NOT_EQ EQUALS
   token/LT LESSGREATER
   token/GT LESSGREATER
   token/PLUS SUM
   token/MINUS SUM
   token/SLASH PRODUCT
   token/ASTERISK PRODUCT})

(defn peek-precedence [parser-atom]
  (get precedences
       (get-in @parser-atom [:peek-token :type])
       LOWEST))

(defn current-precendence [parser-atom]
  (get precedences
       (get-in @parser-atom [:current-token :type])
       LOWEST))

(defn no-prefix-parse-fn-error [parser-atom token-type]
  (swap! parser-atom
         update
         :errors
         conj
         (str "no prefix parse function for " token-type " found")))

(defn next-token [parser-atom]
  (let [peek-tok (:peek-token @parser-atom)
        lexer-atom (:lexer @parser-atom)]
    (swap! parser-atom assoc :current-token peek-tok)
    (swap! parser-atom assoc :peek-token (lexer/next-token lexer-atom))))

(defn parse-expression [parser-atom precedence]
  (let [current-token-type (get-in @parser-atom [:current-token :type])
        prefix (get (:prefix-parse-fns @parser-atom) current-token-type)]
    (if (nil? prefix)
      (do
        (no-prefix-parse-fn-error parser-atom current-token-type)
        nil)
      (loop [left-exp (prefix parser-atom)]
        (if (and (not= token/SEMICOLON
                       (get-in @parser-atom [:peek-token :type]))
                 (< precedence (peek-precedence parser-atom)))
          (let [infix (get (:infix-parse-fns @parser-atom)
                           (get-in @parser-atom [:peek-token :type]))]
            (if (nil? infix)
              left-exp
              (do
                (next-token parser-atom)
                (recur (infix parser-atom left-exp)))))
          left-exp)))))

(defn register-prefix
  [parser-atom token-type prefix-parse-fn]
  (swap! parser-atom
         update
         :prefix-parse-fns
         assoc
         token-type
         prefix-parse-fn))

(defn register-infix
  [parser-atom token-type infix-parse-fn]
  (swap! parser-atom
         update
         :infix-parse-fns
         assoc
         token-type
         infix-parse-fn))

(defn parse-identifier [parser-atom]
  {:token (:current-token @parser-atom)
   :value (get-in @parser-atom [:current-token :literal])})

(defn parse-integer-literal [parser-atom]
  (let [current-token (:current-token @parser-atom)
        literal (:literal current-token)
        value (Integer/parseInt literal)]
    (when (nil? value)
      (swap! parser-atom
             update
             :errors
             conj
             (str "could not parse " literal " as integer")))
    {:token current-token
     :value value}))

(defn parse-prefix-expression [parser-atom]
  (let [current-token (:current-token @parser-atom)
        literal (:literal current-token)]
    (next-token parser-atom)
    {:token current-token
     :operator literal
     :right (parse-expression parser-atom PREFIX)}))

(defn parse-infix-expression [parser-atom left]
  (let [current-token (:current-token @parser-atom)
        literal (:literal current-token)
        precedence (current-precendence parser-atom)]
    (next-token parser-atom)
    {:token current-token
     :operator literal
     :left left
     :right (parse-expression parser-atom precedence)}))

(defn new-parser [lexer-atom]
  (let [parser-atom (atom {:lexer lexer-atom
                           :current-token 0
                           :peek-token 0
                           :errors []
                           :prefix-parse-fns {}
                           :infix-parse-fns {}})]
    (register-prefix parser-atom token/IDENT parse-identifier)
    (register-prefix parser-atom token/INT parse-integer-literal)
    (register-prefix parser-atom token/BANG parse-prefix-expression)
    (register-prefix parser-atom token/MINUS parse-prefix-expression)
    (register-infix parser-atom token/PLUS parse-infix-expression)
    (register-infix parser-atom token/MINUS parse-infix-expression)
    (register-infix parser-atom token/SLASH parse-infix-expression)
    (register-infix parser-atom token/ASTERISK parse-infix-expression)
    (register-infix parser-atom token/EQ parse-infix-expression)
    (register-infix parser-atom token/NOT_EQ parse-infix-expression)
    (register-infix parser-atom token/LT parse-infix-expression)
    (register-infix parser-atom token/GT parse-infix-expression)
    (next-token parser-atom)
    (next-token parser-atom)
    parser-atom))

(defn- peek-error [parser-atom token-type]
  (let [peek-tok (get-in @parser-atom [:peek-token :type])]
    (swap! parser-atom
           update
           :errors
           conj
           (str "expected next token: " token-type ", got: " peek-tok " instead"))))

(defn- expect-peek [parser-atom token-type]
  (if (= token-type
         (get-in @parser-atom [:peek-token :type]))
    (do
      (next-token parser-atom)
      true)
    (do
      (peek-error parser-atom token-type)
      false)))

(defn- parse-let-statement [parser-atom]
  (let [current-token (:current-token @parser-atom)]
    (when (expect-peek parser-atom token/IDENT)
      (let [next-current-token (:current-token @parser-atom)]
        (when  (expect-peek parser-atom token/ASSIGN)
          (while (not= (get-in @parser-atom [:current-token :type])
                       token/SEMICOLON)
            (next-token parser-atom))
          {:token current-token
           :name {:token next-current-token
                  :value (:literal next-current-token)}})))))

(defn- parse-return-statement [parser-atom]
  (let [current-token (:current-token @parser-atom)]
    (next-token parser-atom)
    (while (not= (get-in @parser-atom [:current-token :type])
                 token/SEMICOLON)
      (next-token parser-atom))
    {:token current-token}))

(defn parse-expression-statement [parser-atom]
  (let [current-token (:current-token @parser-atom)
        expression (parse-expression parser-atom LOWEST)]
    (when (= token/SEMICOLON (get-in @parser-atom [:peek-token :type]))
      (next-token parser-atom))
    {:token current-token
     :expression expression}))

(defn- parser-statement [parser-atom]
  (let [token-type (get-in @parser-atom [:current-token :type])]
    (cond
      (= token-type token/LET) (parse-let-statement parser-atom)
      (= token-type token/RETURN) (parse-return-statement parser-atom)
      :else (parse-expression-statement parser-atom))))

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
  (next-token my-test-parser)
  (register-infix my-test-parser token/EQ "hello world"))

