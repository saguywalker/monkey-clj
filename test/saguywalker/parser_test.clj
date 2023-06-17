(ns saguywalker.parser-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.pprint]
            [saguywalker.ast :as ast]
            [saguywalker.lexer :as lexer]
            [saguywalker.parser :as parser]
            [saguywalker.token :as token]))

(defn test-integer-literal
  [expression value]
  (cond
    (not= value (:value expression)) false
    (not= (str value) (token/token-literal expression)) false
    :else true))

(defn test-identifier
  [exp value]
  (cond
    (not= value (:value exp)) false
    (not= value (token/token-literal exp)) false
    :else true))

(defn test-boolean-literal
  [exp value]
  (cond
    (not= value (:value exp)) false
    (not= (str value) (token/token-literal exp)) false
    :else true))

(defn test-literal-expression
  [exp expected]
  (cond
    (int? expected) (test-integer-literal exp expected)
    (string? expected) (test-identifier exp expected)
    (boolean? expected) (test-boolean-literal exp expected)
    :else false))

(deftest test-let-statement
  (testing "test parsing let statement"
    (let [input (string/escape "let x = 5;
                               let y = 10;
                               let foobar = 838383;"
                               {})
          l (lexer/new-lexer input)
          p (parser/new-parser l)
          program (parser/parse-program p)]
      (is (= [] (:errors @p)))
      (is (not= nil program))
      (is (= 3 (count (:statements program))))
      (doseq [[stmt expected] (map vector
                                   (:statements program)
                                   ["x", "y", "foobar"])]
        (is (= (token/token-literal stmt) "let"))
        (is (= expected (get-in stmt [:name :value])))))))

(deftest test-return-statement
  (testing "test parsing return statement"
    (let [input (string/escape "return 5;
                               return 10;
                               return 993322;"
                               {})
          l (lexer/new-lexer input)
          p (parser/new-parser l)
          program (parser/parse-program p)]
      (is (= [] (:errors @p)))
      (is (not= nil program))
      (is (= 3 (count (:statements program))))
      (doseq [[stmt expected] (map vector
                                   (:statements program)
                                   ["5", "10", "993322"])]
        (is (= (token/token-literal stmt) "return"))
;;        (is (= expected (get-in stmt [:return-value])))
        ))))

(deftest test-identifier-expression
  (testing "test identifier expression"
    (let [input (string/escape "foobar;"
                               {})
          l (lexer/new-lexer input)
          p (parser/new-parser l)
          program (parser/parse-program p)]
      (is (= [] (:errors @p)))
      (is (not= nil program))
      (is (= 1 (count (:statements program))))
      (doseq [[stmt expected] (map vector
                                   (:statements program)
                                   ["foobar"])]
        (is (= (ast/expression-stmt->return-value stmt) expected))
        (is (= (token/token-literal stmt) expected))))))

(deftest test-boolean-expression
  (testing "test boolean expression"
    (let [input (string/escape "true;"
                               {})
          l (lexer/new-lexer input)
          p (parser/new-parser l)
          program (parser/parse-program p)]
      (is (= [] (:errors @p)))
      (is (not= nil program))
      (is (= 1 (count (:statements program))))
      (doseq [[stmt expected] (map vector
                                   (:statements program)
                                   [true])]
        (is (= (ast/expression-stmt->return-value stmt) expected))
        (is (= (token/token-literal stmt) (str expected)))))))

(deftest test-integer-literal-expression
  (testing "test integer literal expression"
    (let [input (string/escape "5;"
                               {})
          l (lexer/new-lexer input)
          p (parser/new-parser l)
          program (parser/parse-program p)]
      (is (= [] (:errors @p)))
      (is (not= nil program))
      (is (= 1 (count (:statements program))))
      (doseq [[stmt expected] (map vector
                                   (:statements program)
                                   [5])]
        (is (= (ast/integer-literal->value (:expression stmt)) expected))
        (is (= (token/token-literal (:expression stmt)) (str expected)))))))

(deftest test-prefix-expression
  (testing "test prefix expression"
    (let [test-cases [{:input "!5;"
                       :operator "!"
                       :integer-value 5}
                      {:input "-15;"
                       :operator "-"
                       :integer-value 15}]]
      (doseq [tt test-cases]
        (let [l (lexer/new-lexer (:input tt))
              p (parser/new-parser l)
              program (parser/parse-program p)
              expression (:expression (first (:statements program)))]
          (is (= [] (:errors @p)))
          (is (not= nil program))
          (is (= 1 (count (:statements program))))
          (is (= (:operator expression) (:operator tt)))
          (is (test-integer-literal (:right expression)
                                    (:integer-value tt))))))))

(deftest test-infix-expression
  (testing "test infix expression"
    (let [test-cases [{:input "5 + 6;"
                       :left 5
                       :operator "+"
                       :right 6}
                      {:input "5 - 6;"
                       :left 5
                       :operator "-"
                       :right 6}
                      {:input "5 * 6;"
                       :left 5
                       :operator "*"
                       :right 6}
                      {:input "5 / 6;"
                       :left 5
                       :operator "/"
                       :right 6}
                      {:input "5 > 6;"
                       :left 5
                       :operator ">"
                       :right 6}
                      {:input "5 < 6;"
                       :left 5
                       :operator "<"
                       :right 6}
                      {:input "5 == 6;"
                       :left 5
                       :operator "=="
                       :right 6}
                      {:input "5 != 6;"
                       :left 5
                       :operator "!="
                       :right 6}
                      {:input "true == true"
                       :left true
                       :operator "=="
                       :right true}
                      {:input "true != false"
                       :left true
                       :operator "!="
                       :right false}
                      {:input "false == false"
                       :left false
                       :operator "=="
                       :right false}]]

      (doseq [tt test-cases]
        (let [l (lexer/new-lexer (:input tt))
              p (parser/new-parser l)
              program (parser/parse-program p)
              expression (:expression (first (:statements program)))]
          (is (= [] (:errors @p)))
          (is (not= nil program))
          (is (= 1 (count (:statements program))))
          (is (= (:operator expression) (:operator tt)))
          (is (test-literal-expression (:left expression)
                                       (:left tt)))
          (is (test-literal-expression (:right expression)
                                       (:right tt))))))))

(deftest test-operator-precedence-parsing
  (testing "test operator precedence parsing"
    (let [tests [{:input "-a * b"
                  :expected "((-a) * b)"}
                 {:input "!-a"
                  :expected "(!(-a))"}
                 {:input "a + b + c"
                  :expected "((a + b) + c)"}
                 {:input "a + b - c"
                  :expected "((a + b) - c)"}
                 {:input "a * b * c"
                  :expected "((a * b) * c)"}
                 {:input "a * b / c"
                  :expected "((a * b) / c)"}
                 {:input "a + b * c + d / e - f"
                  :expected "(((a + (b * c)) + (d / e)) - f)"}
                 {:input "3 + 4; - 5 * 5"
                  :expected "(3 + 4)((-5) * 5)"}
                 {:input "5 > 4 == 3 < 4"
                  :expected "((5 > 4) == (3 < 4))"}
                 {:input "true"
                  :expected "true"}
                 {:input "false"
                  :expected "false"}
                 {:input "3 > 5 == false"
                  :expected "((3 > 5) == false)"}
                 {:input "3 < 5 == true"
                  :expected "((3 < 5) == true)"}
                 {:input "3 + 4 * 5 == 3 * 1 + 4 * 5"
                  :expected "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"}
                 {:input "1 + (2 + 3) + 4"
                  :expected "((1 + (2 + 3)) + 4)"}
                 {:input "(5 + 5) * 2"
                  :expected "((5 + 5) * 2)"}
                 {:input "2 / (5 + 5)"
                  :expected "(2 / (5 + 5))"}
                 {:input "-(5 + 5)"
                  :expected "(-(5 + 5))"}
                 {:input "!(true == true)"
                  :expected "(!(true == true))"}]]

      (doseq [tt tests]
        (let [l (lexer/new-lexer (:input tt))
              p (parser/new-parser l)
              program (parser/parse-program p)
              actual (ast/program->string program)]
          (is (= [] (:errors @p)))
          (is (=  actual (:expected tt))))))))

(deftest test-parsing-prefix-expression
  (testing "test parsing prefix expression"
    (doseq [tt [{:input "!true"
                 :operator "!"
                 :value true}
                {:input "!false"
                 :operator "!"
                 :value false}]]
      (let [l (lexer/new-lexer (:input tt))
            p (parser/new-parser l)
            program (parser/parse-program p)
            exp (first (:statements program))]
        (is (= [] (:errors @p)))
        (is (= (get-in exp [:expression :right :value]) (:value tt)))
        (is (= (get-in exp [:expression :operator]) (:operator tt)))))))
