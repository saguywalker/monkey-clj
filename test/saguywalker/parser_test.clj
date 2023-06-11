(ns saguywalker.parser-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [saguywalker.ast :as ast]
            [saguywalker.lexer :as lexer]
            [saguywalker.parser :as parser]
            [saguywalker.token :as token]))

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
