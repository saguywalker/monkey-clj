(ns saguywalker.evaluator-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pp]
            [saguywalker.evaluator :as evaluator]
            [saguywalker.lexer :as lexer]
            [saguywalker.object :as object]
            [saguywalker.parser :as parser]))

(defn test-eval [input]
  (-> input
      (lexer/new-lexer)
      (parser/new-parser)
      (parser/parse-program)
      (evaluator/eval-node)))

(defn test-integer-object [obj expected]
  (= (:value obj) expected))

(deftest test-eval-integer-expression
  (testing "test eval integer expression"
    (doseq [tt [{:input "5"
                 :expected 5}
                {:input "10"
                 :expected 10}]]
      (let [actual (test-eval (:input tt))]
        (is (test-integer-object actual (:expected tt)))))))

(deftest test-eval-boolean-expression
  (testing "test eval boolean expression"
    (doseq [tt [{:input "true"
                 :expected true}
                {:input "false"
                 :expected false}]]
      (let [actual (test-eval (:input tt))]
        (is (test-integer-object actual (:expected tt)))))))


