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
  (and (= (:value obj) expected)
       (= (:type obj) object/INTEGER-OBJ)))

(defn test-boolean-object [obj expected]
  (and (= (:value obj) expected)
       (= (:type obj) object/BOOLEAN-OBJ)))

(deftest test-eval-integer-expression
  (testing "test eval integer expression"
    (doseq [tt [{:input "5"
                 :expected 5}
                {:input "10"
                 :expected 10}
                {:input "-5"
                 :expected -5}
                {:input "-10"
                 :expected -10}
                {:input "5 + 5 + 5 + 5 - 10"
                 :expected 10}
                {:input "2 * 2 * 2 * 2 * 2"
                 :expected 32}
                {:input "-50 + 100 + -50"
                 :expected 0}
                {:input "5 * 2 + 10"
                 :expected 20}
                {:input "5 + 2 * 10"
                 :expected 25}
                {:input "20 + 2 * -10"
                 :expected 0}
                {:input "50 / 2 * 2 + 10"
                 :expected 60}
                {:input "2 * (5 + 10)"
                 :expected 30}
                {:input "3 * 3 * 3 + 10"
                 :expected 37}
                {:input "3 * (3 * 3) + 10"
                 :expected 37}
                {:input "(5 + 10 * 2 + 15 / 3) * 2 + -10"
                 :expected 50}]]

      (let [actual (test-eval (:input tt))]
        (is (test-integer-object actual (:expected tt)))))))

(deftest test-eval-boolean-expression
  (testing "test eval boolean expression"
    (doseq [tt [{:input "true"
                 :expected true}
                {:input "false"
                 :expected false}]]
      (let [actual (test-eval (:input tt))]
        (is (test-boolean-object actual (:expected tt)))))))

(deftest test-bang-operator
  (testing "test bang operator"
    (doseq [tt [{:input "!true"
                 :expected false}
                {:input "!false"
                 :expected true}
                {:input "!5"
                 :expected false}
                {:input "!!true"
                 :expected true}
                {:input "!!false"
                 :expected false}
                {:input "!!5"
                 :expected true}]]
      (let [actual (test-eval (:input tt))]
        (is (test-boolean-object actual (:expected tt)))))))

