(ns saguywalker.monkey-clj
  (:gen-class)
  (:require [saguywalker.repl :as repl]))

(defn -main
  [& _]
  (println "Hello"
           (get (System/getenv) "USER")
           "This is the Monkey programming language!")
  (println "Feel free to type in commands")
  (repl/start))
