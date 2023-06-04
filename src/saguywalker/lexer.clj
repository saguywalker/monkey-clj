(ns saguywalker.lexer)

;; Lexer {:input :position :read-position :ch}

(defn new-lexer [input]
  {:input input
   :position 0
   :read-position 0
   :ch nil})

(defn- read-char [lexer]
  (let [input (:input lexer)
        read-position (:read-position lexer)]
    (-> lexer
        (assoc :ch (if (>= read-position (count input))
                     0
                     (nth input read-position)))
        (assoc :position read-position)
        (update :read-position inc))))

(comment
  (new-lexer "hello world")
  (read-char (new-lexer "hello")))

