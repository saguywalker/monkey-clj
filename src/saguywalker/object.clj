(ns saguywalker.object)

(def INTEGER-OBJ "INTEGER")
(def BOOLEAN-OBJ "BOOLEAN")
(def NULL-OBJ "NULL")

(defn inspect [obj]
  (let [obj-type (:type obj)]
    (cond
      (= obj-type NULL-OBJ) "null"
      :else (str (:value obj)))))

