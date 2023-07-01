(ns saguywalker.object)

(def INTEGER-OBJ "INTEGER")
(def BOOLEAN-OBJ "BOOLEAN")
(def NULL-OBJ "NULL")

(def null-obj {:type NULL-OBJ})

(defn integer-obj [i]
  {:type INTEGER-OBJ
   :value i})

(defn boolean-obj [b]
  {:type BOOLEAN-OBJ
   :value b})

(defn inspect [obj]
  (let [obj-type (:type obj)]
    (cond
      (= obj-type NULL-OBJ) "null"
      :else (str (:value obj)))))

