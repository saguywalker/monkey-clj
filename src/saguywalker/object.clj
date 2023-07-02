(ns saguywalker.object)

(def ERROR-OBJ "ERROR")
(def INTEGER-OBJ "INTEGER")
(def BOOLEAN-OBJ "BOOLEAN")
(def NULL-OBJ "NULL")
(def RETURN-VALUE-OBJ "RETURN_VALUE")

(def null-obj {:type NULL-OBJ})

(defn integer-obj [i]
  {:type INTEGER-OBJ
   :value i})

(defn boolean-obj [b]
  {:type BOOLEAN-OBJ
   :value b})

(defn return-obj [v]
  {:type RETURN-VALUE-OBJ
   :value v})

(defn error-obj [e]
  {:type ERROR-OBJ
   :message e})

(defn inspect [obj]
  (let [obj-type (:type obj)]
    (cond
      (= obj-type NULL-OBJ) "null"
      (= obj-type ERROR-OBJ) (str "ERROR: " (:message obj))
      :else (str (:value obj)))))

