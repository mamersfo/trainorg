(ns trainorg.db
  (:require [clojure.java.jdbc :as jdbc]))

(def db-spec {:classname   "org.sqlite.JDBC"
              :subprotocol "sqlite"
              :subname     "resources/Training.sqlite"})

(defn create-exercises-table
  []
  (jdbc/db-do-commands
   db-spec
   (jdbc/create-table-ddl
    :ZEXERCISES
    [:ZID :varchar "(36) PRIMARY KEY"]
    [:ZNAME :varchar "(20)"]
    [:ZTEXT :varchar "(201)"]
    [:ZCATEGORY :varchar "(20)"]
    [:ZTRAINING :varchar "(20)"])))

(defn clear-exercises-table
  []
  (jdbc/execute! db-spec ["DELETE FROM ZEXERCISES"]))

(defn drop-exercises-table
  []
  (jdbc/execute! db-spec ["DROP TABLE IF EXISTS ZEXERCISES"]))

(def idx (atom 0))

(defn next-id
  []
  (swap! idx inc))

(defn insert-exercise
  [m]
  (jdbc/insert! db-spec :ZEXERCISES m)) 

(defn read-exercises
  []
  (jdbc/query db-spec ["SELECT * FROM ZEXERCISES"]))

(defn perform-query
  ([sql]
   (jdbc/query db-spec [sql]))
  ([sql & params]
   (let [args (vec (cons sql params))]
     (jdbc/query db-spec args))))
