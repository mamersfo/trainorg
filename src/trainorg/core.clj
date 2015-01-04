(ns trainorg.core
  (:require [trainorg.db :as db]
            [cheshire.core :refer :all]))

(def base-pattern #"(^\*+) (\S+)")
(def ext-pattern #"(^\*+) (\S+): ([ \S]+$)")

(defn get-parts
  [line]
  (if line
    (seq (rest
          (or (re-find ext-pattern line)
              (re-find base-pattern line))))))

(defn make-node
  [parts]
  (if (seq parts)
    {:level (count (first parts))
     :type (keyword (clojure.string/lower-case (second parts)))
     :name (last parts)
     :text []}))

(defn make-nodes
  [lines]
  (loop [in lines out []]
    (if-let [line (first in)]
      (if-let [parts (get-parts line)]
        (recur (rest in) (conj out (make-node parts)))
        (recur (rest in) (update-in out [(dec (count out)) :text] conj line)))
      out)))

(defn make-tree
  [nodes]
  (cond
    (= 0 (count nodes)) nil
    (= 1 (count nodes)) (first nodes)
    :else
    (let [parent (first nodes)
          target-level (inc (:level parent))]
      (loop [in (rest nodes) part nil out []]
        (if-let [next (first in)]
          (if (= (:level next) target-level)
            (recur (rest in) (vector next) (if part (conj out part) out))
            (recur (rest in) (conj part next) out))
          (assoc parent :children (map make-tree (conj out part))))))))

(defn join-text
  [nodes]
  (map #(if (seq (:text %))
          (update-in % [:text] (partial clojure.string/join " "))
          (dissoc % :text)) nodes))

(defn parse
  [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (make-tree (join-text (make-nodes (line-seq rdr))))))

(defn string-for-type
  [t coll]
  (apply str (interpose ";" (map :name (filter #(= t (:type %)) coll)))))

(defn exercises
  [root]
  (apply concat
         (for [training (:children root)]
           (apply concat
                  (for [category (:children training)]
                    (for [exercise (:children category)]
                      (let [children (:children exercise)]
                        {:uuid (-> (filter #(= :uuid (:type %)) children)
                                   first :name)
                         :name (:name exercise)
                         :category (:name category)
                         :text (:text exercise)
                         :tags (string-for-type :tag children)
                         :variations (string-for-type :variation children)
                         :station (-> (filter #(= :station (:type %)) children)
                                      first :name)
                         })))))))

(defn random-uuid
  []
  (.toString (java.util.UUID/randomUUID)))


(defn print-uuids
  [count]
  (repeatedly count #(println (random-uuid))))

(defn inspect
  [root]
  (doseq [training (:children root)]
    (println "training:" (:name training))
    (doseq [category (:children training)]
      (println "category:" (:name category))
      (doseq [exercise (:children category)]
        (println "exercise:" (:name exercise))
        (let [variations (filter #(= :variation (:type %)) (:children exercise))]
          (println "variations:" (map :name variations)))))))

(defn load-db
  [root]
  (let [exercises (map #(assoc % :Z_PK (db/next-id)) (exercises root))]
    (db/clear-exercises-table)
    (loop [exercises exercises]
      (when-let [e (first exercises)]
        (db/insert-exercise e)
        (recur (rest exercises))))))

(def training-file (str (System/getProperty "user.home")
                        "/Dropbox/Org/training.org"))

(defn export-json
  ([]
   (export-json (parse training-file)))
  ([root]
   (with-open [out (clojure.java.io/writer "out.json")]
     (generate-stream (exercises root) out))))
