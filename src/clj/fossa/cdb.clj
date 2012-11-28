(ns fossa.cdb
  "This namespace provides support for uploading to CartoDB over the SQL API.
   It requires that you have configuration files for CartoDB and S3 in the
   resources directory. Both are downloadable from our fossaconfig bucket on S3."
  (:use [cascalog.api]
        [cartodb.core :as cdb]
        [clojure.data.json :only (read-json)])
  (:require [fossa.utils :as u]
            [cascalog.ops :as c]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def ^:const PSIZE 10000)
(def ^:const TABLE "gbif_points")
(def ^:const COUNTS-S3 "gbifsource/species-counts")
(def ^:const COUNTS-DEV "name-counts-part-00000")

;; Slurps resources/creds.json for OAuth: {"key" "secret" "user" "password"}
(def creds (read-json (slurp (io/resource "cdb-creds.json"))))

;; Slurps resources/s3.json for Amazon S3: {"access-key" "secret-key"}
(def s3-creds (read-json (slurp (io/resource "s3-creds.json"))))

(defn cdb-execute
  [sql]
  "Execute supplied SQL statement on CartoDB."
  (cdb/query sql (:user creds) :oauth creds))

(defn s3-source
  [path]
  "Return authenticated S3 source for supplied path."
  (let [key (:access-id s3-creds)
        secret (:private-key s3-creds)
        source (str "s3n://" key  ":" secret "@" path)]
    source))

(defn get-insert-sql
  [name count psize & {:keys [table] :or {table TABLE}}]
  "Return SQL string for all INSERT statements for the supplied name, count, and
   partition size."
  (let [count (read-string count)
        n (if (<= count psize) 1 (/ count psize))
        sql "INSERT INTO %s (name, partition) VALUES ('%s', %d);"
        all (for [x (range n)] (format sql table name x))
        statement (reduce str all)]
    statement))

(defn init-table
  [& {:keys [table psize src delete?]
      :or {table TABLE
           psize PSIZE
           src (hfs-textline (.getPath (io/resource COUNTS-DEV)))
           delete? true}}]
  "Initialize CartoDB table by inserting rows for all names and partitions."
  (let [delete-sql (str "delete from " table)]
    (if delete? (cdb-execute delete-sql))
    (?<- (hfs-textline "/tmp/sink" :sinkmode :replace)
         [?name ?count ?sql ?response]
         (src ?line)
         (u/split-line ?line :> ?name ?count)
         (get-insert-sql ?name ?count psize :> ?sql)
         (cdb-execute ?sql :> ?response))))

(defn update-table
  [path]
  "Bulkload file at supplied path to CartoDB. The file contains tab delineated
   textlines where the first column is a Scientific name and the second column
   is an SQL UPDATE statement."
  (let [src (hfs-textline path)]
    (?<- (hfs-textline "/tmp/sink" :sinkmode :replace)
         [?name ?partition ?response]
         (src ?line)
         (u/split-line ?line :> ?name ?partition ?sql)
         (cdb-execute ?sql :> ?response))))
