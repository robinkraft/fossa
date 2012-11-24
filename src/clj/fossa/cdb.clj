(ns fossa.cdb
  "This namespace provides CartoDB support."
  (:use [cascalog.api]
        [cartodb.core :as cdb]
        [clojure.data.json :only (read-json)])
  (:require [fossa.utils :as u]
            [cascalog.ops :as c]
            [clojure.java.io :as io]
            [clojure.string :as s]))

;; Slurps resources/creds.json for OAuth: {"key" "secret" "user" "password"}
(def creds (read-json (slurp (io/resource "cdb-creds.json"))))

;; Slurps resources/s3.json for Amazon S3: {"access-key" "secret-key"}
(def s3-creds (read-json (slurp (io/resource "s3-creds.json"))))

(defn s3-source
  [path]
  "Return authenticated S3 source for supplied path."
  (let [key (:access-id s3-creds)
        secret (:private-key s3-creds)
        source (str "s3n://" key  ":" secret "@" path)]
    source))

(defn cdb-execute
  [sql]
  "Execute supplied SQL statement on CartoDB."
  (cdb/query sql (:user creds) :oauth creds))

(defn file->cdb
  [path]
  "Bulkload file at supplied path to CartoDB. The file contains tab delineated
   textlines where the first column is a Scientific name and the second column
   is an SQL UPDATE statement."
  (let [src (hfs-textline path)]
    (?<- (stdout)
         [?name]
         (src ?line)
         (u/split-line ?line :> ?name ?sql)
         (cdb-execute ?sql))))
