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

(defn split-line
  [line]
  "Returns vector of line values by splitting on tab."
  (vec (.split line "\t")))

(defn cdb-update
  [sql]
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
         (split-line ?line :> ?name ?sql)
         (cdb-update ?sql))))
