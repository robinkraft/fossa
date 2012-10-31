(ns fossa.core
  (:use [cascalog.api])
  (:require [cascalog.ops :as c]))

(def local-data
  "/mnt/hgfs/Dropbox/code/github/MapofLife/gbifer/resources/gbif/occ.txt")

;; Ordered column names from the occurrence_20120802.txt.gz dump.
(def occ-fields ["?occurrenceid" "?taxonid" "?dataresourceid" "?kingdom"
                 "?phylum" "?class" "?orderrank" "?family" "?genus"
                 "?scientificname" "?kingdomoriginal" "?phylumoriginal"
                 "?classoriginal" "?orderrankoriginal" "?familyoriginal"
                 "?genusoriginal" "?scientificnameoriginal" "?authororiginal"
                 "?datecollected" "?year" "?month" "?basisofrecord"
                 "?countryoriginal" "?countryisointerpreted" "?locality"
                 "?county" "?continentorocean" "?stateprovince" "?latitude"
                 "?latitudeinterpreted" "?longitude" "?longitudeinterpreted"
                 "?coordinateprecision" "?geospatialissue" "?lastindexed"])

(defn split-line
  "Returns vector of line values by splitting on tab."
  [line]
  (vec (.split line "\t")))

(defn read-occurrences
  ([]
     (read-occurrences local-data)) 
  ([path]
     (let [src (hfs-textline path)]
       (<- [?scientificname ?occurrenceid ?latitude ?longitude]
           (src ?line)
           (split-line ?line :>> occ-fields)))))

(defn latlons->wkt-multi-point
  [lats lons]
  (let [out-str "MULTIPOINT (%s)"
        sep ", "]
    (->> (interleave lats lons)
         (partition 2 2)
         (map seq)
         (interpose sep)
         (apply str)
         (format out-str))))

(defbufferop collect-ids-latlons
  [tuples]
  (let [occ-ids (vec (map first tuples))
        lats (map #(nth % 1) tuples)
        lons (map #(nth % 2) tuples)]
    [[occ-ids (latlons->wkt-multi-point lats lons)]]))

(defn parse-occurrence-data
  [path]
  (let [occ-src (read-occurrences path)]
  (<- [?sci-name ?occ-ids ?multi-point]
      (occ-src ?sci-name ?occ-id ?lat-str ?lon-str)
      ((c/each #'read-string) ?lat-str ?lon-str :> ?lat ?lon)
      (collect-ids-latlons ?occ-id ?lat ?lon :> ?occ-ids ?multi-point))))

