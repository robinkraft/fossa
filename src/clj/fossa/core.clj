(ns fossa.core
  (:use [cascalog.api])
  (:require [cascalog.ops :as c]
            [clojure.java.io :as io]))

;; Slurps resources/occ.txt:
(def local-data (.getPath (io/resource "occ.txt")))

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
  "Returns a Cascalog generator of occurence fields for supplied data path."
  ([]
     (read-occurrences local-data)) 
  ([path]
     (let [src (hfs-textline path)]
       (<- [?scientificname ?occurrenceid ?latitude ?longitude]
           (src ?line)
           (split-line ?line :>> occ-fields)))))

(defn latlons->wkt-multi-point
  "Returns WKT MULTIPOINT string from supplied lats and lons."
  [lats lons]
  (let [out-str "MULTIPOINT (%s)"
        sep ", "]
    (->> (interleave lons lats)
         (partition 2 2)
         (map seq)
         (interpose sep)
         (apply str)
         (format out-str))))

(defbufferop collect-ids-latlons
  "Returns sequence of WKT MULTIPOINT for each unique occ id."
  [tuples]
  (let [occ-ids (vec (map first tuples))
        lats (map #(nth % 1) tuples)
        lons (map #(nth % 2) tuples)]
    [[occ-ids (latlons->wkt-multi-point lats lons)]]))

(deffilterop valid-name?
  "Return true if name is valid, otherwise return false."
  [name]
  (and (not= name nil) (not= name "")))

;; Valid ranges for latitude and longitude.
(def latlon-range {:lat-min -90 :lat-max 90 :lon-min -180 :lon-max 180})

(defn latlon-valid?
  "Return true if lat and lon are valid, otherwise return false."
  [lat lon]
  (try
    (let [{:keys [lat-min lat-max lon-min lon-max]} latlon-range]
      (and (<= lat lat-max)
           (>= lat lat-min)
           (<= lon lon-max)
           (>= lon lon-min)))
    (catch Exception e false)))

(defn parse-occurrence-data
  "Shred some GBIF."
  [& {:keys [path] :or {path local-data}}]
  (let [occ-src (read-occurrences path)]
  (<- [?sci-name ?occ-ids ?multi-point]
      (occ-src ?sci-name ?occ-id ?lat-str ?lon-str)
      (valid-name? ?sci-name)
      ((c/each #'read-string) ?lat-str ?lon-str :> ?lat ?lon)
      (latlon-valid? ?lat ?lon)
      (collect-ids-latlons ?occ-id ?lat ?lon :> ?occ-ids ?multi-point))))

