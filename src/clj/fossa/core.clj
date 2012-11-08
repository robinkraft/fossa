(ns fossa.core
  (:use [cascalog.api])
  (:require [fossa.utils :as u]
            [cascalog.ops :as c]
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

(defn read-occurrences
  "Returns a Cascalog generator of occurence fields for supplied data path."
  ([]
     (read-occurrences local-data)) 
  ([path]
     (let [src (hfs-textline path)]
       (<- [?scientificname ?occurrenceid ?latitude ?longitude ?prec ?year ?month]
           (src ?line)
           (u/cleanup-slash-N ?coordinateprecision :> ?prec)
           (u/split-line ?line :>> occ-fields)))))

(defbufferop collect-by-latlon
  "Returns WKT MULTIPOINT for each unique latlon, along with
   occurrence ids, precision, year and month of each
   observation. Observations need not be unique - they are aggregated
   into vectors and ordered the same as the multi-point."
  [tuples]
  (let [lats (u/extract 0 tuples)
        lons (u/extract 1 tuples)
        occ (u/extract-field tuples lats lons 2)
        prec (u/extract-field tuples lats lons 3)
        yr (u/extract-field tuples lats lons 4)
        mo (u/extract-field tuples lats lons 5)
        multi-pt (u/parse-for-wkt lats lons)]
    [[multi-pt (vec occ) (vec prec) (vec yr) (vec mo)]]))

(defn parse-occurrence-data
  "Shred some GBIF."
  [& {:keys [path] :or {path local-data}}]
  (let [occ-src (read-occurrences path)]
  (<- [?scientificname ?multipoint ?occ-ids ?precision ?yr ?mo]
      (occ-src ?scientificname ?occurrenceid ?latitude ?longitude ?coordinateprecision ?year ?month)
      (u/valid-name? ?scientificname)
      ((c/each #'read-string) ?latitude ?longitude :> ?lat ?lon)
      (u/latlon-valid? ?lat ?lon)
      (collect-by-latlon ?lat ?lon ?occurrenceid ?coordinateprecision ?year ?month :> ?multipoint ?occ-ids ?precision ?yr ?mo))))
