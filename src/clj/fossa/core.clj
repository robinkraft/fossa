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
       (<- [?scientificname ?lat-f ?lon-f ?occurrenceid ?prec ?year ?month]
           (src ?line)
           (u/split-line ?line :>> occ-fields)
           (u/cleanup-slash-N ?coordinateprecision :> ?prec)
           (u/valid-name? ?scientificname)
           ((c/each #'read-string) ?latitude ?longitude :> ?lat ?lon)
           (u/latlon-valid? ?lat ?lon)
           ((c/each #'float) ?lat ?lon :> ?lat-f ?lon-f)
           (:distinct true)))))

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
        season (u/extract-field tuples lats lons 6)
        multi-pt (u/parse-for-wkt lats lons)]
    [[multi-pt (vec occ) (vec prec) (vec yr) (vec mo) (vec season)]]))

(defn parse-occurrence-data
  "Shred some GBIF."
  [& {:keys [path] :or {path local-data}}]
  (let [occ-src (read-occurrences path)]
  (<- [?sci-name ?multipoint ?occ-ids ?precs ?yrs ?mos ?seasons]
      (occ-src ?sci-name ?lat ?lon ?occ-id ?prec ?year ?month)
      (u/get-season ?lat ?month :> ?season)
      (collect-by-latlon ?lat ?lon ?occ-id ?prec ?year ?month ?season
                         :> ?multipoint ?occ-ids ?precs ?yrs ?mos ?seasons))))
