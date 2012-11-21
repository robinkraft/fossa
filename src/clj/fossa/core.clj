(ns fossa.core
  (:use [cascalog.api])
  (:require [fossa.utils :as u]
            [cascalog.ops :as c]
            [clojure.java.io :as io]))

;; Slurps resources/occ.txt:
(def local-data (.getPath (io/resource "occ.txt")))

;; eBird data resource id:
(def ^:const EBIRD-ID "43")

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

(defn not-ebird
  "Return true if supplied id represents an eBird record, otherwise false."
  [id]
  (not= id EBIRD-ID))

(defn cleanup-data
  "Cleanup data by handling rounding, missing data, etc."
  [digits lat lon prec year month]
  (let [[lat lon clean-prec clean-year clean-month] (map u/str->num-or-empty-str [lat lon prec year month])]
    (concat (map (partial u/round-to digits) [lat lon clean-prec])
            (map str [clean-year clean-month]))))

(defn read-occurrences
  "Returns a Cascalog generator of occurence fields for supplied data path."
  ([]
     (read-occurrences local-data))
  ([path]
     (let [digits 7 ;; round all floats to max seven digits
           src (hfs-textline path)]
       (<- [?scientificname ?occurrenceid ?lat ?lon ?clean-prec ?clean-year ?clean-month]
           (src ?line)
           (u/split-line ?line :>> occ-fields)
           (not-ebird ?dataresourceid) ;; Filter out eBird (See http://goo.gl/4OMLl)
           (u/valid-name? ?scientificname)
           (u/valid-latlon? ?lat ?lon)
           (cleanup-data
            digits ?latitude ?longitude ?coordinateprecision ?year ?month :>
            ?lat ?lon ?clean-prec ?clean-year ?clean-month)
           (:distinct true)))))

(defbufferop collect-by-latlon
  "Returns WKT MULTIPOINT for each unique latlon, along with
   occurrence ids, precision, year and month of each
   observation. Observations need not be unique - they are aggregated
   into vectors and ordered the same as the multi-point."
  [tuples]
  (let [sci-name (first (u/extract 0 tuples))
        lats (u/extract 1 tuples)
        lons (u/extract 2 tuples)
        tuple-parser (partial u/data->update-stmt tuples lats lons sci-name)
        multi-pt (u/mk-update-stmt sci-name "the_geom_multipoint" (u/surround-str (u/parse-for-wkt lats lons) "'"))]
    (into [multi-pt] (map tuple-parser ["occids" "precision" "year" "month" "season"] (range 3 8)))))

(defn parse-occurrence-data
  "Shred some GBIF."
  [occ-src]
  (<- [?sci-name ?update-stmt]
      (occ-src ?sci-name ?occ-id ?lat ?lon ?prec ?year ?month)
      (u/get-season ?lat ?month :> ?season)
      (collect-by-latlon ?sci-name ?lat ?lon ?occ-id ?prec ?year ?month ?season
                         :> ?update-stmt)))


