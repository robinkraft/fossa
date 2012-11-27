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

(def cleaned-fields
  "List of fields in `cleaned-src`. Adding a new field to the vector
   will result in the creation of an UPDATE statement for that field
   in the `parse-occurrence-data` query."
  ["?name" "?occids" "?lats" "?lons" "?precisions" "?years" "?months"])

(def collect-fields
  "Fields that will be aggregated by latlon. New fields derived from
  `cleaned-fields` and generated within `parse-occurrence-data` should
  be added to the `conj` form for processing into UPDATE statements."
  (sort (conj cleaned-fields "?season")))

(def field-map
  "Sorted map of field names as keys and assigned field indices as
   values. New fields will be assigned appropriate indices as they
   are added."
  (u/mk-sorted-map (zipmap collect-fields (range (count collect-fields)))))

(defbufferop collect-by-latlon
  "Returns SQL UPDATE statements as strings for each output field
   (e.g. latlon, occurrence ids, precision, year, etc.). Observation
   locations need not be unique - observations at the same location
   are aggregated and ordered according to the order of the incoming
   latitude and longitude. For more, see tests and documentation for
   `u/data->update-stmt`."
   [tuples]
  (let [sci-name (first (u/extract (field-map "?name") tuples))
        [lats lons] (u/extract-latlons tuples (map field-map ["?lats" "?lons"]))
        tuple-parser (partial u/data->update-stmt tuples lats lons sci-name)
        field-idxs (u/get-field-idxs field-map)
        clean-field-names (u/drop-q-mark field-map)]
    (into [(u/mk-multipoint-update sci-name lats lons)]
          (map tuple-parser clean-field-names field-idxs))))

(defn parse-occurrence-data
  "Shred some GBIF."
  [cleaned-src]
  (<- [?name ?update-stmt]
      (cleaned-src :>> cleaned-fields)
      (u/get-season ?lats ?months :> ?season)
      (collect-by-latlon :<< collect-fields :> ?update-stmt)))

(comment
  "Example RPL command for generating UPDATE statements."
  (let [src (hfs-seqfile (.getPath (io/resource "passer-part-00000")))
        query (parse-occurrence-data src)
        sink (hfs-textline "/tmp/sink" :sinkmode :replace)]
    (?- sink query)))
