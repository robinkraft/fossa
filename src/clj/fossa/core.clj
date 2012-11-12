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

(defn passer-domesticus?
  "Return true if supplied sciname is Passer domesticus, otherwise false."
  [sciname]
  (= "passer domesticus" (clojure.string/trim (clojure.string/lower-case sciname))))

(defn read-occurrences
  "Returns a Cascalog generator of occurence fields for supplied data path."
  ([]
     (read-occurrences local-data))
  ([path]
     (let [digits 7
           src (hfs-textline path)]
       (<- [?scientificname ?lat-str ?lon-str ?occurrenceid ?prec ?year ?month]
           (src ?line)
           (u/split-line ?line :>> occ-fields)
           (not-ebird ?dataresourceid) ;; Filter out eBird (See http://goo.gl/4OMLl)
           (passer-domesticus? ?scientificname) ;; For test data only.
           (u/cleanup-slash-N ?coordinateprecision :> ?prec)
           (u/valid-name? ?scientificname)
           (u/latlon-valid? ?latitude ?longitude)
           ((c/each #'read-string) ?latitude ?longitude :> ?lat ?lon)
           (u/truncate-latlon ?lat ?lon digits :> ?lat-str ?lon-str)
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
  (<- [?sci-name ?stmt]
      (occ-src ?sci-name ?lat ?lon ?occ-id ?prec ?year ?month)
      (u/get-season ?lat ?month :> ?season)
      (collect-by-latlon ?lat ?lon ?occ-id ?prec ?year ?month ?season
                         :> ?multipoint ?occ-ids ?precs ?yrs ?mos ?seasons)
      (u/mk-value-str ?sci-name ?occ-ids ?precs ?yrs ?mos ?seasons :> ?val-str)
      (u/mk-insert-stmt ?val-str ?multipoint :> ?stmt))))
