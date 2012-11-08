(ns fossa.utils-test
  (:use [midje sweet cascalog]
        fossa.utils))

(def test-lats
  [1 2 3 1 3])

(def test-lons
  [4 5 6 4 6])

(def test-vals
  [1 2 3 4 5])

(fact
  (split-line "joe\tblow") => ["joe" "blow"])

(fact
  "Test stringification of latlons"
  (mk-latlon-str 1.2 2.3) => "1.2,2.3")

(facts
  "Test mk-latlon-map"
  (mk-latlon-map 1.2 2.3) => {"1.2,2.3" []}
  (mk-latlon-map 1.2 2.3 5) => {"1.2,2.3" [5]})

(fact
  "Test mk-empty-latlon-map"
  (mk-empty-latlon-map test-lats test-lons) => {"3,6" [], "2,5" [], "1,4" []})

(fact
  "Test collect-latlon-vals->map"
  (collect-latlon-vals->map [1 2 1] [3 4 3] [10 11 12]) => {"2,4" [11], "1,3" [10 12]})

(facts
  "Test latlon->coord-str"
  (latlon->coord-str 1 2) => "(2 1)"
  (latlon->coord-str [1 2]) => "(2 1)")

(fact
  (latlons->wkt-multi-point test-lats test-lons) => "MULTIPOINT ((4 1), (5 2), (6 3), (4 1), (6 3))"
  (latlons->wkt-multi-point [1 2 3] [4 5 6]) => "MULTIPOINT ((4 1), (5 2), (6 3))")

(fact
  "Test mk-sorted-map"
  (mk-sorted-map {:c 1 :b 2 :a 1}) => (into (sorted-map) {:c 1 :b 2 :a 1}))

(fact
  "Test parse-for-wkt"
  (parse-for-wkt test-lats test-lons) => "MULTIPOINT ((4 1), (5 2), (6 3))")

(facts
  "Test extract-field"
  (let [tuples [test-vals
                test-vals
                test-vals
                test-vals
                test-vals]]
    (extract-field tuples test-lats test-lons 1) => [[2 2] [2] [2 2]]
    (extract-field tuples test-lats test-lons 2) => [[3 3] [3] [3 3]]))

(fact
  "Test cleanup-slash-N - replacing \\N with empty string"
  (cleanup-slash-N "\\N") => ""
  (cleanup-slash-N "Really precise") => "Really precise")
