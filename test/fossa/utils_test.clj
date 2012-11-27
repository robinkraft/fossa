(ns fossa.utils-test
  (:use [midje sweet cascalog]
        fossa.utils))

(def test-lats
  [1 2 3 1 3])

(def test-lons
  [4 5 6 4 6])

(def test-vals
  [1 2 3 4 5])

(def field-map
  (sorted-map "?lats" 0 "?lons" 1 "?month" 2 "?name" 3 "?year" 4))

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
  (latlon->coord-str 1 2) => "2 1"
  (latlon->coord-str [1 2]) => "2 1")

(fact
  (latlons->wkt-multi-point test-lats test-lons) => "MULTIPOINT (4 1, 5 2, 6 3, 4 1, 6 3)"
  (latlons->wkt-multi-point [1 2 3] [4 5 6]) => "MULTIPOINT (4 1, 5 2, 6 3)")

(fact
  "Test mk-sorted-map"
  (mk-sorted-map {:c 1 :b 2 :a 1}) => (into (sorted-map) {:c 1 :b 2 :a 1}))

(fact
  "Test parse-for-wkt"
  (parse-for-wkt test-lats test-lons) => "MULTIPOINT (4 1, 5 2, 6 3)")

(fact
  "Test extract - pull specific numbered fields out of defbufferop tuples"
  (let [tuples [[1 2 3]
             [2 3 4]]]
    (extract 1 tuples)) => [2 3])

(facts
  "Test extract-field"
  (let [tuples [test-vals
                test-vals
                test-vals
                test-vals
                test-vals]]
    (extract-field tuples test-lats test-lons 1) => [[2 2] [2] [2 2]]
    (extract-field tuples test-lats test-lons 2) => [[3 3] [3] [3 3]]))

(facts
  "Test valid-name?"
  (valid-name? "ants") => true
  (valid-name? "") => false
  (valid-name? nil) => false)

(facts
  "Test valid-latlon?"
  (valid-latlon? 1.2 2.3) => true
  (valid-latlon? "1.2" "2.3") => true
  (valid-latlon? 100 100.) => false
  (valid-latlon? "100" "100.") => false)

(facts
  "Test str->num-or-empty-str"
  (str->num-or-empty-str "1.2") => 1.2
  (str->num-or-empty-str "\\N") => "")

(facts
  "Test handle-zeros"
  (handle-zeros "3.") => "3"
  (handle-zeros "3.0") => "3"
  (handle-zeros "3.0000") => "3"
  (handle-zeros "3.00100") => "3.00100"
  (handle-zeros "3") => "3"
  (handle-zeros "3.445480") => "3.445480"
  (handle-zeros "3.1234567890") => "3.1234567890")

(facts
  "Test round-to"
  (round-to 7 3) => "3"
  (round-to 7 3.1234567890) => "3.1234568"
  (round-to 7 3.0) => "3"
  (round-to 7 3.120) => "3.12"
  (round-to 7 3.1000000) => "3.1"
  (round-to 7 3.10000009) => "3.1000001"
  (round-to 7 300) => "300"
  (round-to 7 300.0) => "300"
  (round-to 7 300.123456789) => "300.1234568"
  (round-to 7 -3) => "-3"
  (round-to 7 -3.1234567890) => "-3.1234568"
  (round-to 7 -3.0) => "-3"
  (round-to 7 -3.120) => "-3.12"
  (round-to 7 -3.1000000) => "-3.1"
  (round-to 7 -3.10000009) => "-3.1000001"
  (round-to 7 -300) => "-300"
  (round-to 7 -300.0) => "-300"
  (round-to 7 -300.123456789) => "-300.1234568")

(fact
  "Test parse-hemisphere"
  (parse-hemisphere "N") => {0 "winter" 1 "spring" 2 "summer" 3 "fall"}
  (parse-hemisphere "S") => {0 "summer" 1 "fall" 2 "winter" 3 "spring"})

(fact
  "Test get-season-idx"
  (get-season-idx 1) => 0
  (get-season-idx 3) => 1
  (get-season-idx 4) => 1
  (get-season-idx 6) => 2
  (get-season-idx 7) => 2)

(fact
  "Test get-season"
  (get-season 1 1) => "0"
  (get-season -1 1) => "6"
  (get-season 1 3) => "1"
  (get-season -1 3) => "7"
  (get-season 1 4) => "1"
  (get-season -1 4) => "7"
  (get-season 1 7) => "2"
  (get-season -1 7) => "4"
  (get-season 1 10) => "3"
  (get-season -1 10) => "5")

(facts
  "Test surround-str"
  (surround-str "yo" "'") => "'yo'"
  (surround-str "Acidobacteria" "!!!!") => "!!!!Acidobacteria!!!!"
  (surround-str "ada" "r") => "radar")

(facts
  "Test concat-results"
  (concat-results ["2008" "2009"] ",") => "2008,2009"
  (concat-results ["3" "5"] ",") => "3,5"
  (concat-results ["" ""] ",") => ",")

(fact
  "Test prep-vals"
  (prep-vals [["2008" "2009"] ["2009"]]) => "'{\"2008,2009\", \"2009\"}'")

(fact
  "Test mk-update-stmt"
  (mk-update-stmt "Acidobacteria" "years" "'{\"2008,2009\", \"2009\"}'")
  => "UPDATE gbif_points SET years = '{\"2008,2009\", \"2009\"}' WHERE name = 'Acidobacteria';")

(fact
  "Test data->update-stmt"
  (let [sci-name "Passer"
        field-name "occids"
        field-num 2
        tuples [["-1.7" "29.3" "99999999" "" "2007" "8" "1"]
                ["-1.7" "29.3" "11111111" "" "2007" "8" "1"]
                ["-1" "30" "22222222" "" "2012" "3" "4"]]
        lats (extract 0 tuples)
        lons (extract 1 tuples)]
    (data->update-stmt tuples lats lons sci-name field-name field-num))
  => "UPDATE gbif_points SET occids = '{\"22222222\", \"99999999,11111111\"}' WHERE name = 'Passer';")

(fact
  "Test data->update-stmt"
  (let [tuples [["1.2" "4.5" "Ursus" "2007" "2008"]
                ["2.3" "5.6" "Ursus" "2009" "2010"]]]
    (data->update-stmt tuples ["1.2" "4.5"] ["3.4" "5.6"]
                       "Ursus" "years" 3))
  => "UPDATE gbif_points SET years = '{\"2007\", \"2009\"}' WHERE name = 'Ursus';")

(fact
  "Test wkt-str->hex"
  (wkt-str->hex "MULTIPOINT (4 1, 5 2, 6 3)") => "000000000400000003000000000140100000000000003FF0000000000000000000000140140000000000004000000000000000000000000140180000000000004008000000000000")

(fact
  "Test fmt-for-geom-func"
  (fmt-for-geom-func (wkt-str->hex "MULTIPOINT (4 1, 5 2, 6 3)"))
  => "ST_GeomFromWKB(ST_AsBinary(000000000400000003000000000140100000000000003FF0000000000000000000000140140000000000004000000000000000000000000140180000000000004008000000000000::geometry), 4326)")

(fact
  "Test mk-multipoint-update"
  (mk-multipoint-update "Passer" [1 2 3 3] [4 5 6 6])
  => "UPDATE gbif_points SET the_geom_multipoint = ST_GeomFromWKB(ST_AsBinary('000000000400000003000000000140100000000000003FF0000000000000000000000140140000000000004000000000000000000000000140180000000000004008000000000000'::geometry), 4326) WHERE name = 'Passer';")

(fact
  "Test get-parse-fields"
  (get-parse-fields field-map) => ["?month" "?year"])

(fact
  "Test get-field-idxs"
  (get-field-idxs field-map) => [2 4])

(fact
  "Test drop-q-mark"
  (drop-q-mark field-map) => ["month" "year"])
