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
  (get-season 1 1) => "N winter"
  (get-season -1 1) => "S summer"
  (get-season 1 3) => "N spring"
  (get-season -1 3) => "S fall"
  (get-season 1 4) => "N spring"
  (get-season -1 4) => "S fall"
  (get-season 1 7) => "N summer"
  (get-season -1 7) => "S winter"
  (get-season 1 10) => "N fall"
  (get-season -1 10) => "S spring")

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
  "Test mk-value-str. Formatting of result may look funky here, but if
   you print it with println it's ok...."
  (mk-value-str "ants"
                [[1234 3566] [6460]]
                [["" ""] [""]]
                [["2008" "2009"] ["2007"]]
                [["2" "3"] ["4"]]
                [["N winter" "N spring"] ["N spring"]])
  => (str "'ants', '{\"1234,3566\", \"6460\"}', '{\",\", \"\"}', "
          "'{\"2008,2009\", \"2007\"}', '{\"2,3\", \"4\"}', "
          "'{\"N winter,N spring\", \"N spring\"}'"))

(fact
  "Test mk-insert-stmt"
  (let [mp "MULTIPOINT (4 1, 5 2)"]
    (mk-insert-stmt
     (mk-value-str "Acidobacteria" [["1223445" "2302043"] ["2132424"]]
                   [[" " " "] [" "]] [["2007" "2008"] ["2009"]]
                   [["6" "7"] ["12"]] [["S spring" "S fall"] ["N fall"]]) mp))
  => (str "INSERT INTO gbif_points (name, occids, precision, year, month, "
          "season, the_geom_multipoint) values ('Acidobacteria', "
          "'{\"1223445,2302043\", \"2132424\"}', '{\" , \", \" \"}', "
          "'{\"2007,2008\", \"2009\"}', '{\"6,7\", \"12\"}', "
          "'{\"S spring,S fall\", \"N fall\"}', "
          "ST_GeomFromText('MULTIPOINT (4 1, 5 2)', 4326))"))
