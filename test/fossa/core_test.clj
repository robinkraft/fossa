(ns fossa.core-test
  (:use [midje sweet cascalog]
        fossa.core))

(fact
  (split-line "joe\tblow") => ["joe" "blow"])

(fact
  "Test read-occurrences using sample dataset"
  (read-occurrences)
  => (produces-some ["Acidobacteria" "242135095" "9.93166666667" "0.896666666667"]
                    ["Acidobacteria" "244666043" "47.166668" "19.583334"]
                    ["Acidobacteria" "244664083" "-40.8747" "170.851"]
                    ["Acidobacteria" "244662123" "15.509722" "73.88306"]))

(fact
  "Test latlons->wkt-multi-point"
  (latlons->wkt-multi-point [1 2 3] [4 5 6]) => "MULTIPOINT ((1 4), (2 5), (3 6))")

(fact
  "Test parse-occurrence-data"
  (parse-occurrence-data local-data)
  => (produces [["Acidobacteria"
                 ["242135095" "244666043" "244664083" "244662123" "244661391" "244662763" "242136127" "242135147" "242135541" "244661001"]
                 "MULTIPOINT ((9.93166666667 0.896666666667), (47.166668 19.583334), (-40.8747 170.851), (15.509722 73.88306), (73.967 -140.088), (-43.288 -175.5532), (31.41666667 35.41666667), (41.526565 -70.670762), (41.576233 -70.6336), (53.49027 6.144433))"]]))
