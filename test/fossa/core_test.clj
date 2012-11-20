(ns fossa.core-test
  (:use [midje sweet cascalog]
        fossa.core))

(fact
  "Test read-occurrences using sample dataset"
  (read-occurrences)
  => (produces-some [["Passer domesticus" "999999999" "-40.8747" "170.851" "" "2007" "6"]
                     ["Passer domesticus" "111111111" "-40.8747" "170.851" "10" "2007" ""]
                     ["Passer domesticus" "333333333" "-40.8747283" "170.851" "" "2007" ""]
                     ["Passer domesticus" "444444444" "-40.8747283" "170.851" "" "2007" ""]
                     ["Passer domesticus" "222222222" "-40.8747283" "170.851" "10" "2007" ""]]))

(fact
  "Test parse-occurrence-data"
  (parse-occurrence-data (read-occurrences))
  => (produces-some [["Passer domesticus" "INSERT INTO gbif_points (name, occids, precision, year, month, season, the_geom_multipoint) values ('Passer domesticus', '{\"999999999,111111111\", \"333333333,444444444,222222222\"}', '{\",10\", \",,10\"}', '{\"2007,2007\", \"2007,2007,2007\"}', '{\"6,\", \",,\"}', '{\"4,\", \",,\"}', ST_GeomFromText('MULTIPOINT (170.851 -40.8747, 170.851 -40.8747283)', 4326))"]]))

