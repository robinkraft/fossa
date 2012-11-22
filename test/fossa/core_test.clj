(ns fossa.core-test
  (:use cascalog.api
        [midje sweet cascalog]
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
  (let [src (parse-occurrence-data (read-occurrences))]
    (<- [?stmt]
        (src _ ?stmt)))
  => (produces-some
      [["UPDATE gbif_points SET the_geom_multipoint = 'MULTIPOINT (0 0, -70.6336 41.576233, 6.144433 53.49027)' WHERE name = 'Acidobacteria';"]
       ["UPDATE gbif_points SET occids = '{\"100000000,500000000\", \"242135541\", \"244661001\"}' WHERE name = 'Acidobacteria';"]
       ["UPDATE gbif_points SET season = '{\"5,7\", \"2\", \"2\"}' WHERE name = 'Acidobacteria';"]
       ["UPDATE gbif_points SET precisions = '{\",\", \"\", \"\"}' WHERE name = 'Acidobacteria';"]
       ["UPDATE gbif_points SET years = '{\"2008,2010\", \"2007\", \"2008\"}' WHERE name = 'Acidobacteria';"]
       ["UPDATE gbif_points SET months = '{\"10,3\", \"6\", \"5\"}' WHERE name = 'Acidobacteria';"]]))
