(ns fossa.core-test
  (:use [midje sweet cascalog]
        fossa.core))

(fact
  "Test read-occurrences using sample dataset"
  (read-occurrences)
  => (produces-some [["Acidobacteria" "41.5265650" "-70.6707620" "242135147" "" "2007" "1"]]))

(fact
  "Test parse-occurrence-data"
  (parse-occurrence-data)
  => (produces [["Acidobacteria" "INSERT INTO gbif_points (name, occids, precision, year, month, season, the_geom_multipoint) values ('Acidobacteria', '{\"244664083\", \"244662763\", \"100000000,500000000\", \"244662123\", \"242136127\", \"242135147\", \"242135541\", \"244666043\", \"244661001\", \"244661391\", \"242135095\"}', '{\"\", \"\", \",\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\"}', '{\"2007\", \"2007\", \"2008,2010\", \"2008\", \"2007\", \"2007\", \"2007\", \"1992\", \"2008\", \"2007\", \"2005\"}', '{\"6\", \"4\", \"10,3\", \"9\", \"3\", \"1\", \"6\", \"4\", \"5\", \"8\", \"3\"}', '{\"S winter\", \"S fall\", \"S spring,S fall\", \"N fall\", \"N spring\", \"N winter\", \"N summer\", \"N spring\", \"N summer\", \"N fall\", \"N spring\"}', ST_GeomFromText('MULTIPOINT (170.8510000 -40.8747000, -175.5532000 -43.2880000, 0.0000000 0.0000000, 73.8830600 15.5097220, 35.4166667 31.4166667, -70.6707620 41.5265650, -70.6336000 41.5762330, 19.5833340 47.1666680, 6.1444330 53.4902700, -140.0880000 73.9670000, 0.8966667 9.9316667)', 4326))"]]))

