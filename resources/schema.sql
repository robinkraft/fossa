SELECT AddGeometryColumn('gbifpoints', 'the_geom_multipoint', 4326, 'MULTIPOINT', 2);
ALTER TABLE points ADD COLUMN occids text[];
ALTER TABLE points ADD COLUMN precision text[];
ALTER TABLE points ADD COLUMN year text[];
ALTER TABLE points ADD COLUMN month text[];
ALTER TABLE points ADD COLUMN season text[];

INSERT INTO gbifpoints (name, occids, the_geom_multipoint) values ('testname', '{"1","10,11,12,13"}', st_geomfromtext('MULTIPOINT ((0.896666666667 9.93166666667), (19.583334 47.166668))', 4326))

INSERT INTO gbifpoints (name, occids, precision, year, month, season, the_geom_multipoint) values ('Acidobacteria', '{"244664083", "244662763", "100000000,500000000", "244662123", "242136127", "242135147", "242135541", "244666043", "244661001", "244661391", "242135095"}', '{"", "", ",", "", "", "", "", "", "", "", ""}', '{"2007", "2007", "2008,2010", "2008", "2007", "2007", "2007", "1992", "2008", "2007", "2005"}', '{"6", "4", "10,3", "9", "3", "1", "6", "4", "5", "8", "3"}', '{"S winter", "S fall", "S spring,S fall", "N fall", "N spring", "N winter", "N summer", "N spring", "N summer", "N fall", "N spring"}', ST_GeomFromText('MULTIPOINT (170.8510000 -40.8747000, -175.5532000 -43.2880000, 0.0000000 0.0000000, 73.8830600 15.5097220, 35.4166667 31.4166667, -70.6707620 41.5265650, -70.6336000 41.5762330, 19.5833340 47.1666680, 6.1444330 53.4902700, -140.0880000 73.9670000, 0.8966667 9.9316667)', 4326))

SELECT (ST_DumpPoints(ST_Transform(t.the_geom_multipoint,3857))).geom AS the_geom_webmercator, unnest(t.occids) FROM gbifpoints AS t

SELECT * FROM (SELECT (ST_DumpPoints(ST_Transform(a.the_geom_multipoint,3857))).geom AS the_geom_webmercator, unnest(a.occids) AS occid, unnest(a.years) AS year from n AS a) g WHERE year ~ '1977'
