#!/bin/sh

DATABASE='geosan'
USERNAME='aladoy'
HOSTNAME='localhost'

psql -h $HOSTNAME -U $USERNAME $DATABASE << EOF

DROP MATERIALIZED VIEW IF EXISTS syndemic.cf2_laus_analysis;
DROP MATERIALIZED VIEW IF EXISTS syndemic.colaus_f2_complete;
DROP MATERIALIZED VIEW IF EXISTS syndemic.covid_laus_analysis;

CREATE MATERIALIZED VIEW syndemic.colaus_f2_complete
AS
SELECT * FROM syndemic.colaus_f2 f2
INNER JOIN 
(SELECT pt, brnsws, datarrival, bthpl_dem, ethori_self, lvplyr, edtyp3, edtyp4, mrtsts2, cvdbase, cvdbase_adj FROM  syndemic.colaus_b) b
USING (pt)
WHERE NOT ST_IsEmpty(f2.geometry);


CREATE MATERIALIZED VIEW syndemic.cf2_laus_analysis
AS
SELECT DISTINCT f.* 
FROM syndemic.colaus_f2_complete f, (SELECT ST_Union(geometry) as geometry FROM lausanne_sectors) laus 
WHERE ST_Intersects(f.geometry, laus.geometry);


CREATE MATERIALIZED VIEW syndemic.covid_laus_analysis
AS
SELECT DISTINCT g.*, EXTRACT(YEAR FROM date_reception) - EXTRACT(YEAR FROM date_naissance) as age
FROM geocovid.s3_20dfiltered_tests_geo g, lausanne_sectors l 
WHERE ST_Intersects(g.geometry, l.geometry)
AND EXTRACT(YEAR FROM date_reception) - EXTRACT(YEAR FROM date_naissance) >= 45;


EOF