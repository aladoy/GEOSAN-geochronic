#!/bin/sh

DATABASE='geosan'
USERNAME='aladoy'
HOSTNAME='localhost'

psql -h $HOSTNAME -U $USERNAME $DATABASE << EOF

DROP TABLE IF EXISTS geochronic.f2_geo_vaud CASCADE;
DROP MATERIALIZED VIEW IF EXISTS geochronic.cf2_laus_analysis;
DROP MATERIALIZED VIEW IF EXISTS geochronic.covid_laus_analysis;
DROP MATERIALIZED VIEW IF EXISTS geochronic.population_reli;
DROP MATERIALIZED VIEW IF EXISTS lausanne_sectors_extent;

CREATE OR REPLACE FUNCTION sliver_killer(geometry,float) RETURNS geometry AS
$$ SELECT ST_BuildArea(ST_Collect(a.geom)) as final_geom
FROM ST_DumpRings($1) AS a
WHERE a.path[1] = 0 OR
(a.path[1] > 0 AND ST_Area(a.geom) > $2)
$$
LANGUAGE 'sql' IMMUTABLE;

CREATE MATERIALIZED VIEW lausanne_sectors_extent AS 
SELECT sliver_killer(l.geometry,50::float) as geometry FROM (SELECT ST_Union(geometry) as geometry FROM lausanne_sectors) l

CREATE TABLE geochronic.f2_geo_vaud
AS
SELECT * FROM geochronic.colaus_f2 f2
INNER JOIN 
(SELECT pt, brnsws, datarrival, bthpl_dem, ethori_self, lvplyr, edtyp3, edtyp4, mrtsts2, cvdbase, cvdbase_adj FROM  geochronic.colaus_b) b
USING (pt)
WHERE NOT ST_IsEmpty(f2.geometry) AND ST_Intersects(f2.geometry, (SELECT ST_Union(geometry) as geometry FROM vd_canton));

CREATE MATERIALIZED VIEW geochronic.cf2_laus_analysis
AS
SELECT DISTINCT f.* 
FROM geochronic.f2_geo_vaud f, (SELECT ST_Union(geometry) as geometry FROM lausanne_sectors) laus 
WHERE ST_Intersects(f.geometry, laus.geometry);

CREATE MATERIALIZED VIEW geochronic.covid_laus_analysis
AS
SELECT DISTINCT g.*, EXTRACT(YEAR FROM date_reception) - EXTRACT(YEAR FROM date_naissance) as age
FROM geocovid.s3_20dfiltered_tests_geo g, lausanne_sectors l 
WHERE ST_Intersects(g.geometry, l.geometry)
AND EXTRACT(YEAR FROM date_reception) - EXTRACT(YEAR FROM date_naissance) >= 45;


CREATE MATERIALIZED VIEW geochronic.population_reli
AS
SELECT m.reli, m.ptot_x as ptot, m.p4549f, m.p4549m, m.p5054f, m.p5054m, m.p5559f, m.p5559m, m.p6064f, m.p6064m, m.p6569f, m.p6569m,
m.p7074f, m.p7074m, m.p7579f, m.p7579m, m.p80mf, m.p80mm, re.geometry 
FROM microgis_ha m, lausanne_reli_polygon re
WHERE m.reli=re.reli;

EOF