#!/bin/sh

DATABASE='geosan'
USERNAME='aladoy'
HOSTNAME='localhost'

psql -h $HOSTNAME -U $USERNAME $DATABASE << EOF

DROP TABLE IF EXISTS geochronic.b_geo_vaud CASCADE;
DROP TABLE IF EXISTS geochronic.f1_geo_vaud CASCADE;
DROP TABLE IF EXISTS geochronic.f2_geo_vaud CASCADE;
DROP MATERIALIZED VIEW IF EXISTS lausanne_sectors_extent;

CREATE MATERIALIZED VIEW lausanne_sectors_extent AS 
SELECT sliver_killer(l.geometry,50::float) as geometry FROM (SELECT ST_Union(geometry) as geometry FROM lausanne_sectors) l;

CREATE TABLE geochronic.b_geo_vaud
AS
SELECT * FROM geochronic.colaus_b b
WHERE NOT ST_IsEmpty(b.geometry) AND ST_Intersects(b.geometry, (SELECT ST_Union(geometry) as geometry FROM vd_canton));

CREATE TABLE geochronic.f1_geo_vaud
AS
SELECT * FROM geochronic.colaus_f1 f1
INNER JOIN 
(SELECT pt, brnsws, datarrival, bthpl_dem, ethori_self, lvplyr, edtyp3, edtyp4, mrtsts2, cvdbase, cvdbase_adj FROM  geochronic.colaus_b) b
USING (pt)
WHERE NOT ST_IsEmpty(f1.geometry) AND ST_Intersects(f1.geometry, (SELECT ST_Union(geometry) as geometry FROM vd_canton));

CREATE TABLE geochronic.f2_geo_vaud
AS
SELECT * FROM geochronic.colaus_f2 f2
INNER JOIN 
(SELECT pt, brnsws, datarrival, bthpl_dem, ethori_self, lvplyr, edtyp3, edtyp4, mrtsts2, cvdbase, cvdbase_adj FROM  geochronic.colaus_b) b
USING (pt)
WHERE NOT ST_IsEmpty(f2.geometry) AND ST_Intersects(f2.geometry, (SELECT ST_Union(geometry) as geometry FROM vd_canton));

EOF