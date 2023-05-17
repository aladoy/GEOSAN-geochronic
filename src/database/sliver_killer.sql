CREATE OR REPLACE FUNCTION sliver_killer(geometry,float) RETURNS geometry AS
$$ SELECT ST_BuildArea(ST_Collect(a.geom)) as final_geom
FROM ST_DumpRings($1) AS a
WHERE a.path[1] = 0 OR
(a.path[1] > 0 AND ST_Area(a.geom) > $2)
$$
LANGUAGE 'sql' IMMUTABLE;