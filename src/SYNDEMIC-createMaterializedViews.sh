#!/bin/sh

DATABASE='geosan'
USERNAME='aladoy'
HOSTNAME='localhost'

psql -h $HOSTNAME -U $USERNAME $DATABASE << EOF

DROP MATERIALIZED VIEW IF EXISTS syndemic.colaus_f2_complete;

CREATE MATERIALIZED VIEW syndemic.colaus_f2_complete
AS
SELECT * FROM syndemic.colaus_f2 f2
INNER JOIN 
(SELECT pt, brnsws, datarrival, bthpl_dem, ethori_self, lvplyr, edtyp3, edtyp4, mrtsts2, cvdbase, cvdbase_adj FROM  syndemic.colaus_b) b
USING (pt)
WHERE NOT ST_IsEmpty(f2.geometry);

EOF