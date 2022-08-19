#!/bin/sh

DATABASE='geosan'
USERNAME='aladoy'
HOSTNAME='localhost'

psql -h $HOSTNAME -U $USERNAME $DATABASE << EOF

DROP TABLE IF EXISTS syndemic.colaus_b;

CREATE TABLE syndemic.colaus_b(
    pt integer PRIMARY KEY,
    sex integer NOT NULL CHECK (sex IN (0,1)),
    datexam date NOT NULL,
    age numeric NOT NULL,
    brnsws integer CHECK (brnsws IS NULL OR brnsws IN (0,1)),
    datarrival date,
    bthpl_dem text,
    ethori_self text CHECK (ethori_self IS NULL OR ethori_self IN ('W', 'X', 'B', 'O', 'A', 'K')),
    lvplyr numeric,
    edtyp3 integer CHECK (edtyp3 IS NULL OR edtyp3 IN (1,2,3)),
    edtyp4 integer CHECK (edtyp4 IS NULL OR edtyp4 IN (1,2,3,4)),
    mrtsts2 integer CHECK (mrtsts2 IS NULL OR mrtsts2 IN (0,1)),
    job_curr8 integer CHECK (job_curr8 IS NULL OR job_curr8 IN (1,2,3,9)),
    cvdbase integer CHECK (cvdbase IS NULL OR cvdbase IN (0,1)),
    cvdbase_adj integer CHECK (cvdbase_adj IS NULL OR cvdbase_adj=1),
    conso_hebdo integer CHECK (conso_hebdo >= 0),
    sbsmk integer CHECK (sbsmk IS NULL OR sbsmk IN (0,1,2)),
    HTA integer CHECK (HTA IS NULL OR HTA IN (0,1)),
    hctld integer CHECK (hctld IS NULL OR hctld IN (0,1,9)),
    dbtld integer CHECK (dbtld IS NULL OR dbtld IN (0,1,9)),
    DIAB integer CHECK (DIAB IS NULL OR DIAB IN (0,1)),
    phyact integer CHECK (phyact IS NULL OR phyact IN (0,1,2,9)),
    Polypharm integer CHECK (Polypharm IN (0,1)),
    F0gaf_l numeric,
    F0gaf_w numeric,
    F0gaf_c numeric,
    F0STs_tot numeric,
    F0STt_tot numeric,
    strname text,
    deinr text,
    plz4 integer,
    municipality text,
    coordx_21781 numeric,
    coordy_21781 numeric,
    strname_init text,
    deinr_init text,
    plz4_init integer,
    geometry geometry(Point, 2056)
);


DROP TABLE IF EXISTS syndemic.colaus_f1;

CREATE TABLE syndemic.colaus_f1(
    pt integer PRIMARY KEY,
    F1sex integer NOT NULL CHECK (F1sex IN (0,1)),
    F1datexam date NOT NULL,
    F1age numeric NOT NULL,
    F1mrtsts2 integer CHECK (F1mrtsts2 IS NULL OR F1mrtsts2 IN (0,1)),
    F1job_curr8 integer CHECK (F1job_curr8 IS NULL OR F1job_curr8 IN (1,2,3,9)),
    F1conso_hebdo integer CHECK (F1conso_hebdo >= 0),
    F1sbsmk integer CHECK (F1sbsmk IS NULL OR F1sbsmk IN (0,1,2)),
    F1HTA integer CHECK (F1HTA IS NULL OR F1HTA IN (0,1)),
    F1hctld integer CHECK (F1hctld IS NULL OR F1hctld IN (0,1,9)),
    F1dbtld integer CHECK (F1dbtld IS NULL OR F1dbtld IN (0,1,9)),
    F1DIAB integer CHECK (F1DIAB IS NULL OR F1DIAB IN (0,1)),
    Polypharm integer CHECK (Polypharm IN (0,1)),
    F1gaf_l numeric,
    F1gaf_w numeric,
    F1gaf_c numeric,
    F1STs_tot numeric,
    F1STt_tot numeric,
    F1CVD integer CHECK (F1CVD IN (0,1)),
    strname text,
    deinr text,
    plz4 integer,
    municipality text,
    coordx_21781 numeric,
    coordy_21781 numeric,
    strname_init text,
    deinr_init text,
    plz4_init integer,
    geometry geometry(Point, 2056)
);

DROP TABLE IF EXISTS syndemic.colaus_f2;

CREATE TABLE syndemic.colaus_f2(
    pt integer PRIMARY KEY,
    F2sex integer CHECK (F2sex IN (0,1)),
    F2datexam date,
    F2datquest date,
    F2age numeric,
    F2mrtsts integer CHECK (F2mrtsts IS NULL OR F2mrtsts IN (0,2,3)),
    F2mrtsts2 integer CHECK (F2mrtsts2 IS NULL OR F2mrtsts2 IN (0,1)),
    F2dmst integer CHECK (F2dmst IS NULL OR F2dmst IN (0,1,2,3,8)),
    F2nochd integer CHECK (F2nochd >= 0),
    F2sclhlp1 integer CHECK (F2sclhlp1 IS NULL OR F2sclhlp1 IN (0,1,9)),
    F2sclhlp2 integer CHECK (F2sclhlp2 IS NULL OR F2sclhlp2 IN (0,1,9)),
    F2job_curr1 integer CHECK (F2job_curr1 IS NULL OR F2job_curr1 IN (0,1)),
    F2job_curr4 integer CHECK (F2job_curr4 IS NULL OR F2job_curr4 IN (1,5,8,9)),
    F2job_curr7 integer CHECK (F2job_curr7 >= 0),
    F2job_not1 integer CHECK (F2job_not1 IS NULL OR F2job_not1 IN (0,1,2,3,4,5,6,9)),
    F2job_prev4 integer CHECK (F2job_prev4 >= 0),
    F2family18 integer CHECK (F2family18 >= 0),
    F2income1 integer CHECK (F2income1 IS NULL OR F2income1 IN (0,1,2,3,4,5,8,9)),
    F2income2 integer CHECK (F2income2 IS NULL OR F2income2 IN (0,1,2,3)),
    F2income3 integer CHECK (F2income3 IS NULL OR F2income3 IN (0,1,2,3,4,5,8,9,99)),
    F2income4 integer CHECK (F2income4 >= 0),
    F2income5 integer CHECK (F2income5 IS NULL OR F2income5 IN (0,1,2,3,9)),
    F2income6 integer CHECK (F2income6 IS NULL OR F2income6 IN (1,2,9)),
    F2Quest1 integer CHECK (F2Quest1 IS NULL OR F2Quest1 IN (0,1,2,3,4,5)),
    F2conso_hebdo integer CHECK (F2conso_hebdo >= 0),
    F2alcool2 integer CHECK (F2alcool2 IS NULL OR F2alcool2 IN (0,1,2,3)),
    F2sbsmk integer CHECK (F2sbsmk IS NULL OR F2sbsmk IN (0,1,2)),
    F2equiv numeric CHECK (F2equiv >= 0),
    F2hypdr integer CHECK (F2hypdr IS NULL OR F2hypdr IN (1,2,9)),
    F2crbpmed integer CHECK (F2crbpmed IS NULL OR F2crbpmed IN (1,2,8,9)),
    F2HTA integer CHECK (F2HTA IS NULL OR F2HTA IN (0,1)),
    F2hctld integer CHECK (F2hctld IS NULL OR F2hctld IN (0,1,9)),
    F2dbtld integer CHECK (F2dbtld IS NULL OR F2dbtld IN (0,1,9)),
    F2DIAB integer CHECK (F2DIAB IS NULL OR F2DIAB IN (0,1)),
    F2care1 integer CHECK (F2care1 >= 0),
    F2care1b integer CHECK (F2care1b >= 0),
    F2care2 integer CHECK (F2care2 >= 0),
    F2seden integer CHECK (F2seden IS NULL OR F2seden IN (0,1)),
    F2BMI numeric CHECK (F2BMI >= 0),
    F2BMI_cat2 integer CHECK (F2BMI_cat2 IS NULL OR F2BMI_cat2 IN (0,1,2,3)),
    F2waist_cat integer CHECK (F2waist_cat IS NULL OR F2waist_cat IN (0,1)),
    F2na numeric CHECK (F2na >= 0),
    F2k numeric CHECK (F2k >= 0),
    F2cr numeric CHECK (F2cr >= 0),
    Polypharm integer CHECK (Polypharm IN (0,1)),
    F2gaf_l numeric,
    F2gaf_w numeric,
    F2gaf_c numeric,
    F2STs_tot numeric,
    F2STt_tot numeric,
    F2CVD integer CHECK (F2CVD IN (0,1)),
    strname text,
    deinr text,
    plz4 integer,
    municipality text,
    coordx_21781 numeric,
    coordy_21781 numeric,
    strname_init text,
    deinr_init text,
    plz4_init text,
    geometry geometry(Point, 2056)
);

DROP TABLE IF EXISTS syndemic.colaus_f3;

CREATE TABLE syndemic.colaus_f3(
    pt integer PRIMARY KEY,
    F3sex integer NOT NULL CHECK (F3sex IN (0,1)),
    F3datexam date,
    F3datquest date,
    F3age numeric,
    F3mrtsts integer CHECK (F3mrtsts IS NULL OR F3mrtsts IN (0,2,3)),
    F3mrtsts2 integer CHECK (F3mrtsts2 IS NULL OR F3mrtsts2 IN (0,1)),
    F3dmst integer CHECK (F3dmst IS NULL OR F3dmst IN (0,1,2,3,4)),
    F3nochd integer CHECK (F3nochd >= 0),
    F3sclhlp3 integer CHECK (F3sclhlp3 IS NULL OR F3sclhlp3 IN (0,1,9)),
    F3job_curr1 integer CHECK (F3job_curr1 IS NULL OR F3job_curr1 IN (0,1)),
    F3job_curr4 integer CHECK (F3job_curr4 >= 0),
    F3job_curr7 integer CHECK (F3job_curr7 IS NULL OR F3job_curr7 IN (1,23,24)),
    F3job_not2 integer CHECK (F3job_not2 IS NULL OR F3job_not2 IN (0,1,9)),
    F3job_prev3 integer CHECK (F3job_prev3 IS NULL OR F3job_prev3 IN (1,2,4,6,11)),
    F3income3 integer CHECK (F3income3 IS NULL OR F3income3 IN (0,1,2,3,4,5,6,9)),
    F3income4 integer CHECK (F3income4 IS NULL OR F3income4 IN (0,1,2,3,4,5,6,9)),
    F3income5 integer CHECK (F3income5 IS NULL OR F3income5 IN (0,1,3,4,9)),
    F3income6 integer CHECK (F3income6 IS NULL OR F3income6 IN (1,2,9)),
    F3assur1 integer CHECK (F3assur1 IS NULL OR F3assur1 IN (1,2,3,4,5,9)),
    F3Quest1 integer CHECK (F3Quest1 IS NULL OR F3Quest1 IN (1,2,3,4,5)),
    F3conso_hebdo integer CHECK (F3conso_hebdo >= 0),
    F3alcool2 integer CHECK (F3alcool2 IS NULL OR F3alcool2 IN (0,1,2,3)),
    F3sbsmk integer CHECK (F3sbsmk IS NULL OR F3sbsmk IN (0,1,2)),
    F3equiv numeric CHECK (F3equiv >= 0),
    F3hypdr integer CHECK (F3hypdr IS NULL OR F3hypdr IN (0,1)),
    F3crbpmed integer CHECK (F3crbpmed IS NULL OR F3crbpmed IN (0,1)),
    F3HTA integer CHECK (F3HTA IS NULL OR F3HTA IN (0,1)),
    F3hctld integer CHECK (F3hctld IS NULL OR F3hctld IN (0,1)),
    F3dbtld integer CHECK (F3dbtld IS NULL OR F3dbtld IN (0,1)),
    F3DIAB integer CHECK (F3DIAB IS NULL OR F3DIAB IN (0,1)),
    F3care1 integer CHECK (F3care1 IS NULL OR F3care1 IN (0,1)),
    F3care1a integer CHECK (F3care1a >= 0),
    F3care1b integer CHECK (F3care1b >= 0),
    F3care3 integer CHECK (F3care3 IS NULL OR F3care3 IN (0,1)),
    F3care3a integer CHECK (F3care3a >= 0),
    F3care4 integer CHECK (F3care4 IS NULL OR F3care4 IN (1,2)),
    F3care4a integer CHECK (F3care4a >= 0),
    F3IPAQ_excl integer CHECK (F3IPAQ_excl IS NULL OR F3IPAQ_excl IN (0,1,2)),
    F3IPAQ_score integer CHECK (F3IPAQ_score IS NULL OR F3IPAQ_score IN (0,1,2)),
    F3IPAQ_MET_tot numeric CHECK (F3IPAQ_MET_tot >= 0),
    F3BMI numeric CHECK (F3BMI >= 0),
    F3BMI_cat2 integer CHECK (F3BMI_cat2 IS NULL OR F3BMI_cat2 IN (0,1,2,3)),
    F3waist_cat integer CHECK (F3waist_cat IS NULL OR F3waist_cat IN (0,1)),
    F3na numeric CHECK (F3na >= 0),
    F3k numeric CHECK (F3k >= 0),
    F3cr numeric CHECK (F3cr >= 0),
    Polypharm integer CHECK (Polypharm IN (0,1)),
    F3gaf_l numeric,
    F3gaf_w numeric,
    F3gaf_c numeric,
    F3STs_tot numeric,
    F3STt_tot numeric,
    F3CVD integer CHECK (F3CVD IN (0,1)),
    strname text,
    deinr text,
    plz4 integer,
    municipality text,
    coordx_21781 numeric,
    coordy_21781 numeric,
    strname_init text,
    deinr_init text,
    plz4_init text,
    geometry geometry(Point, 2056)
);


EOF












