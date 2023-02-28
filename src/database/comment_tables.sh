#!/bin/sh

DATABASE='geosan'
USERNAME='aladoy'
HOSTNAME='localhost'

psql -h $HOSTNAME -U $USERNAME $DATABASE << EOF

COMMENT ON COLUMN geochronic.colaus_b.pt IS 'CoLaus ID';
COMMENT ON COLUMN geochronic.colaus_b.sex IS 'Sex, numeric, 1=male, 0=female';
COMMENT ON COLUMN geochronic.colaus_b.datexam IS 'Date of physical examination (if done)';
COMMENT ON COLUMN geochronic.colaus_b.age IS 'Age (years)';
COMMENT ON COLUMN geochronic.colaus_b.brnsws IS 'Born in Switzerland';
COMMENT ON COLUMN geochronic.colaus_b.datarrival IS 'Date of arrival in Switzerland';
COMMENT ON COLUMN geochronic.colaus_b.bthpl_dem IS 'Country of birth';
COMMENT ON COLUMN geochronic.colaus_b.ethori_self IS 'Self-reported ethnicity';
COMMENT ON COLUMN geochronic.colaus_b.lvplyr IS 'How many years did you live in Switzerland';
COMMENT ON COLUMN geochronic.colaus_b.edtyp3 IS 'Educational level (3 gr)';
COMMENT ON COLUMN geochronic.colaus_b.edtyp4 IS 'Educational level (4 gr)';
COMMENT ON COLUMN geochronic.colaus_b.job_curr8 IS 'Job type (4 cats)';
COMMENT ON COLUMN geochronic.colaus_b.mrtsts2 IS 'Marital status (2 cats)';
COMMENT ON COLUMN geochronic.colaus_b.cvdbase IS 'Baseline cardiovascular disease, reported';
COMMENT ON COLUMN geochronic.colaus_b.cvdbase_adj IS 'Baseline cardiovascular disease, adjudicated';
COMMENT ON COLUMN geochronic.colaus_b.conso_hebdo IS 'Weekly alcohol consumption (units)';
COMMENT ON COLUMN geochronic.colaus_b.sbsmk IS 'Smoking status';
COMMENT ON COLUMN geochronic.colaus_b.hta IS 'Hypertension (>=140/90 or ttt)';
COMMENT ON COLUMN geochronic.colaus_b.hctld IS 'Have you ever been told that you had high cholesterol';
COMMENT ON COLUMN geochronic.colaus_b.dbtld IS 'Have you ever been told that you had diabetes';
COMMENT ON COLUMN geochronic.colaus_b.diab IS 'Diabetes using FPG>=7.0 mmol/L (Y/N)';
COMMENT ON COLUMN geochronic.colaus_b.phyact IS 'Physical activity (min 20 min/week)';
COMMENT ON COLUMN geochronic.colaus_b.polypharm IS 'Polypharmacy (5 or more daily medications)';
COMMENT ON COLUMN geochronic.colaus_b.f0gaf_l IS 'Global Assessment of Functioning - lifetime';
COMMENT ON COLUMN geochronic.colaus_b.f0gaf_w IS 'Global Assessment of Functioning - worst';
COMMENT ON COLUMN geochronic.colaus_b.f0gaf_c IS 'Global Assessment of Functioning - current';
COMMENT ON COLUMN geochronic.colaus_b.f0STs_tot IS 'State-Trait Anxiety Inventory- state (current)';
COMMENT ON COLUMN geochronic.colaus_b.f0STt_tot IS 'State-Trait Anxiety Inventory- trait (lifetime)';
COMMENT ON COLUMN geochronic.colaus_b.init_addr IS 'Address provided by participant';
COMMENT ON COLUMN geochronic.colaus_b.matching_addr IS 'Address used for geocoding';
COMMENT ON COLUMN geochronic.colaus_b.egid IS 'Building ID';
COMMENT ON COLUMN geochronic.colaus_b.note_geocoding IS 'Geocoding accuracy';
COMMENT ON COLUMN geochronic.colaus_b.gkode IS 'X-coord (EPSG:2056)';
COMMENT ON COLUMN geochronic.colaus_b.gkodn IS 'Y-coord (EPSG:2056)';


COMMENT ON COLUMN geochronic.colaus_f1.pt IS 'CoLaus ID';
COMMENT ON COLUMN geochronic.colaus_f1.f1sex IS 'Sex, numeric, 1=male, 0=female';
COMMENT ON COLUMN geochronic.colaus_f1.f1datexam IS 'Date of physical examination (if done)';
COMMENT ON COLUMN geochronic.colaus_f1.f1age IS 'Age (years)';
COMMENT ON COLUMN geochronic.colaus_f1.f1mrtsts2 IS 'Marital status (2 cats)';
COMMENT ON COLUMN geochronic.colaus_f1.f1job_curr8 IS 'Job type (4 cats)';
COMMENT ON COLUMN geochronic.colaus_f1.f1conso_hebdo IS 'Weekly alcohol consumption (units)';
COMMENT ON COLUMN geochronic.colaus_f1.f1sbsmk IS 'Smoking status';
COMMENT ON COLUMN geochronic.colaus_f1.f1hta IS 'Hypertension (>=140/90 or ttt)';
COMMENT ON COLUMN geochronic.colaus_f1.f1hctld IS 'Have you ever been told that you had high cholesterol';
COMMENT ON COLUMN geochronic.colaus_f1.f1dbtld IS 'Have you ever been told that you had diabetes';
COMMENT ON COLUMN geochronic.colaus_f1.f1diab IS 'Diabetes using FPG>=7.0 mmol/L (Y/N)';
COMMENT ON COLUMN geochronic.colaus_f1.polypharm IS 'Polypharmacy (5 or more daily medications)';
COMMENT ON COLUMN geochronic.colaus_f1.f1gaf_l IS 'Global Assessment of Functioning - lifetime';
COMMENT ON COLUMN geochronic.colaus_f1.f1gaf_w IS 'Global Assessment of Functioning - worst';
COMMENT ON COLUMN geochronic.colaus_f1.f1gaf_c IS 'Global Assessment of Functioning - current';
COMMENT ON COLUMN geochronic.colaus_f1.f1sts_tot IS 'State-Trait Anxiety Inventory- state (current)';
COMMENT ON COLUMN geochronic.colaus_f1.f1stt_tot IS 'State-Trait Anxiety Inventory- trait (lifetime)';
COMMENT ON COLUMN geochronic.colaus_f1.f1cvd IS 'Cardiovascular disease since 2009/last part';
COMMENT ON COLUMN geochronic.colaus_f1.init_addr IS 'Address provided by participant';
COMMENT ON COLUMN geochronic.colaus_f1.matching_addr IS 'Address used for geocoding';
COMMENT ON COLUMN geochronic.colaus_f1.egid IS 'Building ID';
COMMENT ON COLUMN geochronic.colaus_f1.note_geocoding IS 'Geocoding accuracy';
COMMENT ON COLUMN geochronic.colaus_f1.gkode IS 'X-coord (EPSG:2056)';
COMMENT ON COLUMN geochronic.colaus_f1.gkodn IS 'Y-coord (EPSG:2056)';


COMMENT ON COLUMN geochronic.colaus_f2.pt IS 'CoLaus ID';
COMMENT ON COLUMN geochronic.colaus_f2.f2sex IS 'Sex, numeric, 1=male, 0=female';
COMMENT ON COLUMN geochronic.colaus_f2.f2datexam IS 'Date of physical examination (if done)';
COMMENT ON COLUMN geochronic.colaus_f2.f2datquest IS 'Date of questionnaire (if done)';
COMMENT ON COLUMN geochronic.colaus_f2.f2age IS 'Age (years)';
COMMENT ON COLUMN geochronic.colaus_f2.f2mrtsts IS 'Current marital status';
COMMENT ON COLUMN geochronic.colaus_f2.f2mrtsts2 IS 'Marital status (2 cats)';
COMMENT ON COLUMN geochronic.colaus_f2.f2dmst IS 'Current domestic situation';
COMMENT ON COLUMN geochronic.colaus_f2.f2nochd IS 'How many children do you have?';
COMMENT ON COLUMN geochronic.colaus_f2.f2sclhlp1 IS 'Are you receiving retirement social benefits (AVS)?';
COMMENT ON COLUMN geochronic.colaus_f2.f2sclhlp2 IS 'Are you receiving invalidity social benefits (AI)?';
COMMENT ON COLUMN geochronic.colaus_f2.f2job_curr1 IS 'Are you currently engaged in a professional activity?';
COMMENT ON COLUMN geochronic.colaus_f2.f2job_curr4 IS 'Current professional situation (work time)';
COMMENT ON COLUMN geochronic.colaus_f2.f2job_curr7 IS 'Current occupational position (ESEC)';
COMMENT ON COLUMN geochronic.colaus_f2.f2job_not1 IS 'Reasons for not working';
COMMENT ON COLUMN geochronic.colaus_f2.f2job_prev4 IS 'Last known occupational position (ESEC)';
COMMENT ON COLUMN geochronic.colaus_f2.f2family18 IS 'Household highest occupation (ESEC)';
COMMENT ON COLUMN geochronic.colaus_f2.f2income1 IS 'Monthly household gross income';
COMMENT ON COLUMN geochronic.colaus_f2.f2income2 IS 'Monthly household gross income (Tertiles)';
COMMENT ON COLUMN geochronic.colaus_f2.f2income3 IS 'Nb. of persons <15y in the household depending on income';
COMMENT ON COLUMN geochronic.colaus_f2.f2income4 IS 'Nb. of persons >=15y in the household depending on income';
COMMENT ON COLUMN geochronic.colaus_f2.f2income5 IS 'Real financial difficulties to meet needs';
COMMENT ON COLUMN geochronic.colaus_f2.f2income6 IS 'Renouncing to health care due to financial difficulties';
COMMENT ON COLUMN geochronic.colaus_f2.f2Quest1 IS 'How would you rate your health?';
COMMENT ON COLUMN geochronic.colaus_f2.f2conso_hebdo IS 'Weekly alcohol consumption (units)';
COMMENT ON COLUMN geochronic.colaus_f2.f2alcool2 IS 'Alcohol consumption (4 groups)';
COMMENT ON COLUMN geochronic.colaus_f2.f2sbsmk IS 'Smoking status';
COMMENT ON COLUMN geochronic.colaus_f2.f2equiv IS 'Cigarette equivalents';
COMMENT ON COLUMN geochronic.colaus_f2.f2hypdr IS 'Did a doctor tell you that you were hypertensive (since CoLaus 2)?';
COMMENT ON COLUMN geochronic.colaus_f2.f2crbpmed IS 'Do you take medication for high blood pressure?';
COMMENT ON COLUMN geochronic.colaus_f2.f2hta IS 'Hypertension (>=140/90 or ttt)';
COMMENT ON COLUMN geochronic.colaus_f2.f2hctld IS 'Did a doctor tell you that you had high cholesterol (since CoLaus 2)?';
COMMENT ON COLUMN geochronic.colaus_f2.f2hypolip IS 'Hypolipidemic drug treatment (Y/N)';
COMMENT ON COLUMN geochronic.colaus_f2.f2dbtld IS 'Have you ever been told that you have diabetes?';
COMMENT ON COLUMN geochronic.colaus_f2.f2diab IS 'Diabetes using FPG>=7.0 mmol/L (Y/N)';
COMMENT ON COLUMN geochronic.colaus_f2.f2orldrg IS 'Oral antidiabetic drugs (Y/N)';
COMMENT ON COLUMN geochronic.colaus_f2.f2insn IS 'Insulin (Y/N)';
COMMENT ON COLUMN geochronic.colaus_f2.f2antidiab IS 'Antidiabetic drug treatment (Y/N)';
COMMENT ON COLUMN geochronic.colaus_f2.f2care1 IS 'Nb of consultations/visits last 12 months';
COMMENT ON COLUMN geochronic.colaus_f2.f2care1b IS 'Nb of emergency consultations last 12 months';
COMMENT ON COLUMN geochronic.colaus_f2.f2care2 IS 'Nb of hospitalizations last 12 months';
COMMENT ON COLUMN geochronic.colaus_f2.f2seden IS 'Sedentary: yes 10% or less etj in 4+BMR, no more than 10% 4+BMR';
COMMENT ON COLUMN geochronic.colaus_f2.f2bmi IS 'Body mass index';
COMMENT ON COLUMN geochronic.colaus_f2.f2bmi_cat2 IS 'BMI categories (+ underw)';
COMMENT ON COLUMN geochronic.colaus_f2.f2waist_cat IS 'Abdominal obesity (WHO)';
COMMENT ON COLUMN geochronic.colaus_f2.f2na IS 'Sodium (mmol/L)';
COMMENT ON COLUMN geochronic.colaus_f2.f2k IS 'Potassium (mmol/L)';
COMMENT ON COLUMN geochronic.colaus_f2.f2cr IS 'Creatinin (micromol/L)';
COMMENT ON COLUMN geochronic.colaus_f2.polypharm IS 'Polypharmacy (5 or more daily medications)';
COMMENT ON COLUMN geochronic.colaus_f2.f2gaf_l IS 'Global Assessment of Functioning - lifetime';
COMMENT ON COLUMN geochronic.colaus_f2.f2gaf_w IS 'Global Assessment of Functioning - worst';
COMMENT ON COLUMN geochronic.colaus_f2.f2gaf_c IS 'Global Assessment of Functioning - current';
COMMENT ON COLUMN geochronic.colaus_f2.f2sts_tot IS 'State-Trait Anxiety Inventory- state (current)';
COMMENT ON COLUMN geochronic.colaus_f2.f2stt_tot IS 'State-Trait Anxiety Inventory- trait (lifetime)';
COMMENT ON COLUMN geochronic.colaus_f2.f2cvd IS 'Cardiovascular disease since 2009/last part';
COMMENT ON COLUMN geochronic.colaus_f2.init_addr IS 'Address provided by participant';
COMMENT ON COLUMN geochronic.colaus_f2.matching_addr IS 'Address used for geocoding';
COMMENT ON COLUMN geochronic.colaus_f2.egid IS 'Building ID';
COMMENT ON COLUMN geochronic.colaus_f2.note_geocoding IS 'Geocoding accuracy';
COMMENT ON COLUMN geochronic.colaus_f2.gkode IS 'X-coord (EPSG:2056)';
COMMENT ON COLUMN geochronic.colaus_f2.gkodn IS 'Y-coord (EPSG:2056)';

EOF
