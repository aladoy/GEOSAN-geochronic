# GEOCHRONIC: Identifying Hot Spots of Cardiometabolic Risk Factors in a Swiss City

This repository contains the codebase developed for the project titled "Identifying hot spots of cardiometabolic risk factors in a Swiss city: impact of individual and environmental factors." by Anaïs Ladoy, Pedro Marques-Vidal, Idris Guessous, and Stéphane Joost.
The preprint version is acessible at https://doi.org/10.21203/rs.3.rs-3359714/v1.

## Confidentiality Statement

Due to the sensitive nature of the health data used in this research, only the source code is made available. The data and results are not provided.

## Directory Structure

```
├── database
│   ├── comment_tables.sh
│   ├── create_materialized_views.sh
│   ├── create_schema_db.sh
│   ├── create_study_dataset.py
│   ├── create_tables_db.sh
│   ├── import_colaus_data.py
│   └── sliver_killer.sql
├── descriptive
│   ├── describe_participants.R
│   └── map_indicators.R
├── modeling
│   ├── confounders_adjustment.R
│   ├── map_gwr_results.R
│   ├── model_env_asociations.py
│   ├── spatial_variation_risk_baseline.R
│   ├── spatial_variation_risk_F1.R
│   ├── spatial_variation_risk_F2.R
│   ├── utils_confounders_adjustment.R
│   ├── utils_map_gwr_results.R
│   └── utils_spatial_variation_risk.R
└── wrangling
    ├── assign_to_reli.py
    ├── build_ha_characteristics.py
    ├── build_residential_history.py
    ├── compute_env_exposures.py
    ├── define_indiv_covariates.R
    ├── define_outcomes.R
    ├── re_geocode_colaus.py
    └── utils_define_outcomes.R
```

## Script Workflow

The main script for executing the project workflow is `run_project.py`.

## Dependencies

**R Environment**

The R dependencies are managed using renv. The lockfile renv.lock in the repository should be used to install the same package versions as used in the study.
To restore the R environment, run the following commands in your R terminal:

```
install.packages("renv")
renv::restore()
```

**Python Environment**

The Python dependencies are managed through Conda. An exported Conda environment file (environment.yml) is included in the repository.
To create a new Conda environment with the specified packages, run the following command in your terminal:

```
conda env create -f environment.yml
```

## External Dependencies

GIRAPH-functions by aladoy

- **URL**: https://github.com/aladoy/GIRAPH-functions
- **Commit**: e9e75a0203db2daf6763d6706de75ceef273bea1
- **Description**: This external repository provides utility functions that are integrated into this project. Specifically, the following Python files from GIRAPH-functions were used:  
  `db_utils.py`: Database utility functions  
  `basic_utils.py`: General utility functions  
  `geocoding_utils.py`: Geocoding functions  
  `esda_utils.py`: Exploratory spatial data analysis functions
- **How to Incorporate**: Clone or download the GIRAPH-functions repository and ensure that it is accessible within your Python environment.

```
git clone https://github.com/aladoy/GIRAPH-functions.git
cd GIRAPH-functions
git checkout e9e75a0203db2daf6763d6706de75ceef273bea1
```
