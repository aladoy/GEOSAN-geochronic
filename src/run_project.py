"""This code executes all the codes required to perform the syndemic analysis"""


import subprocess
import os

# Commit for libraries : bf78b2d


def main():

    # Build the spatial database
    subdir_db = "database"
    subdir_wrang = "wrangling"
    subdir_descr = "descriptive"

    # # Import dataset to DB
    # execute_code(subdir_wrang, "re_geocode_colaus.py")
    # execute_code(subdir_db, "create_schema_db.sh", interpreter='sh')
    # execute_code(subdir_db, "create_tables_db.sh", interpreter='sh')
    # execute_code(subdir_db, "import_colaus_data.py")
    # execute_code(subdir_db, "create_materialized_views.sh", interpreter='sh')
    # execute_code(subdir_db, "comment_tables.sh", interpreter='sh')

    # # Add residential information
    # execute_code(subdir_wrang, "assign_to_reli.py")
    # execute_code(subdir_wrang, "build_residential_history.py")

    # Define outcomes and covariates
    execute_code(subdir_wrang, "define_outcomes.R", interpreter='Rscript')
    execute_code(subdir_wrang, "define_indiv_covariates.R",
                 interpreter='Rscript')
    execute_code(subdir_db, "create_study_dataset.py")

    # ESDA
    execute_code(subdir_descr, "describe_participants.R",
                 interpreter='Rscript')


def execute_code(program_subdir, program_name, interpreter="python"):

    src_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src"
    program = os.sep.join([src_dir, program_subdir, program_name])
    subprocess.call([interpreter, program])
    print("Finished:" + program)


if __name__ == "__main__":
    main()
