"""This code executes all the codes required to perform the syndemic analysis"""


import subprocess
import os


def main():

    # Build the spatial database
    subdir_db = "database"
    execute_code(subdir_db, "create_schema_db.sh", interpreter='sh')
    execute_code(subdir_db, "create_tables_db.sh", interpreter='sh')
    execute_code(subdir_db, "import_colaus_data.py")
    execute_code(subdir_db, "create_materialized_views.sh", interpreter='sh')
    execute_code(subdir_db, "comment_tables.sh", interpreter='sh')

    subdir_wrang = "wrangling"
    execute_code(subdir_wrang, "define_outcomes.R", interpreter='Rscript')

    subdir_wrang = "descriptive"
    execute_code(subdir_wrang, "describe_participants.R",
                 interpreter='Rscript')

    execute_code(subdir_db, "create_study_dataset.py")

    #execute_code(subdir_wrang, "assign_to_reli.py")


def execute_code(program_subdir, program_name, interpreter="python"):

    src_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src"
    program = os.sep.join([src_dir, program_subdir, program_name])
    subprocess.call([interpreter, program])
    print("Finished:" + program)


if __name__ == "__main__":
    main()