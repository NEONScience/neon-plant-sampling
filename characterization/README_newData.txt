neon-tos-sampling-design = CSP analysis 
https://bitbucket.org/lzachmann/neon-tos-sampling-design/src/master/

2021-02
Ran new analyses for YELL
- add new data to code/neondata/data-raw as a new file date
	- code to format portal data to match characterization data in kjFiles/task1b_yell.r
	- add growth form to mapping table, add all missing columns even if data are missing
- if analyses already exist for a given site/plotID, data from previous analyses need to be removed from data-raw folder
- run code/install_R_packagage.r 
- run code/neondata/data-raw/buildAllData.r
- re-run code/install_R_package.r
- run code/task_1b_vstdiv.r
- new output produced in results/task_1b_single_table_ranks_and_weights.csv (.xlsx doesn't write)
