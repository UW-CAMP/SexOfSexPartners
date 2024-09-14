# YRBS SoSP Project

Full citation: Lawley, KA, Barry MP, Koenig LJ, Suarez NA, Williams AM, Delaney KP, Hoover KW, Hamilton DT, Goodreau SM. Trends in sex of sex partners and sexual identity among US adolescents, 2015-2021. [journal info TBD]. 


This manuscript uses data from the Youth Risk Behavior Surveillance System (YRBS) years 2015-2021. The following instructions explain how to replicate the data retrieval, cleaning, and analyses used in this manuscript.


To run the scripts in this project, you should create folders to store your data. Store the raw data files in a folder called "data - raw". You should also make a folder called "data - clean" to house the data that will be generated, and one called "plots" for the plots generated.

**Step 1. Obtain data** 

The YRBS National Data Set for 2019 and years prior can be found here: https://www.cdc.gov/healthyyouth/data/yrbs/sadc_2019/sadc_2019_national.dat

The YRBS National Data Set for 2021 can be found here: https://www.cdc.gov/healthyyouth/data/yrbs/files/2021/XXH2021_YRBS_Data.dat


Note: Both YRBS data sets are too large to store on GitHub; you must download them locally to reproduce the present analyses. To do this, use the corresponding Format and Input SAS programs, each modified from CDC for the present analysis. These are available in this repository as “YRBS SAS Formats” and “YRBS SAS Input” for the 2019 and prior years file and as "2021 YRBS SAS Formats" and 2021 YRBS SAS Input" for the 2021 data set.


SAS program instructions: To generate the YRBS 2021 dataset, you must first open the script named "2021 YRBS SAS Formats.sas" in the SAS program. The heading of the file contains specific instructions to properly save the data format file. After successfully running this file, you may then run the script named "2021 YRBS SAS Inputs.sas" Again, the heading contains specific instructions to properly execute the file. Next, to generate the 2015-2019 dataset, similarly run and follow corresponding instructions for the scripts "YRBS SAS Formats.sas" and "YRBS SAS Inputs.sas", respectively. After running these files, you will have produced the needed YRBS datasets for the analyses.

**Step 2. Prepare datasets and run analyses**

There are two different ways you can run the scripts to obtain the analyses.

**Option 1: Run all files**

***Single Step: Run the Master Script***
Run "SoSP master script.R" to generate the necessary data sets and all project files. This will generate a series of plots to accompany the analyses. Before calling in data, ensure that the directory location is correct on line 25 of the “SoSP_01_prepare” file. 


**Option 2: Run specified files**

***Step A: Prepare Data Sets***
In order, run the scripts “SoSP_00_packages” and “SoSP_01_prepare”. As previously in Option 1, before calling in data, ensure that the directory location is correct on line 25 of the “SoSP_01_prepare” file. After completing this, you have generated the clean data files to conduct the analyses of your choosing.


***Step B: Run Project Analyses***
After completing Step A once, you will save local copies of the data sets used by the remaining project files in the project. You may now run the remaining scripts based on which specific analyses are of interest. Analyses scripts are “SoSP_02_analyses”.


You have now completed the steps to replicate all analyses used in this manuscript. For any questions, please email Steven Goodreau at goodreau@uw.edu.


