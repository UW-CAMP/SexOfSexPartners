#### **Replication code for manuscript:**

##### Lawley KA, Barry MP, Koenig LJ, Suarez NA, Williams AM, Delaney KP, Hoover KW, Hamilton DT, Goodreau SM. "Trends in sex of sex partners and sexual identity among US adolescents, 2015-2021". *Journal of Adolescent Health*, in press. 

---

This manuscript uses data from the US Centers for Disease Control's *Youth Risk Behavior Surveillance System* (YRBS) from years 2015-2021. The following instructions explain how to replicate the data retrieval, cleaning, and analyses used in this manuscript.

To recreate this analysis, you will need access to SAS and RStudio. Original work done in the software and operating system environments indicated below.

- SAS version 9.4 in Windows: Edition	Windows Server 2022 Datacenter Version 21H2 Installed on	‎10/‎30/‎2022, OS build	20348.1487

- RStudio Version 2022.12.0+353 (2022.12.0+353), R Version 4.1.0 (2021-05-18) in Mac OS 13.1

---

**Step 1. Clone this GitHub repository to your local environment** 

**Step 2. Create folders in your local environment** 

To run the scripts in this project, you will need to create folders to store your data. Specifically, in the main repository folder (SexOfSexPartners), create three folders, called "data - raw", "data - clean", and "plots".

**Step 3. Obtain data** 

The project uses two YRBS data files, both files are too large to store on GitHub; thus  you must download them from YRBS to reproduce the present analyses. Save both of them in the folder "data - raw" that you created above: 

- [the 2019 Combined High School National Dataset](https://www.cdc.gov/yrbs/files/sadc_2019/sadc_2019_national.dat) (in this context combined means that it contains prior year data as well)


- [the 2021 High School National Dataset](https://www.cdc.gov/yrbs/files/2021/XXH2021_YRBS_Data.dat)

If the above links are no longer active, you will likely find the new links on the overall [data download page](https://www.cdc.gov/yrbs/data/index.html)

**Step 3. Convert data from SAS format to .csv** 

To do this, use the four SAS programs found in the "SAS files" directory. These are  each derived from files provided by CDC but modified for the present analysis. 

Note that each file contains specific instructions in the heading related to editing the file paths that you *must* follow to properly save the data format file.

Once you have made the necessary path edits, you should then open SAS and run the four files in this order:

- YRBS SAS Formats.sas
- YRBS SAS Input.sas 
- 2021 YRBS SAS Formats.sas
- 2021 YRBS SAS Input.sas

**Step 4. Prepare datasets and run analyses**

There are two different ways you can run the scripts to obtain the analyses:

***Option A: Run all files***

- *Single Step: Run the Master Script*. Run "SoSP master script.R" to generate the necessary data sets and all project files, conduct analyses, and generate figures.

***Option B: Run specified files***

- *Prepare Data Sets*. Run the scripts “SoSP_00_packages.R” and then “SoSP_01_prepare.R”. These two files together generate the final clean data files. You only need to run this step once, regardless of how many different analyses you chose to conduct in the nest step.

- *Run Project Analyses*. You may now run the all or parts of the remaining scripts based on which specific elements  are of interest. Statistical analyses are in the scripts “SoSP_02_analyses.R”, while "SoSP_03_tables_figures.R" generates the figures and the data for the tables used in the manuscript. 

---
  
*You have now completed the steps to replicate all analyses used in this manuscript. For any questions, please email Steven Goodreau at goodreau@uw.edu.*


