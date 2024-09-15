### This code runs the entire YRBS sex-of-sex-partners analysis.
# Goodreau SM, Barry MP, Lawley KA
# University of Washington

#Before running this code, please read the README file for instructions on downloading the data 


# Abbreviations:
# SoSP: sex(es) of sex partners

# SAS analyses, address in the corresponding ReadMe file must precede this program steps
rm(list=ls())
set.seed(1)

# Prepare project files
source("SoSP_00_packages.R") # call in packages; you may have to open this file and install any new packages 
source("SoSP_01_prepare.R") # prepare objects for SoSP analyses

# SoSP Analyses
source("SoSP_02_analyses.R") # SoSP analyses 
source("SoSP_03_tables_figures.R") # SoSP tables and figures 

