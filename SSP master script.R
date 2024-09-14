### This code runs the entire YRBS sex-of-sex-partners analysis.
# Goodreau SM, Barry MP, Lawley KA
# University of Washington

# Abbreviations:
# SoSP: sex(es) of sex partners

# SAS analyses, address in the corresponding ReadMe file must precede this program steps
rm(list=ls())
set.seed(1)

# Prepare project files
source("SoSP_00_packages.R") # call in packages; you may have to open this file and install any new packages 
#source("SOGI_03_prepare_YRBS_2021.R") 
#source("SOGI_04_prepare_YRBS_2015-2019.R") 
#source("SOGI_05_merge_YRBS.R") # run this file to prepare a single file `yrbs_final.rds`

# SSP Analyses
source("SOGI_31_SSP_prepare.R") # prepare objects for SoSP analyses
source("SOGI_32_SSP_analyses.R") # SoSP analyses 

