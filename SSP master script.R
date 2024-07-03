### This code runs the entire YRBS/BRFSS sexual orientation, gender identity, and sex-of-sex-partners analysis (CAMP 60).
# Goodreau SM, Barry MP
# University of Washington

# Abbreviations:
# SO: sexual orientation
# GI: gender identity
# SSP: sex(es) of sex partners

# SAS analyses, address in the corresponding ReadMe file must precede this program steps
rm(list=ls())
set.seed(1)

# Prepare project files
source("SOGI_00_packages.R") # call in packages; you may have to open this file and install new packages 
#source("SOGI_03_prepare_YRBS_2021.R") 
#source("SOGI_04_prepare_YRBS_2015-2019.R") 
#source("SOGI_05_merge_YRBS.R") # run this file to prepare a single file `yrbs_final.rds`

# SSP Analyses
source("SOGI_SSP_prepare.R") # prepare objects for SSP analyses
source("SOGI_32_SSP_analyses.R") # SSP analyses 

