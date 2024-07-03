### Prepare for YRBS SSP Analyses

# This script's code uses the YRBS dataset, previously prepared in script "SOGI_03_Prepare Combined Dataset"
### and prepares objects for the sex-of-sex-partner analyses in "SOGI_32_SSP_analyses"

### Prepare workspace ----- 
# clear environment
rm(list = ls())

# packages
source("SOGI_00_packages.R")

# define functions
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}

# call in data
yrbs_merge <- readRDS("data - clean/yrbs_final.rds")


#########################################################
### CREATE NEW VARIABLES FOR COCHRAN ARMITAGE TESTS 
#########################################################

##Variable conditional on EHHS

# Create ssp_cond using ifelse statement
yrbs_merge$ssp_cond <- ifelse(yrbs_merge$sex_of_sps == "1_never", NA, yrbs_merge$sex_of_sps)

write_rds(yrbs_merge, file = "data - clean/yrbs_clean.rds")

#########################################################
### CREATE NEW VARIABLES FOR COCHRAN ARMITAGE TESTS 
#########################################################

##THESE VARIABLES ARE PROBABLY NOT NEEDED ANY MORE. CONFIRM BEFORE DELETING (KL - 5/8/24)
## Variable that only includes sex_of_sps = 2_female or 1_never,
## Everything else set to NA
#yrbs_merge <- yrbs_merge %>%
#  mutate(ssp_female = ifelse(sex_of_sps %in% c("2_female", "1_never"), sex_of_sps, NA))


## Variable that only includes sex_of_sps = 3_male or 1_never,
## Everything else set to NA
#yrbs_merge <- yrbs_merge %>%
#  mutate(ssp_male = ifelse(sex_of_sps %in% c("3_male", "1_never"), sex_of_sps, NA))

## Variable that only includes sex_of_sps = 4_fem+mal or 1_never,
## Everything else set to NA
#yrbs_merge <- yrbs_merge %>%
#  mutate(ssp_both = ifelse(sex_of_sps %in% c("4_fem+mal", "1_never"), sex_of_sps, NA))



