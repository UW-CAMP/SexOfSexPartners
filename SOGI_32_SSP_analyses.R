### Script for SSP analysis

# This script's code uses the YRBS dataset, previously prepared in script "SOGI_31_SSP_prepare"
### to conduct SSP-related analyses

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
yrbs_final <- readRDS("data - clean/yrbs_final_new.rds")

#If packages listed below are not already installed
#install.packages("coin")
#install.packages("multiCA")
#install.packages("nnet")
#install.packages("mlogit")
#install.packages("lmtest")
#install.packages("car")

library(coin)
library(multiCA)
library(nnet)
library(mlogit)
library(lmtest)
library(car)

# Create directory

if(!dir.exists("plots/SSP")){
   if(!dir.exists("plots/")){
      dir.create("plots/")
   }
   dir.create("plots/SSP")
}



#########################################################
### SUBSETS FOR ANALYSES 
#########################################################

#Filter by year 
yrbs_2015 <- yrbs_final[yrbs_final$year == "2015",]
yrbs_2017 <- yrbs_final[yrbs_final$year == "2017",]
yrbs_2019 <- yrbs_final[yrbs_final$year == "2019",]
yrbs_2021 <- yrbs_final[yrbs_final$year == "2021",]

##Subset by sexual orientation
#Filter by year 
yrbs_straight <- yrbs_final[yrbs_final$so_new == "1_straight",]
yrbs_lesgay <- yrbs_final[yrbs_final$so_new == "2_lesgay",]
yrbs_bi <- yrbs_final[yrbs_final$so_new == "3_bi",]
yrbs_dko <- yrbs_final[yrbs_final$so_new == "4_dko",]
yrbs_ref <- yrbs_final[yrbs_final$so_new == "5_ref",]

yrbs_strfem <- yrbs_straight[yrbs_straight$sex == "Female", ]
yrbs_lesfem <- yrbs_lesgay[yrbs_lesgay$sex == "Female", ]
yrbs_bifem <- yrbs_bi[yrbs_bi$sex == "Female", ]
yrbs_dkofem <- yrbs_dko[yrbs_dko$sex == "Female", ]
yrbs_reffem <- yrbs_ref[yrbs_ref$sex == "Female", ]

yrbs_strmal <- yrbs_straight[yrbs_straight$sex == "Male", ]
yrbs_gaymal <- yrbs_lesgay[yrbs_lesgay$sex == "Male", ]
yrbs_bimal <- yrbs_bi[yrbs_bi$sex == "Male", ]
yrbs_dkomal <- yrbs_dko[yrbs_dko$sex == "Male", ]
yrbs_refmal <- yrbs_ref[yrbs_ref$sex == "Male", ]



#########################################################
### SUBSET YRBS_FINAL BY SEX 
#########################################################

# Create a subset for only female participants
yrbs_female <- yrbs_final[yrbs_final$sex == "Female", ]

# Create a subset for only male participants
yrbs_male <- yrbs_final[yrbs_final$sex == "Male", ]


yrbs_female15 <- yrbs_2015[yrbs_2015$sex == "Female", ]
yrbs_female17 <- yrbs_2017[yrbs_2017$sex == "Female", ]
yrbs_female19 <- yrbs_2019[yrbs_2019$sex == "Female", ]
yrbs_female21 <- yrbs_2021[yrbs_2021$sex == "Female", ]

yrbs_male15 <- yrbs_2015[yrbs_2015$sex == "Male", ]
yrbs_male17 <- yrbs_2017[yrbs_2017$sex == "Male", ]
yrbs_male19 <- yrbs_2019[yrbs_2019$sex == "Male", ]
yrbs_male21 <- yrbs_2021[yrbs_2021$sex == "Male", ]


#########################################################
### CREATE PROP TABLES TO EXAMINE DATA 
#########################################################

#Tables examining by age, SSP, sex, and sexual orientation
round(100* prop.table(table(yrbs_final$age, yrbs_final$sex_of_sps, yrbs_final$sex, yrbs_final$so_new), c(1,3, 4)),0)

table(yrbs_final$sex, yrbs_final$sex_of_sp)

chisq_test <- chisq.test(table(yrbs_final$sex, yrbs_final$sex_of_sp))
chisq_test

#Filter by year 

table(yrbs_2015$sex, yrbs_2015$sex_of_sp)
round(100* prop.table(table(yrbs_2015$sex, yrbs_2015$sex_of_sp)))
table(yrbs_2017$sex, yrbs_2017$sex_of_sp)
round(100* prop.table(table(yrbs_2017$sex, yrbs_2017$sex_of_sp)))
table(yrbs_2019$sex, yrbs_2019$sex_of_sp)
round(100* prop.table(table(yrbs_2019$sex, yrbs_2019$sex_of_sp)))
table(yrbs_2021$sex, yrbs_2021$sex_of_sp)
round(100* prop.table(table(yrbs_2021$sex, yrbs_2021$sex_of_sp)))


#Conditional on ever having had sex 
yrbs_sex <- yrbs_final[yrbs_final$sex_of_sps != "1_never",]
table(yrbs_sex$sex_of_sps)

table(yrbs_sex$sex, yrbs_sex$sex_of_sp)

yrbs_sex_2015 <- yrbs_sex[yrbs_sex$year == "2015",]
yrbs_sex_2017 <- yrbs_sex[yrbs_sex$year == "2017",]
yrbs_sex_2019 <- yrbs_sex[yrbs_sex$year == "2019",]
yrbs_sex_2021 <- yrbs_sex[yrbs_sex$year == "2021",]

##################################################################
### CODE TO CONDUCT MULTINOMIAL OUTCOME TREND TEST- SZABO (2016)
##################################################################

#frequency of each SSP by year, by sex (includes 1_never)

szabo_table_F <- table(yrbs_female$sex_of_sps, yrbs_female$year)
prop.table(szabo_table_F)

## using formula interface
multiCA_szabo_tbl_F <- multiCA.test(sex_of_sps ~ year, data=yrbs_female)


#frequency of each SSP by year, by sex (cond on EHHS; include 1_never)

df_proportions_fem <- yrbs_female %>%
  group_by(year) %>%
  summarise(
    none_prop = sum(sex_of_sps == "1_never") / n() * 100,
    females_prop = sum(sex_of_sps == "2_female") / n() * 100,
    males_prop = sum(sex_of_sps == "3_male") / n() * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_fem)



szabo_table_M <- table(yrbs_male$sex_of_sps, yrbs_male$year)
prop.table(szabo_table_M)

## using formula interface
multiCA_szabo_tbl_M <- multiCA.test(sex_of_sps ~ year, data=yrbs_male)


#frequency of each SSP by year, by sex (cond on EHHS; does include 1_never)


df_proportions_mal <- yrbs_male %>%
  group_by(year) %>%
  summarise(
    none_prop = sum(sex_of_sps == "1_never") / n() * 100,
    females_prop = sum(sex_of_sps == "2_female") / n() * 100,
    males_prop = sum(sex_of_sps == "3_male") / n() * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_mal)



#frequency of each SSP by year, by sex (does not includes 1_never)

szabo_table_F2 <- table(yrbs_female$ever_sex_sps, yrbs_female$year)
prop.table(szabo_table_F2)

## using formula interface
multiCA_szabo_tbl_F2 <-multiCA.test(ever_sex_sps ~ year, data=yrbs_female)
print(multiCA_szabo_tbl_F2)

#frequency of each SSP by year, by sex (does not includes 1_never)


df_proportions_fem2 <- yrbs_female %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_fem2)



szabo_table_M2 <- table(yrbs_male$ever_sex_sps, yrbs_male$year)
prop.table(szabo_table_M2)

## using formula interface
multiCA_szabo_tbl_M2 <- multiCA.test(ever_sex_sps ~ year, data=yrbs_male)


#frequency of each SSP by year, by sex (does not includes 1_never)


df_proportions_mal2 <- yrbs_male %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_mal2)



#################################
## Frequency of SSP by age, split by sex

#FEMALE
szabo_table_F3 <- table(yrbs_female$ever_sex_sps, yrbs_female$age)
prop.table(szabo_table_F3)

## using formula interface
multiCA_szabo_tbl_F3 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_female)

df_proportions_fem3 <- yrbs_female %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_fem3)


#MALE
szabo_table_M3 <- table(yrbs_male$ever_sex_sps, yrbs_male$age)
prop.table(szabo_table_M3)

## using formula interface
multiCA_szabo_tbl_M3 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_male)

df_proportions_mal3 <- yrbs_male %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_mal3)


#################################
## Frequency of SSP by age, split by year and by sex


#FEMALE 2015
szabo_table_F4 <- table(yrbs_female15$ever_sex_sps, yrbs_female15$age)
prop.table(szabo_table_F4)

## using formula interface
multiCA_szabo_tbl_F4 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_female15)

df_proportions_fem4 <- yrbs_female15 %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_fem4)


#FEMALE 2017
szabo_table_F5 <- table(yrbs_female17$ever_sex_sps, yrbs_female17$age)
prop.table(szabo_table_F5)

## using formula interface
multiCA_szabo_tbl_F5 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_female17)

df_proportions_fem5 <- yrbs_female17 %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_fem5)


#FEMALE 2019
szabo_table_F6 <- table(yrbs_female19$ever_sex_sps, yrbs_female19$age)
prop.table(szabo_table_F6)

## using formula interface
multiCA_szabo_tbl_F6 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_female19)

df_proportions_fem6 <- yrbs_female19 %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_fem6)


#FEMALE 2021
szabo_table_F7 <- table(yrbs_female21$ever_sex_sps, yrbs_female21$age)
prop.table(szabo_table_F7)

## using formula interface
multiCA_szabo_tbl_F7 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_female21)

df_proportions_fem7 <- yrbs_female21 %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_fem7)


#MALE 2015
szabo_table_M4 <- table(yrbs_male15$ever_sex_sps, yrbs_male15$age)
prop.table(szabo_table_M4)

## using formula interface
multiCA_szabo_tbl_M4 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_male15)

df_proportions_mal4 <- yrbs_male15 %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_mal4)


#MALE 2017
szabo_table_M5 <- table(yrbs_male17$ever_sex_sps, yrbs_male17$age)
prop.table(szabo_table_M5)

## using formula interface
multiCA_szabo_tbl_M5 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_male17)

df_proportions_mal5 <- yrbs_male17 %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_mal5)


#MALE 2019
szabo_table_M6 <- table(yrbs_male19$ever_sex_sps, yrbs_male19$age)
prop.table(szabo_table_M6)

## using formula interface
multiCA_szabo_tbl_M6 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_male19)
print(multiCA_szabo_tbl_M6)

df_proportions_mal6 <- yrbs_male19 %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_mal6)

#MALE 2021
szabo_table_M7 <- table(yrbs_male21$ever_sex_sps, yrbs_male21$age)
prop.table(szabo_table_M7)

## using formula interface
multiCA_szabo_tbl_M7 <- multiCA.test(ever_sex_sps ~ age, data=yrbs_male21)

df_proportions_mal7 <- yrbs_male21 %>%
  group_by(age) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_mal7)

##############################################
##FREQUENCY BY YEAR BY SEXUAL ORIENTATION

#straight female
szabo_table_strfem <- table(yrbs_strfem$ever_sex_sps, yrbs_strfem$year)
prop.table(szabo_table_strfem)

## using formula interface
multiCA_szabo_tbl_strfem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_strfem)

df_proportions_strfem <- yrbs_strfem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_strfem)


#lesbian female
szabo_table_lesfem <- table(yrbs_lesfem$ever_sex_sps, yrbs_lesfem$year)
prop.table(szabo_table_lesfem)

## using formula interface
multiCA_szabo_tbl_lesfem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_lesfem)

df_proportions_lesfem <- yrbs_lesfem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_lesfem)


#bisexual female
szabo_table_bifem <- table(yrbs_bifem$ever_sex_sps, yrbs_bifem$year)
prop.table(szabo_table_bifem)

## using formula interface
multiCA_szabo_tbl_bifem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_bifem)

df_proportions_bifem <- yrbs_bifem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_bifem)



#dont know female
szabo_table_dkofem <- table(yrbs_dkofem$ever_sex_sps, yrbs_dkofem$year)
prop.table(szabo_table_dkofem)

## using formula interface
multiCA_szabo_tbl_dkofem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_dkofem)

df_proportions_dkofem <- yrbs_dkofem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_dkofem)


#declined female
szabo_table_reffem <- table(yrbs_reffem$ever_sex_sps, yrbs_reffem$year)
prop.table(szabo_table_reffem)

## using formula interface
multiCA_szabo_tbl_reffem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_reffem)

df_proportions_reffem <- yrbs_reffem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_reffem)

#straight male
szabo_table_strmal <- table(yrbs_strmal$ever_sex_sps, yrbs_strmal$year)
prop.table(szabo_table_strmal)

## using formula interface
multiCA_szabo_tbl_strmal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_strmal)

df_proportions_strmal <- yrbs_strmal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_strmal)


#gay male
szabo_table_gaymal <- table(yrbs_gaymal$ever_sex_sps, yrbs_gaymal$year)
prop.table(szabo_table_gaymal)

## using formula interface
multiCA_szabo_tbl_gaymal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_gaymal)

df_proportions_gaymal <- yrbs_gaymal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_gaymal)



#bisexual male
szabo_table_bimal <- table(yrbs_bimal$ever_sex_sps, yrbs_bimal$year)
prop.table(szabo_table_bimal)

## using formula interface
multiCA_szabo_tbl_bimal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_bimal)

df_proportions_bimal <- yrbs_bimal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_bimal)



#don't know male
szabo_table_dkomal <- table(yrbs_dkomal$ever_sex_sps, yrbs_dkomal$year)
prop.table(szabo_table_dkomal)

## using formula interface
multiCA_szabo_tbl_dkomal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_dkomal)

df_proportions_dkomal <- yrbs_dkomal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_dkomal)


#declined to answer male
szabo_table_refmal <- table(yrbs_refmal$ever_sex_sps, yrbs_refmal$year)
prop.table(szabo_table_refmal)

## using formula interface
multiCA_szabo_tbl_refmal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_refmal)

df_proportions_refmal <- yrbs_refmal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_refmal)


################################################################################

#################################
## Frequency of SSP by year, split by age and by sex

# Create a subset for only female participants by age
yrbs_14yoF <- yrbs_female[yrbs_female$age == 14, ]
yrbs_15yoF <- yrbs_female[yrbs_female$age == 15, ]
yrbs_16yoF <- yrbs_female[yrbs_female$age == 16, ]
yrbs_17yoF <- yrbs_female[yrbs_female$age == 17, ]
yrbs_18yoF <- yrbs_female[yrbs_female$age == 18, ]

# Create a subset for only male participants by age
yrbs_14yoM <- yrbs_male[yrbs_male$age == 14, ]
yrbs_15yoM <- yrbs_male[yrbs_male$age == 15, ]
yrbs_16yoM <- yrbs_male[yrbs_male$age == 16, ]
yrbs_17yoM <- yrbs_male[yrbs_male$age == 17, ]
yrbs_18yoM <- yrbs_male[yrbs_male$age == 18, ]


#14 year old female
szabo_table_14yoF <- table(yrbs_14yoF$ever_sex_sps, yrbs_14yoF$year)
prop.table(szabo_table_14yoF)

## using formula interface
multiCA_szabo_tbl_14yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_14yoF)

df_proportions_14yoF <- yrbs_14yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_14yoF)


#15 year old female
szabo_table_15yoF <- table(yrbs_15yoF$ever_sex_sps, yrbs_15yoF$year)
prop.table(szabo_table_15yoF)

## using formula interface
multiCA_szabo_tbl_15yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_15yoF)

df_proportions_15yoF <- yrbs_15yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_15yoF)


#16 year old female
szabo_table_16yoF <- table(yrbs_16yoF$ever_sex_sps, yrbs_16yoF$year)
prop.table(szabo_table_16yoF)

## using formula interface
multiCA_szabo_tbl_16yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_16yoF)

df_proportions_16yoF <- yrbs_16yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_16yoF)


#17 year old female
szabo_table_17yoF <- table(yrbs_17yoF$ever_sex_sps, yrbs_17yoF$year)
prop.table(szabo_table_17yoF)

## using formula interface
multiCA_szabo_tbl_17yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_17yoF)

df_proportions_17yoF <- yrbs_17yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_17yoF)


#18 year old female
szabo_table_18yoF <- table(yrbs_18yoF$ever_sex_sps, yrbs_18yoF$year)
prop.table(szabo_table_18yoF)

## using formula interface
multiCA_szabo_tbl_18yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_18yoF)

df_proportions_18yoF <- yrbs_18yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_18yoF)


#14 year old male
szabo_table_14yoM <- table(yrbs_14yoM$ever_sex_sps, yrbs_14yoM$year)
prop.table(szabo_table_14yoM)

## using formula interface
multiCA_szabo_tbl_14yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_14yoM)

df_proportions_14yoM <- yrbs_14yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_14yoM)


#15 year old male
szabo_table_15yoM <- table(yrbs_15yoM$ever_sex_sps, yrbs_15yoM$year)
prop.table(szabo_table_15yoM)

## using formula interface
multiCA_szabo_tbl_15yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_15yoM)

df_proportions_15yoM <- yrbs_15yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_15yoM)



#16 year old male
szabo_table_16yoM <- table(yrbs_16yoM$ever_sex_sps, yrbs_16yoM$year)
prop.table(szabo_table_16yoM)

## using formula interface
multiCA_szabo_tbl_16yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_16yoM)

df_proportions_16yoM <- yrbs_16yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_16yoM)



#17 year old male
szabo_table_17yoM <- table(yrbs_17yoM$ever_sex_sps, yrbs_17yoM$year)
prop.table(szabo_table_17yoM)

## using formula interface
multiCA_szabo_tbl_17yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_17yoM)

df_proportions_17yoM <- yrbs_17yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_17yoM)


#18 year old male
szabo_table_18yoM <- table(yrbs_18yoM$ever_sex_sps, yrbs_18yoM$year)
prop.table(szabo_table_18yoM)

## using formula interface
multiCA_szabo_tbl_18yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_18yoM)

df_proportions_18yoM <- yrbs_18yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_18yoM)

#####################################
#####################################
##COMPARING METHODS: SZABO VS STANDARD MULTINOMIAL LOGISTIC REGRESSION

#frequency of each SSP by year, by sex (includes 1_never)
##Szabo method 

szabo_table_F <- table(yrbs_female$sex_of_sps, yrbs_female$year)
prop.table(szabo_table_F)

## using formula interface
multiCA_szabo_tbl_F <- multiCA.test(sex_of_sps ~ year, data=yrbs_female)

multiCA_szabo_tbl_F

#frequency of each SSP by year, by sex (cond on EHHS; include 1_never)

df_proportions_fem <- yrbs_female %>%
  group_by(year) %>%
  summarise(
    none_prop = sum(sex_of_sps == "1_never") / n() * 100,
    females_prop = sum(sex_of_sps == "2_female") / n() * 100,
    males_prop = sum(sex_of_sps == "3_male") / n() * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_fem)


#Standard multinomial logistic regression 

# Fit the multinomial logistic regression model
model <- multinom(sex_of_sps ~ year, data = yrbs_female)

# Summarize the model to see the coefficients and their standard errors
summary(model)

# Conduct the Wald test for the predictor
anova_results <- Anova(model, type = "II", test = "Wald")
print(anova_results)

p_value <- anova_results$"Pr(>Chisq)"[1]
if (!is.na(p_value)) {
  if (p_value < 0.05) {
    cat("There is a significant trend in the data (p-value =", p_value, ").\n")
  } else {
    cat("There is no significant trend in the data (p-value =", p_value, ").\n")
  }
} else {
  cat("The p-value is NA. Check the model and data.\n")
}

##############################################################################################
##################################################################
##DIFFERENT METHOD 

# Fit the multinomial logistic regression model
fem_model1 <- multinom(sex_of_sps ~ so_new + age, data = yrbs_female)

# Summarize the model to see the coefficients and their standard errors
summary(fem_model1)


# Fit the multinomial logistic regression model
fem_model2 <- multinom(sex_of_sps ~ so_new + depression, data = yrbs_female)

# Summarize the model to see the coefficients and their standard errors
summary(fem_model2)

# If p-values are not directly provided, you can compute them
z_values <- summary(fem_model2)$coefficients / summary(fem_model2)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Print z-values and p-values
print(z_values)
print(p_values)
##############################################################################################

## WEIGHTED CHI SQUARE TESTS 
###########

##Depression

##Lesbian females 
yrbs_les_sex <- yrbs_lesfem %>%
  filter(sex_of_sps != "1_never")

table(yrbs_les_sex$discord_1)
table(yrbs_les_sex$discord_2)

les_fem_tbl1 <- table(yrbs_les_sex$discord_1, yrbs_les_sex$depression)
print(les_fem_tbl1)

#discord_1
les_fem_chi1 <- wtd.chi.sq(yrbs_les_sex$discord_1, yrbs_les_sex$depression, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi1)

#discord_2
les_fem_chi2 <- wtd.chi.sq(yrbs_les_sex$discord_2, yrbs_les_sex$depression, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi2)




##gay males 
yrbs_gay_sex <- yrbs_gaymal %>%
  filter(sex_of_sps != "1_never")

table(yrbs_gay_sex$discord_1)
table(yrbs_gay_sex$discord_2)

gay_mal_tbl1 <- table(yrbs_gay_sex$discord_1, yrbs_gay_sex$depression)
print(gay_mal_tbl1)

#discord_1
gay_mal_chi1 <- wtd.chi.sq(yrbs_gay_sex$discord_1, yrbs_gay_sex$depression, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi1)

#discord_2
gay_mal_chi2 <- wtd.chi.sq(yrbs_gay_sex$discord_2, yrbs_gay_sex$depression, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi2)


##str males 
yrbs_strM_sex <- yrbs_strmal %>%
  filter(sex_of_sps != "1_never")

table(yrbs_strM_sex$discord_1)
table(yrbs_strM_sex$discord_2)

str_mal_tbl1 <- table(yrbs_strM_sex$discord_1, yrbs_strM_sex$depression)
print(str_mal_tbl1)

#discord_1
str_mal_chi1 <- wtd.chi.sq(yrbs_strM_sex$discord_1, yrbs_strM_sex$depression, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi1)

#discord_2
str_mal_chi2 <- wtd.chi.sq(yrbs_strM_sex$discord_2, yrbs_strM_sex$depression, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi2)




##str females 
yrbs_strF_sex <- yrbs_strfem %>%
  filter(sex_of_sps != "1_never")

table(yrbs_strF_sex$discord_1)
table(yrbs_strF_sex$discord_2)

str_fem_tbl1 <- table(yrbs_strF_sex$discord_1, yrbs_strF_sex$depression)
print(str_fem_tbl1)

#discord_1
str_fem_chi1 <- wtd.chi.sq(yrbs_strF_sex$discord_1, yrbs_strF_sex$depression, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi1)

#discord_2
str_fem_chi2 <- wtd.chi.sq(yrbs_strF_sex$discord_2, yrbs_strF_sex$depression, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi2)




##suic_idea

##Lesbian females 

les_fem_tbl3 <- table(yrbs_les_sex$discord_1, yrbs_les_sex$suic_idea)
print(les_fem_tbl3)

#discord_1
les_fem_chi3 <- wtd.chi.sq(yrbs_les_sex$discord_1, yrbs_les_sex$suic_idea, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi3)

#discord_2
les_fem_chi4 <- wtd.chi.sq(yrbs_les_sex$discord_2, yrbs_les_sex$suic_idea, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi4)



##gay males 

gay_mal_tbl2 <- table(yrbs_gay_sex$discord_1, yrbs_gay_sex$suic_idea)
print(gay_mal_tbl2)

#discord_1
gay_mal_chi3 <- wtd.chi.sq(yrbs_gay_sex$discord_1, yrbs_gay_sex$suic_idea, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi3)

#discord_2
gay_mal_chi4 <- wtd.chi.sq(yrbs_gay_sex$discord_2, yrbs_gay_sex$suic_idea, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi4)


##str males 

str_mal_tbl2 <- table(yrbs_strM_sex$discord_1, yrbs_strM_sex$suic_idea)
print(str_mal_tbl2)

#discord_1
str_mal_chi3 <- wtd.chi.sq(yrbs_strM_sex$discord_1, yrbs_strM_sex$suic_idea, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi3)

#discord_2
str_mal_chi4 <- wtd.chi.sq(yrbs_strM_sex$discord_2, yrbs_strM_sex$suic_idea, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi4)


##str females 

str_fem_tbl2 <- table(yrbs_strF_sex$discord_1, yrbs_strF_sex$suic_idea)
print(str_fem_tbl2)

#discord_1
str_fem_chi3 <- wtd.chi.sq(yrbs_strF_sex$discord_1, yrbs_strF_sex$suic_idea, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi3)

#discord_2
str_fem_chi4 <- wtd.chi.sq(yrbs_strF_sex$discord_2, yrbs_strF_sex$suic_idea, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi4)


##smoke_vape

##Lesbian females 

les_fem_tbl5 <- table(yrbs_les_sex$discord_1, yrbs_les_sex$smoke_vape)
print(les_fem_tbl5)

#discord_1
les_fem_chi5 <- wtd.chi.sq(yrbs_les_sex$discord_1, yrbs_les_sex$smoke_vape, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi5)

#discord_2
les_fem_chi6 <- wtd.chi.sq(yrbs_les_sex$discord_2, yrbs_les_sex$smoke_vape, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi6)




##gay males 

gay_mal_tbl5 <- table(yrbs_gay_sex$discord_1, yrbs_gay_sex$smoke_vape)
print(gay_mal_tbl5)

#discord_1
gay_mal_chi5 <- wtd.chi.sq(yrbs_gay_sex$discord_1, yrbs_gay_sex$smoke_vape, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi5)

#discord_2
gay_mal_chi6 <- wtd.chi.sq(yrbs_gay_sex$discord_2, yrbs_gay_sex$smoke_vape, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi6)


##str males 

str_mal_tbl5 <- table(yrbs_strM_sex$discord_1, yrbs_strM_sex$smoke_vape)
print(str_mal_tbl5)

#discord_1
str_mal_chi5 <- wtd.chi.sq(yrbs_strM_sex$discord_1, yrbs_strM_sex$smoke_vape, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi5)

#discord_2
str_mal_chi6 <- wtd.chi.sq(yrbs_strM_sex$discord_2, yrbs_strM_sex$smoke_vape, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi6)




##str females 

str_fem_tbl5 <- table(yrbs_strF_sex$discord_1, yrbs_strF_sex$smoke_vape)
print(str_fem_tbl5)

#discord_1
str_fem_chi5 <- wtd.chi.sq(yrbs_strF_sex$discord_1, yrbs_strF_sex$smoke_vape, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi5)

#discord_2
str_fem_chi6 <- wtd.chi.sq(yrbs_strF_sex$discord_2, yrbs_strF_sex$smoke_vape, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi6)


##ever_weed_D

##Lesbian females 

les_fem_tbl6 <- table(yrbs_les_sex$discord_1, yrbs_les_sex$ever_weed_D)
print(les_fem_tbl6)

#discord_1
les_fem_chi7 <- wtd.chi.sq(yrbs_les_sex$discord_1, yrbs_les_sex$ever_weed_D, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi7)

#discord_2
les_fem_chi8 <- wtd.chi.sq(yrbs_les_sex$discord_2, yrbs_les_sex$ever_weed_D, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi8)




##gay males 

gay_mal_tbl6 <- table(yrbs_gay_sex$discord_1, yrbs_gay_sex$ever_weed_D)
print(gay_mal_tbl6)

#discord_1
gay_mal_chi7 <- wtd.chi.sq(yrbs_gay_sex$discord_1, yrbs_gay_sex$ever_weed_D, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi7)

#discord_2
gay_mal_chi8 <- wtd.chi.sq(yrbs_gay_sex$discord_2, yrbs_gay_sex$ever_weed_D, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi8)


##str males 

str_mal_tbl6 <- table(yrbs_strM_sex$discord_1, yrbs_strM_sex$ever_weed_D)
print(str_mal_tbl6)

#discord_1
str_mal_chi7 <- wtd.chi.sq(yrbs_strM_sex$discord_1, yrbs_strM_sex$ever_weed_D, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi7)

#discord_2
str_mal_chi8 <- wtd.chi.sq(yrbs_strM_sex$discord_2, yrbs_strM_sex$ever_weed_D, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi8)




##str females 

str_fem_tbl6 <- table(yrbs_strF_sex$discord_1, yrbs_strF_sex$ever_weed_D)
print(str_fem_tbl6)

#discord_1
str_fem_chi7 <- wtd.chi.sq(yrbs_strF_sex$discord_1, yrbs_strF_sex$ever_weed_D, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi7)

#discord_2
str_fem_chi8 <- wtd.chi.sq(yrbs_strF_sex$discord_2, yrbs_strF_sex$ever_weed_D, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                           drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi8)



##ever_drink_D

##Lesbian females 

les_fem_tbl8 <- table(yrbs_les_sex$discord_1, yrbs_les_sex$ever_drink_D)
print(les_fem_tbl8)

#discord_1
les_fem_chi11 <- wtd.chi.sq(yrbs_les_sex$discord_1, yrbs_les_sex$ever_drink_D, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi11)

#discord_2
les_fem_chi12 <- wtd.chi.sq(yrbs_les_sex$discord_2, yrbs_les_sex$ever_drink_D, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi12)




##gay males 

gay_mal_tbl8 <- table(yrbs_gay_sex$discord_1, yrbs_gay_sex$ever_drink_D)
print(gay_mal_tbl8)

#discord_1
gay_mal_chi11 <- wtd.chi.sq(yrbs_gay_sex$discord_1, yrbs_gay_sex$ever_drink_D, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi11)

#discord_2
gay_mal_chi12 <- wtd.chi.sq(yrbs_gay_sex$discord_2, yrbs_gay_sex$ever_drink_D, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi12)


##str males 

str_mal_tbl8 <- table(yrbs_strM_sex$discord_1, yrbs_strM_sex$ever_drink_D)
print(str_mal_tbl8)

#discord_1
str_mal_chi11 <- wtd.chi.sq(yrbs_strM_sex$discord_1, yrbs_strM_sex$ever_drink_D, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi11)

#discord_2
str_mal_chi12 <- wtd.chi.sq(yrbs_strM_sex$discord_2, yrbs_strM_sex$ever_drink_D, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi12)




##str females 

str_fem_tbl8 <- table(yrbs_strF_sex$discord_1, yrbs_strF_sex$ever_drink_D)
print(str_fem_tbl8)

#discord_1
str_fem_chi11 <- wtd.chi.sq(yrbs_strF_sex$discord_1, yrbs_strF_sex$ever_drink_D, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi11)

#discord_2
str_fem_chi12 <- wtd.chi.sq(yrbs_strF_sex$discord_2, yrbs_strF_sex$ever_drink_D, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi12)


##suic_attempt_D

##Lesbian females 

les_fem_tbl9 <- table(yrbs_les_sex$discord_1, yrbs_les_sex$suic_attempt_D)
print(les_fem_tbl9)

#discord_1
les_fem_chi13 <- wtd.chi.sq(yrbs_les_sex$discord_1, yrbs_les_sex$suic_attempt_D, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi13)

#discord_2
les_fem_chi14 <- wtd.chi.sq(yrbs_les_sex$discord_2, yrbs_les_sex$suic_attempt_D, var3=NULL, weight= yrbs_les_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(les_fem_chi14)




##gay males 

gay_mal_tbl9 <- table(yrbs_gay_sex$discord_1, yrbs_gay_sex$suic_attempt_D)
print(gay_mal_tbl9)

#discord_1
gay_mal_chi13 <- wtd.chi.sq(yrbs_gay_sex$discord_1, yrbs_gay_sex$suic_attempt_D, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi13)

#discord_2
gay_mal_chi14 <- wtd.chi.sq(yrbs_gay_sex$discord_2, yrbs_gay_sex$suic_attempt_D, var3=NULL, weight= yrbs_gay_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(gay_mal_chi14)


##str males 

str_mal_tbl9 <- table(yrbs_strM_sex$discord_1, yrbs_strM_sex$suic_attempt_D)
print(str_mal_tbl9)

#discord_1
str_mal_chi13 <- wtd.chi.sq(yrbs_strM_sex$discord_1, yrbs_strM_sex$suic_attempt_D, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi13)

#discord_2
str_mal_chi14 <- wtd.chi.sq(yrbs_strM_sex$discord_2, yrbs_strM_sex$suic_attempt_D, var3=NULL, weight= yrbs_strM_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(str_mal_chi14)




##str females 

str_fem_tbl9 <- table(yrbs_strF_sex$discord_1, yrbs_strF_sex$suic_attempt_D)
print(str_fem_tbl9)

#discord_1
str_fem_chi13 <- wtd.chi.sq(yrbs_strF_sex$discord_1, yrbs_strF_sex$suic_attempt_D, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi13)

#discord_2
str_fem_chi14 <- wtd.chi.sq(yrbs_strF_sex$discord_2, yrbs_strF_sex$suic_attempt_D, var3=NULL, weight= yrbs_strF_sex$weight, na.rm=TRUE,
                            drop.missing.levels=TRUE, mean1=TRUE)
print(str_fem_chi14)


##############################################################################################
#######################################
########  PLOTS #######################
#######################################
##############################################################################################

# FIGURE 1: PLOTS BY SEX AND YEAR (CONDITIONED AND NOT CONDITIOND ON EHHS)
# (tables 1-4)
#+++++++++++++++++++++++++++++++++++++++++
png("plots/SSP/SSP_by_sex_and_year.png", 
    width = 4*300, height = 6*300, res = 300
    )
{
   par(mfrow=c(2,2))
   par(mar=c(2,0,3,0))
   par(oma=c(0,5,0,1))
   my_lwd = 1.5
   my_xlim = c(2014.5, 2021.5)
   
   plot(df_proportions_fem$year, df_proportions_fem$females_prop/100, type = "l", 
        col = "white", lwd = my_lwd, xlab = "", ylab = "", 
        xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
   axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
   mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
   axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
   mtext("All female respondents", 3, cex = 0.7, line =0.3)
   mtext("Proportion w/ sex partners of given sex", 2, cex = 0.7, line = 3, outer = TRUE)
   abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
   text(2014.6, 0.99, "A", cex=0.7)
   lines(df_proportions_fem$year, df_proportions_fem$females_prop/100, lty = 2, 
         col = "#31BAF6", lwd = my_lwd)
   lines(df_proportions_fem$year, df_proportions_fem$males_prop/100, lty = 3, 
         col = "#EEC441", lwd = my_lwd)
   lines(df_proportions_fem$year, df_proportions_fem$both_prop/100, lty = 4, 
         col = "#37C817", lwd = my_lwd)
   lines(df_proportions_fem$year, df_proportions_fem$none_prop/100, lty = 1, 
         col = "black", lwd = my_lwd)
   
   plot(df_proportions_mal$year, df_proportions_mal$females_prop/100, type = "l", 
        col = "white", lwd = my_lwd, xlab = "", ylab = "", 
        xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
   axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
   mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
   mtext("All male respondents", 3, cex = 0.7, line =0.3)
   abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
   text(2014.6, 0.99, "B", cex=0.7)
   lines(df_proportions_mal$year, df_proportions_mal$females_prop/100, lty = 2, 
         col = "#31BAF6", lwd = my_lwd)
      lines(df_proportions_mal$year, df_proportions_mal$males_prop/100, lty = 3, 
         col = "#EEC441", lwd = my_lwd)
   lines(df_proportions_mal$year, df_proportions_mal$both_prop/100, lty = 4, 
         col = "#37C817", lwd = my_lwd)
   lines(df_proportions_mal$year, df_proportions_mal$none_prop/100, lty = 1, 
         col = "black", lwd = my_lwd)
   
   legend(2016, 1, c("None", "Female partners only", 
                     "Male partners only", 
                     "Female and male partners"),
          col=c("#000000", "#31BAF6", "#EEC441", "#37C817"), 
          lty = 1:4, cex=0.5
   )
   
   plot(df_proportions_fem2$year, df_proportions_fem2$females_prop/100, type = "l", 
        col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
        xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
   axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
   mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
   axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
   mtext("Female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
   abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
   text(2014.6, 0.99, "C", cex=0.7)
   lines(df_proportions_fem2$year, df_proportions_fem2$females_prop/100, lty = 2, 
         col = "#31BAF6", lwd = my_lwd)
   lines(df_proportions_fem2$year, df_proportions_fem2$males_prop/100, lty = 3, 
         col = "#EEC441", lwd = my_lwd)
   lines(df_proportions_fem2$year, df_proportions_fem2$both_prop/100, lty = 4, 
         col = "#37C817", lwd = my_lwd)
   
   plot(df_proportions_mal2$year, df_proportions_mal2$females_prop/100, type = "l", 
        col = "white", lwd = my_lwd, xlab = "", ylab = "", 
        xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
   axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
   mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
   mtext("Male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
   abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
   text(2014.6, 0.99, "D", cex=0.7)
   lines(df_proportions_mal2$year, df_proportions_mal2$females_prop/100, lty = 2, 
         col = "#31BAF6", lwd = my_lwd)
   lines(df_proportions_mal2$year, df_proportions_mal2$males_prop/100, lty = 3, 
         col = "#EEC441", lwd = my_lwd)
   lines(df_proportions_mal2$year, df_proportions_mal2$both_prop/100, lty = 4, 
         col = "#37C817", lwd = my_lwd)
}
dev.off()


###############################################################################################
# FIGURE 2: PLOTS BY SO AND YEAR FOR FEMALES (CONDITIONED ON EHHS)
# tables 5-9
#Straight female, lesbian female, bisexual female, don't know female, declined to answer female
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SSP/SSP_by_SO_and_year_fem.png", 
    width = 5*400, height = 6*300, res = 300
)
{
  par(mfrow=c(2,3))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  
  plot(df_proportions_strfem$year, df_proportions_strfem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Straight female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  mtext("Proportion w/ sex partners of given sex", 2, cex = 0.7, line = 3, outer = TRUE)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=0.7)
  lines(df_proportions_strfem$year, df_proportions_strfem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_strfem$year, df_proportions_strfem$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_strfem$year, df_proportions_strfem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_lesfem$year, df_proportions_lesfem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("Lesbian female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=0.7)
  lines(df_proportions_lesfem$year, df_proportions_lesfem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_lesfem$year, df_proportions_lesfem$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_lesfem$year, df_proportions_lesfem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  
  plot(df_proportions_bifem$year, df_proportions_bifem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("Bisexual female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "C", cex=0.7)
  lines(df_proportions_bifem$year, df_proportions_bifem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_bifem$year, df_proportions_bifem$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_bifem$year, df_proportions_bifem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_dkofem$year, df_proportions_dkofem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Don't know female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "D", cex=0.7)
  lines(df_proportions_dkofem$year, df_proportions_dkofem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_dkofem$year, df_proportions_dkofem$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_dkofem$year, df_proportions_dkofem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_reffem$year, df_proportions_reffem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("Declined female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "E", cex=0.7)
  lines(df_proportions_reffem$year, df_proportions_reffem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_reffem$year, df_proportions_reffem$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_reffem$year, df_proportions_reffem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)

  # Create an empty plot for the legend
  plot.new()
  legend("center", c("Female partners only", 
                     "Male partners only", 
                     "Female and male partners"),
         col=c("#31BAF6", "#EEC441", "#37C817"), 
         lty = 1:4, cex=0.8, bty = "o"
  )
  
  }
dev.off()

###############################################################################################
# FIGURE 3: PLOTS BY SO AND YEAR FOR MALES (CONDITIONED ON EHHS)
# tables 10-14
#Straight male, gay male, bisexual male, don't know male, declined to answer male
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SSP/SSP_by_SO_and_year_mal.png", 
    width = 5*400, height = 6*300, res = 300
)
{
  par(mfrow=c(2,3))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  
  
  plot(df_proportions_strmal$year, df_proportions_strmal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Straight male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=0.7)
  lines(df_proportions_strmal$year, df_proportions_strmal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_strmal$year, df_proportions_strmal$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_strmal$year, df_proportions_strmal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_gaymal$year, df_proportions_gaymal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("Gay male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=0.7)
  lines(df_proportions_gaymal$year, df_proportions_gaymal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_gaymal$year, df_proportions_gaymal$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_gaymal$year, df_proportions_gaymal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  
  plot(df_proportions_bimal$year, df_proportions_bimal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("Bisexual male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "C", cex=0.7)
  lines(df_proportions_bimal$year, df_proportions_bimal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_bimal$year, df_proportions_bimal$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_bimal$year, df_proportions_bimal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_dkomal$year, df_proportions_dkomal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Don't know male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "D", cex=0.7)
  lines(df_proportions_dkomal$year, df_proportions_dkomal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_dkomal$year, df_proportions_dkomal$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_dkomal$year, df_proportions_dkomal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_refmal$year, df_proportions_refmal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("Declined male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "E", cex=0.7)
  lines(df_proportions_refmal$year, df_proportions_refmal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_refmal$year, df_proportions_refmal$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_refmal$year, df_proportions_refmal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
 
  # Create an empty plot for the legend
  plot.new()
  legend("center", c("Female partners only", 
                     "Male partners only", 
                     "Female and male partners"),
         col=c("#31BAF6", "#EEC441", "#37C817"), 
         lty = 1:4, cex=0.8, bty = "o"
  )
  
}

dev.off()

#######################################################################################

# FIGURE 4: PLOTS BY AGE AND YEAR FOR FEMALES (CONDITIONED ON EHHS)
# tables 15-19
#Females Ages 14, 15, 16, 17, and 18 
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SSP/SSP_by_age_and_year_fem.png", 
    width = 5*400, height = 6*300, res = 300
)
{
  par(mfrow=c(2,3))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  
  
  plot(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("14yo female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=0.7)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  
  plot(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("15yo female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=0.7)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  
  plot(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("16yo female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "C", cex=0.7)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("17yo female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "D", cex=0.7)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("18yo female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "E", cex=0.7)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
 
  # Create an empty plot for the legend
  plot.new()
  legend("center", c("Female partners only", 
                     "Male partners only", 
                     "Female and male partners"),
         col=c("#31BAF6", "#EEC441", "#37C817"), 
         lty = 1:4, cex=0.8, bty = "o"
  ) 

  }

dev.off()

#######################################################################################

# FIGURE 5: PLOTS BY AGE AND YEAR FOR MALES (CONDITIONED ON EHHS)
# tables 20-24
#males Ages 14, 15, 16, 17, and 18 
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SSP/SSP_by_age_and_year_mal.png", 
    width = 5*400, height = 6*300, res = 300
)
{
  par(mfrow=c(2,3))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  
  
  plot(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("14yo male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=0.7)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("15yo male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=0.7)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  
  plot(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("16yo male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "C", cex=0.7)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("17yo male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "D", cex=0.7)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  mtext("18yo male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "E", cex=0.7)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$males_prop/100, lty = 3, 
        col = "#EEC441", lwd = my_lwd)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  # Create an empty plot for the legend
  plot.new()
  legend("center", c("Female partners only", 
                     "Male partners only", 
                     "Female and male partners"),
         col=c("#31BAF6", "#EEC441", "#37C817"), 
         lty = 1:4, cex=0.8, bty = "o"
  )
  
  
  }



dev.off()

#######################################################################################

