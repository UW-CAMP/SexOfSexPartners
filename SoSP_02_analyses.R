### Script for SoSP analysis

### This script's code uses the YRBS dataset, previously prepared in script "SoSP_01_prepare"
### to conduct SSP-related analyses

### Prepare workspace ----- 

# packages
source("SoSP_00_packages.R")

# define functions
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}

# call in data
yrbs_final <- readRDS("data - clean/yrbs_final.rds")


# Create directory

if(!dir.exists("plots/SoSP")){
   if(!dir.exists("plots/")){
      dir.create("plots/")
   }
   dir.create("plots/SoSP")
}



#########################################################
### SUBSETS FOR ANALYSES 
#########################################################

#Subset by year 
yrbs_2015 <- yrbs_final[yrbs_final$year == "2015",]
yrbs_2017 <- yrbs_final[yrbs_final$year == "2017",]
yrbs_2019 <- yrbs_final[yrbs_final$year == "2019",]
yrbs_2021 <- yrbs_final[yrbs_final$year == "2021",]

##Subset by sexual identity
yrbs_straight <- yrbs_final[yrbs_final$so_new == "1_straight",]
yrbs_lesgay <- yrbs_final[yrbs_final$so_new == "2_lesgay",]
yrbs_bi <- yrbs_final[yrbs_final$so_new == "3_bi",]
yrbs_dko <- yrbs_final[yrbs_final$so_new == "4_dko",]
yrbs_ref <- yrbs_final[yrbs_final$so_new == "5_ref",]

##Subset by sexual identity and sex
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

#Subset by sex only 
# Create a subset for only female participants
yrbs_female <- yrbs_final[yrbs_final$sex == "Female", ]

# Create a subset for only male participants
yrbs_male <- yrbs_final[yrbs_final$sex == "Male", ]

#Subset by sex and year 
yrbs_female15 <- yrbs_2015[yrbs_2015$sex == "Female", ]
yrbs_female17 <- yrbs_2017[yrbs_2017$sex == "Female", ]
yrbs_female19 <- yrbs_2019[yrbs_2019$sex == "Female", ]
yrbs_female21 <- yrbs_2021[yrbs_2021$sex == "Female", ]

yrbs_male15 <- yrbs_2015[yrbs_2015$sex == "Male", ]
yrbs_male17 <- yrbs_2017[yrbs_2017$sex == "Male", ]
yrbs_male19 <- yrbs_2019[yrbs_2019$sex == "Male", ]
yrbs_male21 <- yrbs_2021[yrbs_2021$sex == "Male", ]

#Subset conditional on ever having had sex 
yrbs_sex <- yrbs_final[yrbs_final$sex_of_sps != "1_never",]
table(yrbs_sex$sex_of_sps)

table(yrbs_sex$sex, yrbs_sex$sex_of_sp)

#Subset those who have had sex by year
yrbs_sex_2015 <- yrbs_sex[yrbs_sex$year == "2015",]
yrbs_sex_2017 <- yrbs_sex[yrbs_sex$year == "2017",]
yrbs_sex_2019 <- yrbs_sex[yrbs_sex$year == "2019",]
yrbs_sex_2021 <- yrbs_sex[yrbs_sex$year == "2021",]


#########################################################
### CREATE PROP TABLES TO EXAMINE DATA 
#########################################################

#Tables examining by age, SSP, sex, and sexual identity
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


##################################################################
### CODE TO CONDUCT MULTINOMIAL OUTCOME TREND TEST- SZABO (2016)
##################################################################

#Assessing SoSP trends by sex, including those with no prior sexual experience
###############################################################################

#Females
szabo_table_F <- table(yrbs_female$sex_of_sps, yrbs_female$year)
prop.table(szabo_table_F)

#frequency of each SSP by year, by sex (include 1_never)
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

## using formula interface
multiCA_szabo_tbl_F <- multiCA.test(sex_of_sps ~ year, data=yrbs_female)
print(multiCA_szabo_tbl_F)


#Males
szabo_table_M <- table(yrbs_male$sex_of_sps, yrbs_male$year)
prop.table(szabo_table_M)

#frequency of each SSP by year, by sex (include 1_never)
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

## using formula interface
multiCA_szabo_tbl_M <- multiCA.test(sex_of_sps ~ year, data=yrbs_male)
print(multiCA_szabo_tbl_M)
###################################

#Assessing SoSP trends by sex, excluding those with no prior sexual experience
###############################################################################

#Females
szabo_table_F2 <- table(yrbs_female$ever_sex_sps, yrbs_female$year)
prop.table(szabo_table_F2)

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

## using formula interface
multiCA_szabo_tbl_F2 <-multiCA.test(ever_sex_sps ~ year, data=yrbs_female)
print(multiCA_szabo_tbl_F2)



#Males
szabo_table_M2 <- table(yrbs_male$ever_sex_sps, yrbs_male$year)
prop.table(szabo_table_M2)

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

## using formula interface
multiCA_szabo_tbl_M2 <- multiCA.test(ever_sex_sps ~ year, data=yrbs_male)
print(multiCA_szabo_tbl_M2)
###################################

#Assessing frequency of sexual identity across years by sex, without 
#considering SoSP
#####################################################################

df_proportions_fem_so <- yrbs_female %>%
  group_by(year) %>%
  summarise(
    straight_prop = sum(so_new == "1_straight") / n() * 100,
    lesgay_prop = sum(so_new == "2_lesgay") / n() * 100,
    bi_prop = sum(so_new == "3_bi") / n() * 100,
    dko_prop = sum(so_new == "4_dko") / n() * 100,
    ref_prop = sum(so_new == "5_ref") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_fem_so)

#p values
multiCA_so_fem_year <- multiCA.test(so_new ~ year, data=yrbs_female)
print(multiCA_so_fem_year)


df_proportions_mal_so <- yrbs_male %>%
  group_by(year) %>%
  summarise(
    straight_prop = sum(so_new == "1_straight") / n() * 100,
    lesgay_prop = sum(so_new == "2_lesgay") / n() * 100,
    bi_prop = sum(so_new == "3_bi") / n() * 100,
    dko_prop = sum(so_new == "4_dko") / n() * 100,
    ref_prop = sum(so_new == "5_ref") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_mal_so)

#p values
multiCA_so_mal_year <- multiCA.test(so_new ~ year, data=yrbs_male)
print(multiCA_so_mal_year)
###################################

#Assessing SoSP trends by sexual identity and year, 
#including those with no prior sexual experience
####################################################


#straight female
szabo_table_strfem_unc <- table(yrbs_strfem$sex_of_sps, yrbs_strfem$year)
prop.table(szabo_table_strfem_unc)

df_proportions_strfem_unc <- yrbs_strfem %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_strfem_unc)

## using formula interface
multiCA_szabo_tbl_strfem_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_strfem)
print(multiCA_szabo_tbl_strfem_unc)


#lesbian female
szabo_table_lesfem_unc <- table(yrbs_lesfem$sex_of_sps, yrbs_lesfem$year)
prop.table(szabo_table_lesfem_unc)

df_proportions_lesfem_unc <- yrbs_lesfem %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_lesfem_unc)

## using formula interface
multiCA_szabo_tbl_lesfem_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_lesfem)
print(multiCA_szabo_tbl_lesfem_unc)



#bisexual female
szabo_table_bifem_unc <- table(yrbs_bifem$sex_of_sps, yrbs_bifem$year)
prop.table(szabo_table_bifem_unc)

df_proportions_bifem_unc <- yrbs_bifem %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_bifem_unc)

## using formula interface
multiCA_szabo_tbl_bifem_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_bifem)
print(multiCA_szabo_tbl_bifem_unc)


#don't know female
szabo_table_dkofem_unc <- table(yrbs_dkofem$sex_of_sps, yrbs_dkofem$year)
prop.table(szabo_table_dkofem_unc)


df_proportions_dkofem_unc <- yrbs_dkofem %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_dkofem_unc)

## using formula interface
multiCA_szabo_tbl_dkofem_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_dkofem)
print(multiCA_szabo_tbl_dkofem_unc)


#refused female
szabo_table_reffem_unc <- table(yrbs_reffem$sex_of_sps, yrbs_reffem$year)
prop.table(szabo_table_reffem_unc)

df_proportions_reffem_unc <- yrbs_reffem %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_reffem_unc)

## using formula interface
multiCA_szabo_tbl_reffem_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_reffem)
print(multiCA_szabo_tbl_reffem_unc)


#straight male
szabo_table_strmal_unc <- table(yrbs_strmal$sex_of_sps, yrbs_strmal$year)
prop.table(szabo_table_strmal_unc)

df_proportions_strmal_unc <- yrbs_strmal %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_strmal_unc)

## using formula interface
multiCA_szabo_tbl_strmal_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_strmal)
print(multiCA_szabo_tbl_strmal_unc)


#gay male
szabo_table_gaymal_unc <- table(yrbs_gaymal$sex_of_sps, yrbs_gaymal$year)
prop.table(szabo_table_gaymal_unc)

df_proportions_gaymal_unc <- yrbs_gaymal %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_gaymal_unc)

## using formula interface
multiCA_szabo_tbl_gaymal_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_gaymal)
print(multiCA_szabo_tbl_gaymal_unc)



#bisexual male
szabo_table_bimal_unc <- table(yrbs_bimal$sex_of_sps, yrbs_bimal$year)
prop.table(szabo_table_bimal_unc)

df_proportions_bimal_unc <- yrbs_bimal %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_bimal_unc)

## using formula interface
multiCA_szabo_tbl_bimal_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_bimal)
print(multiCA_szabo_tbl_bimal_unc)


#don't know male
szabo_table_dkomal_unc <- table(yrbs_dkomal$sex_of_sps, yrbs_dkomal$year)
prop.table(szabo_table_dkomal_unc)

df_proportions_dkomal_unc <- yrbs_dkomal %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_dkomal_unc)

## using formula interface
multiCA_szabo_tbl_dkomal_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_dkomal)
print(multiCA_szabo_tbl_dkomal_unc)


#refused male
szabo_table_refmal_unc <- table(yrbs_refmal$sex_of_sps, yrbs_refmal$year)
prop.table(szabo_table_refmal_unc)

df_proportions_refmal_unc <- yrbs_refmal %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_refmal_unc)

## using formula interface
multiCA_szabo_tbl_refmal_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_refmal)
print(multiCA_szabo_tbl_refmal_unc)


###################################

#Assessing SoSP trends by sexual identity and year, excluding those with no 
#prior sexual experience
############################################################################

#straight female
szabo_table_strfem <- table(yrbs_strfem$ever_sex_sps, yrbs_strfem$year)
prop.table(szabo_table_strfem)

df_proportions_strfem <- yrbs_strfem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_strfem)

## using formula interface
multiCA_szabo_tbl_strfem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_strfem)
print(multiCA_szabo_tbl_strfem)



#lesbian female
szabo_table_lesfem <- table(yrbs_lesfem$ever_sex_sps, yrbs_lesfem$year)
prop.table(szabo_table_lesfem)

df_proportions_lesfem <- yrbs_lesfem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_lesfem)

## using formula interface
multiCA_szabo_tbl_lesfem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_lesfem)
print(multiCA_szabo_tbl_lesfem)


#bisexual female
szabo_table_bifem <- table(yrbs_bifem$ever_sex_sps, yrbs_bifem$year)
prop.table(szabo_table_bifem)

df_proportions_bifem <- yrbs_bifem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_bifem)

## using formula interface
multiCA_szabo_tbl_bifem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_bifem)
print(multiCA_szabo_tbl_bifem)


#dont know female
szabo_table_dkofem <- table(yrbs_dkofem$ever_sex_sps, yrbs_dkofem$year)
prop.table(szabo_table_dkofem)

df_proportions_dkofem <- yrbs_dkofem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_dkofem)

## using formula interface
multiCA_szabo_tbl_dkofem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_dkofem)
print(multiCA_szabo_tbl_dkofem)


#declined female
szabo_table_reffem <- table(yrbs_reffem$ever_sex_sps, yrbs_reffem$year)
prop.table(szabo_table_reffem)

df_proportions_reffem <- yrbs_reffem %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_reffem)

## using formula interface
multiCA_szabo_tbl_reffem <- multiCA.test(ever_sex_sps ~ year, data=yrbs_reffem)
print(multiCA_szabo_tbl_reffem)


#straight male
szabo_table_strmal <- table(yrbs_strmal$ever_sex_sps, yrbs_strmal$year)
prop.table(szabo_table_strmal)

df_proportions_strmal <- yrbs_strmal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_strmal)

## using formula interface
multiCA_szabo_tbl_strmal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_strmal)
print(multiCA_szabo_tbl_strmal)



#gay male
szabo_table_gaymal <- table(yrbs_gaymal$ever_sex_sps, yrbs_gaymal$year)
prop.table(szabo_table_gaymal)

df_proportions_gaymal <- yrbs_gaymal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_gaymal)

## using formula interface
multiCA_szabo_tbl_gaymal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_gaymal)
print(multiCA_szabo_tbl_gaymal)



#bisexual male
szabo_table_bimal <- table(yrbs_bimal$ever_sex_sps, yrbs_bimal$year)
prop.table(szabo_table_bimal)

df_proportions_bimal <- yrbs_bimal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_bimal)

## using formula interface
multiCA_szabo_tbl_bimal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_bimal)
print(multiCA_szabo_tbl_bimal)



#don't know male
szabo_table_dkomal <- table(yrbs_dkomal$ever_sex_sps, yrbs_dkomal$year)
prop.table(szabo_table_dkomal)

df_proportions_dkomal <- yrbs_dkomal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_dkomal)

## using formula interface
multiCA_szabo_tbl_dkomal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_dkomal)
print(multiCA_szabo_tbl_dkomal)


#declined to answer male
szabo_table_refmal <- table(yrbs_refmal$ever_sex_sps, yrbs_refmal$year)
prop.table(szabo_table_refmal)

df_proportions_refmal <- yrbs_refmal %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_refmal)

## using formula interface
multiCA_szabo_tbl_refmal <- multiCA.test(ever_sex_sps ~ year, data=yrbs_refmal)
print(multiCA_szabo_tbl_refmal)

##############

#Assessing SoSP trends by age and year, 
#including those with no prior sexual experience.
####################################################

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

##assessing trends in sexual identity by age including everyone who has never had sex 
#################################################################################

#14yo female
szabo_table_14yoF_unc <- table(yrbs_14yoF$sex_of_sps, yrbs_14yoF$year)
prop.table(szabo_table_14yoF_unc)


df_proportions_14yoF_unc <- yrbs_14yoF %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_14yoF_unc)

## using formula interface
multiCA_szabo_tbl_14yoF_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_14yoF)
print(multiCA_szabo_tbl_14yoF_unc)


#15yo female
szabo_table_15yoF_unc <- table(yrbs_15yoF$sex_of_sps, yrbs_15yoF$year)
prop.table(szabo_table_15yoF_unc)


df_proportions_15yoF_unc <- yrbs_15yoF %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_15yoF_unc)

## using formula interface
multiCA_szabo_tbl_15yoF_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_15yoF)
print(multiCA_szabo_tbl_15yoF_unc)



#16yo female
szabo_table_16yoF_unc <- table(yrbs_16yoF$sex_of_sps, yrbs_16yoF$year)
prop.table(szabo_table_16yoF_unc)

df_proportions_16yoF_unc <- yrbs_16yoF %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_16yoF_unc)

## using formula interface
multiCA_szabo_tbl_16yoF_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_16yoF)
print(multiCA_szabo_tbl_16yoF_unc)


#17yo female
szabo_table_17yoF_unc <- table(yrbs_17yoF$sex_of_sps, yrbs_17yoF$year)
prop.table(szabo_table_17yoF_unc)


df_proportions_17yoF_unc <- yrbs_17yoF %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_17yoF_unc)

## using formula interface
multiCA_szabo_tbl_17yoF_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_17yoF)
print(multiCA_szabo_tbl_17yoF_unc)



#18yo female
szabo_table_18yoF_unc <- table(yrbs_18yoF$sex_of_sps, yrbs_18yoF$year)
prop.table(szabo_table_18yoF_unc)

df_proportions_18yoF_unc <- yrbs_18yoF %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_18yoF_unc)

## using formula interface
multiCA_szabo_tbl_18yoF_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_18yoF)
print(multiCA_szabo_tbl_18yoF_unc)



#14yo male
szabo_table_14yoM_unc <- table(yrbs_14yoM$sex_of_sps, yrbs_14yoM$year)
prop.table(szabo_table_14yoM_unc)

df_proportions_14yoM_unc <- yrbs_14yoM %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_14yoM_unc)

## using formula interface
multiCA_szabo_tbl_14yoM_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_14yoM)
print(multiCA_szabo_tbl_14yoM_unc)



#15yo male
szabo_table_15yoM_unc <- table(yrbs_15yoM$sex_of_sps, yrbs_15yoM$year)
prop.table(szabo_table_15yoM_unc)

df_proportions_15yoM_unc <- yrbs_15yoM %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_15yoM_unc)

## using formula interface
multiCA_szabo_tbl_15yoM_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_15yoM)
print(multiCA_szabo_tbl_15yoM_unc)


#16yo male
szabo_table_16yoM_unc <- table(yrbs_16yoM$sex_of_sps, yrbs_16yoM$year)
prop.table(szabo_table_16yoM_unc)

df_proportions_16yoM_unc <- yrbs_16yoM %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_16yoM_unc)

## using formula interface
multiCA_szabo_tbl_16yoM_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_16yoM)
print(multiCA_szabo_tbl_16yoM_unc)


#17yo male
szabo_table_17yoM_unc <- table(yrbs_17yoM$sex_of_sps, yrbs_17yoM$year)
prop.table(szabo_table_17yoM_unc)

df_proportions_17yoM_unc <- yrbs_17yoM %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_17yoM_unc)

## using formula interface
multiCA_szabo_tbl_17yoM_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_17yoM)
print(multiCA_szabo_tbl_17yoM_unc)


#18yo male
szabo_table_18yoM_unc <- table(yrbs_18yoM$sex_of_sps, yrbs_18yoM$year)
prop.table(szabo_table_18yoM_unc)

df_proportions_18yoM_unc <- yrbs_18yoM %>%
  group_by(year) %>%
  summarise(
    never_prop = sum(sex_of_sps == "1_never", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    females_prop = sum(sex_of_sps == "2_female", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    males_prop = sum(sex_of_sps == "3_male", na.rm = T) / sum(!is.na(sex_of_sps)) * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal", na.rm = T) / sum(!is.na(sex_of_sps)) * 100
  )

print(df_proportions_18yoM_unc)

## using formula interface
multiCA_szabo_tbl_18yoM_unc <- multiCA.test(sex_of_sps ~ year, data=yrbs_18yoM)
print(multiCA_szabo_tbl_18yoM_unc)

####################################################################

##assessing trends in sexual identity by age excluding everyone who has never had sex 
######################################################################################


#14 year old female
szabo_table_14yoF <- table(yrbs_14yoF$ever_sex_sps, yrbs_14yoF$year)
prop.table(szabo_table_14yoF)

df_proportions_14yoF <- yrbs_14yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_14yoF)

## using formula interface
multiCA_szabo_tbl_14yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_14yoF)
print(multiCA_szabo_tbl_14yoF)


#15 year old female
szabo_table_15yoF <- table(yrbs_15yoF$ever_sex_sps, yrbs_15yoF$year)
prop.table(szabo_table_15yoF)


df_proportions_15yoF <- yrbs_15yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_15yoF)

## using formula interface
multiCA_szabo_tbl_15yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_15yoF)
print(multiCA_szabo_tbl_15yoF)


#16 year old female
szabo_table_16yoF <- table(yrbs_16yoF$ever_sex_sps, yrbs_16yoF$year)
prop.table(szabo_table_16yoF)

df_proportions_16yoF <- yrbs_16yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_16yoF)

## using formula interface
multiCA_szabo_tbl_16yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_16yoF)
print(multiCA_szabo_tbl_16yoF)


#17 year old female
szabo_table_17yoF <- table(yrbs_17yoF$ever_sex_sps, yrbs_17yoF$year)
prop.table(szabo_table_17yoF)

df_proportions_17yoF <- yrbs_17yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_17yoF)

## using formula interface
multiCA_szabo_tbl_17yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_17yoF)
print(multiCA_szabo_tbl_17yoF)


#18 year old female
szabo_table_18yoF <- table(yrbs_18yoF$ever_sex_sps, yrbs_18yoF$year)
prop.table(szabo_table_18yoF)

df_proportions_18yoF <- yrbs_18yoF %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_18yoF)

## using formula interface
multiCA_szabo_tbl_18yoF <- multiCA.test(ever_sex_sps ~ year, data=yrbs_18yoF)
print(multiCA_szabo_tbl_18yoF)



#14 year old male
szabo_table_14yoM <- table(yrbs_14yoM$ever_sex_sps, yrbs_14yoM$year)
prop.table(szabo_table_14yoM)

df_proportions_14yoM <- yrbs_14yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_14yoM)

## using formula interface
multiCA_szabo_tbl_14yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_14yoM)
print(multiCA_szabo_tbl_14yoM)



#15 year old male
szabo_table_15yoM <- table(yrbs_15yoM$ever_sex_sps, yrbs_15yoM$year)
prop.table(szabo_table_15yoM)

df_proportions_15yoM <- yrbs_15yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_15yoM)

## using formula interface
multiCA_szabo_tbl_15yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_15yoM)
print(multiCA_szabo_tbl_15yoM)



#16 year old male
szabo_table_16yoM <- table(yrbs_16yoM$ever_sex_sps, yrbs_16yoM$year)
prop.table(szabo_table_16yoM)

df_proportions_16yoM <- yrbs_16yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_16yoM)

## using formula interface
multiCA_szabo_tbl_16yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_16yoM)
print(multiCA_szabo_tbl_16yoM)


#17 year old male
szabo_table_17yoM <- table(yrbs_17yoM$ever_sex_sps, yrbs_17yoM$year)
prop.table(szabo_table_17yoM)

df_proportions_17yoM <- yrbs_17yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_17yoM)

## using formula interface
multiCA_szabo_tbl_17yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_17yoM)
print(multiCA_szabo_tbl_17yoM)


#18 year old male
szabo_table_18yoM <- table(yrbs_18yoM$ever_sex_sps, yrbs_18yoM$year)
prop.table(szabo_table_18yoM)


df_proportions_18yoM <- yrbs_18yoM %>%
  group_by(year) %>%
  summarise(
    females_prop = sum(ever_sex_sps == "2_female", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    males_prop = sum(ever_sex_sps == "3_male", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100,
    both_prop = sum(ever_sex_sps == "4_fem+mal", na.rm = T) / sum(!is.na(ever_sex_sps)) * 100
  )

print(df_proportions_18yoM)

## using formula interface
multiCA_szabo_tbl_18yoM <- multiCA.test(ever_sex_sps ~ year, data=yrbs_18yoM)
print(multiCA_szabo_tbl_18yoM)


##############################################################################################
#post hoc analyses on trend test for males who have sex with males (including those who 
#have sex with males and females) to see if there is a sig. change over time. 
#Only include those who have had sex 

##create a new variable for males with any male partners vs males with no male partners
#yrbs_male 

yrbs_male$ever_sex_sps_D <- ifelse(yrbs_male$ever_sex_sps == "2_female", "only_female",
                                   ifelse(yrbs_male$ever_sex_sps %in% c("3_male", "4_fem+mal"), "any_male", NA))


table(yrbs_male$ever_sex_sps_D)



#Trend test for any males with male partners
szabo_M_male_sps <- table(yrbs_male$ever_sex_sps_D, yrbs_male$year)
prop.table(szabo_M_male_sps)

## using formula interface
multiCA_szabo_tbl_M_male_sps <- multiCA.test(ever_sex_sps_D ~ year, data=yrbs_male)

multiCA_szabo_tbl_M_male_sps

df_proportions_M_male_sps <- yrbs_male %>%
  group_by(year) %>%
  summarise(
    only_females_prop = sum(ever_sex_sps_D == "only_female", na.rm = T) / sum(!is.na(ever_sex_sps_D)) * 100,
    any_males_prop = sum(ever_sex_sps_D == "any_male", na.rm = T) / sum(!is.na(ever_sex_sps_D)) * 100
  )

print(df_proportions_M_male_sps)


##create a new variable for females with any female partners vs females with no female partners
#yrbs_female 

yrbs_female$ever_sex_sps_D <- ifelse(yrbs_female$ever_sex_sps == "3_male", "only_male",
                                     ifelse(yrbs_female$ever_sex_sps %in% c("2_female", "4_fem+mal"), "any_female", NA))


table(yrbs_female$ever_sex_sps_D)


#Trend test for any males with male partners
szabo_F_female_sps <- table(yrbs_female$ever_sex_sps_D, yrbs_female$year)
prop.table(szabo_F_female_sps)

## using formula interface
multiCA_szabo_tbl_F_female_sps <- multiCA.test(ever_sex_sps_D ~ year, data=yrbs_female)

multiCA_szabo_tbl_F_female_sps

df_proportions_F_female_sps <- yrbs_female %>%
  group_by(year) %>%
  summarise(
    only_males_prop = sum(ever_sex_sps_D == "only_male", na.rm = T) / sum(!is.na(ever_sex_sps_D)) * 100,
    any_females_prop = sum(ever_sex_sps_D == "any_female", na.rm = T) / sum(!is.na(ever_sex_sps_D)) * 100
  )

print(df_proportions_F_female_sps)

