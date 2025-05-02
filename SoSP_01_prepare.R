###################################################################################
## This is the SoSP YRBS prepare script
# Kendall Lawley | lawleyk@uw.edu | 2024 June
###################################################################################

### Prepare workspace -----  
# clear environment
rm(list = ls())

# * call in packages ----
source("SoSP_00_packages.R")

# * define functions -----
tableNA <- function(x, ...){
  table(x, useNA = "ifany", ...)  
}
# the `tableNA` function tabulates data as the `table` function, but includes "NA"s by default.

# * call in data ------------
# Note: before calling in data, ensure that the directory location is correct; you may need to change it from 
# what is indicated here on line 26. This is referenced in the README file associated with this project. 

#################### Prepare dataset for 2015 to 2019 ####################
# `yrbs_years` object 
yrbs_years <- seq(15, 19, 2)
yrbs_data_in <- read.csv("data - raw/yrbs.csv")

yrbs <- list()
for (yr in yrbs_years) {
  yrbs[[yr]] <- subset(yrbs_data_in, year == 2000+yr)
}


# data management --------
# * age ----------
for (yr in yrbs_years) {
  yrbs[[yr]]$age_cat <- factor(yrbs[[yr]]$age,
                               levels = 1:7,
                               labels = c(
                                 "<14",
                                 "<14",
                                 "14",
                                 "15",
                                 "16",
                                 "17",
                                 ">=18"))
}

for (yr in yrbs_years) {
  yrbs[[yr]]$age <- yrbs[[yr]]$age + 11 # make age variable numeric with actual age
}

for (yr in yrbs_years) {
  yrbs[[yr]] <- subset(yrbs[[yr]], (age >= 14 & !is.na(age))) # drop those under 14 or with no age data
}

# * sex --------
for (yr in yrbs_years) {
  yrbs[[yr]]$sex <- factor(yrbs[[yr]]$sex,
                           levels = 1:2,
                           labels = c("Female", "Male"))
  
  yrbs[[yr]] <- subset(yrbs[[yr]], !is.na(sex)) # drop those with no sex reported
}

# * remove observations with 100% missingness ------
for (yr in yrbs_years) {
  yrbs[[yr]] <- yrbs[[yr]][,colSums(is.na(yrbs[[yr]]))<nrow(yrbs[[yr]])]
}

# * keep only variables needed for restriction process (based on missingness investigation) ----
# The comments indicate the context of each variable. The number of each question is not always 
# consistent across YRBS years. This is indicated in the number next to each comment.

# NOTE: Not all of these variables will make it into the final dataset, but they should be included
# initially to identify participants who stopped the survey. 

#Variables from 2015
yrbs[[15]] <- yrbs[[15]] %>% select(c("year",		
                                      "survyear",		
                                      "weight",		
                                      "stratum",		
                                      "PSU",		
                                      "record",		
                                      "age",	#	1
                                      "sex",	#	2
                                      "grade",	#	3
                                      # hispanic ID	4
                                      # race ID	5
                                      "stheight",	#	6
                                      "stweight",	#	7
                                      "qbikehelmet",	#	8
                                      "q8",	# seatbelt use	9
                                      "q9",	# riding in car with driver who drank	10
                                      "q10",	# drove after drinking	11
                                      "q11",	# text while driving	12
                                      "q12",	# carry weapon	13
                                      "q13",	# weapon at school	15
                                      "q15",	# unsafe at school	16
                                      "q16",	# threatened at school	17
                                      "q17",	# physical fight	18
                                      "q18",	# fight at school	20
                                      "q19",	# sexual coercion	21
                                      "q22",	# dating violence	22
                                      "q21",	# dating pressure for sex	23
                                      "q23",	# bullied at school	24
                                      "q24",	# cyberbullying	25
                                      "q25",	# depressive sx	26
                                      "q26",	# S.I.	27
                                      "q27",	# suicide plan	28
                                      "q28",	# suicide attempt	29
                                      "q29",	# treated for S.A.	30
                                      "q30",	# tried smoking	31
                                      "q32",	# smoking days	33
                                      "q33",	# number of cigs per smoking day	34
                                      "q38",	# how many days smoked cigars	38
                                      "q34",	# ever vaped	39
                                      "q35",	# vaping days	40
                                      "q40",	# age at first alc drink	42
                                      "q41",	# drinking days	43
                                      "q43",	# number of drinks, max	45
                                      "q44",	# how get alc	46
                                      "q45",	# N times using cannabis	47
                                      "q46",	# age at first cannabis use	48
                                      "q47",	# cannabis days	49
                                      "q50",	# cocaine	50
                                      "q51",	# inhalants	51
                                      "q52",	# opioids	52
                                      "q53",	# meth	53
                                      "q54",	# MDMA	54
                                      "q48",	# synthetic cannabis	55
                                      "q55",	# steroids	56
                                      "q56",	# injection	58
                                      "q57",	# drugs sold school	59
                                      "q58",	# ever sex	60
                                      "q59",	# age at sexual debut	61
                                      "q60",	# N sex partners ever	62
                                      "q61",	# N SPs p3 mo	63
                                      "q62",	# drugs or drink before sex	64
                                      "q63",	# condom last sex	65
                                      "q64",	# contraception	66
                                      "q65",	# sex of sex partners	67
                                      "q66",	# ************* S.O.	68
                                      "sexid",
                                      "q67",	# weight self-describe	69
                                      "q68",	# weight change desire	70
                                      "q69",	# juice	71
                                      "q70",	# fruit	72
                                      "q71",	# salad	73
                                      "q72",	# potatoes	74
                                      "q73",	# carrots	75
                                      "q74",	# other veg	76
                                      "q75",	# soda	77
                                      "q76",	# milk	78
                                      "q77",	# breakfast	79
                                      "q78",	# physically active	80
                                      "q79",	# TV on school days	81
                                      "q80",	# video games on school days	82
                                      "q81",	# PE classes	83
                                      "q82",	# sports teams	84
                                      "q84",	# HIV test ever	85
                                      "q86",	# dentist	86
                                      "q87",	# asthma	87
                                      "q88",	# hours of sleep	88
                                      "q89"	# grades	89
))

#Variables from 2017
yrbs[[17]] <- yrbs[[17]] %>% select(c("year",		
                                      "survyear",		
                                      "weight",		
                                      "stratum",		
                                      "PSU",		
                                      "record",	
                                      "age",	# 	1
                                      "sex",	#	2
                                      "grade",	#	3
                                      #hispanic/latino	4
                                      #race	5
                                      "stheight",	#	6
                                      "stweight",	#	7
                                      "q8",	# seatbelt use	8
                                      "q9",	# riding in car with driver who drank	9
                                      "q10",	# drove after drinking	10
                                      "q11",	# text while driving	11
                                      "q12",	# carry weapon	12
                                      "q13",	# weapon at school	13
                                      "q14",	# gun at school	14
                                      "q15",	# unsafe at school	15
                                      "q16",	# threatened or injured at school	16
                                      "q17",	# physical fight	17
                                      "q18",	# fight at school	18
                                      "q19",	# sexual coercion ever	19
                                      "q20",	# sexual coercion last year	20
                                      "q21",	# dating sexual coercion	21
                                      "q22",	# dating violence	22
                                      "q23",	# builled on school property	23
                                      "q24",	# e-bullied	24
                                      "q25",	# depression screen	25
                                      "q26",	# considered suicide	26
                                      "q27",	# suicide plan	27
                                      "q28",	# suicide attempt	28
                                      "q29",	# suicide treatment 	29
                                      "q30",	# ever tried smoking	30
                                      "q31",	# age tried smoking	31
                                      "q32",	# number of days smoking	32
                                      "q33",	# number of cigs per day	33
                                      "q34",	# vaping ever	34
                                      "q35",	# vaping last month	35
                                      "q36",	# how get vapes	36
                                      "q37",	# tobacco products	37
                                      "q38",	# cigars	38
                                      "q39",	# try quit tobacco	39
                                      # 	40
                                      "q40",	# how old first alcohol	41
                                      "q41",	# how many drinking days last month	42
                                      "q44",	# how get alc last month	43
                                      "q42",	# binge drinking	44
                                      "q43",	# most drinks in a row	45
                                      "q45",	# how many times used cannabid	46
                                      "q46",	# age first cannabis	47
                                      "q47",	# cannabis, last month	48
                                      "q50",	# cocaine	49
                                      "q51",	# inhalants	50
                                      "q52",	# heroin	51
                                      "q53",	# meth	52
                                      "q54",	# mdma	53
                                      "q48",	# synthetic cannabis"	54
                                      "q55",	# steroids	55
                                      "q49",	# rx pain meds	56
                                      "q56",	# injection drugs	57
                                      "q57",	# bought drugs at school	58
                                      "q58",	# ever had sex	59
                                      "q59",	# age at sexual debuts	60
                                      "q60",	# how many sex partners ever	61
                                      "q61",	# how many sex partners last 3 months	62
                                      "q62",	# alc or drugs before last sex	63
                                      "q63",	# used condom?	64
                                      "q64",	# one form of contraception	65
                                      "q65",	# sex of sex partners	66
                                      "q66",	# ********** S.O.	67
                                      "sexid",
                                      "q67",	# over/under weight	68
                                      "q68",	# lose/gain weight	69
                                      "q69",	# drank juice	70
                                      "q70",	# ate fruit	71
                                      "q71",	# ate salad	72
                                      "q72",	# ate potatoes	73
                                      "q73",	# carrots	74
                                      "q74",	# other veggies	75
                                      "q75",	# soda	76
                                      "q76",	# milk	77
                                      "q77",	# breakfast	78
                                      "q78",	# physically active	79
                                      "q79",	# TV hours school day	80
                                      "q80",	# video games school day	81
                                      "q81",	# phys ed	82
                                      "q82",	# sports teams	83
                                      "q83",	# concussion	84
                                      "q84",	# HIV test	85
                                      "q86",	# dentist	86
                                      "q87",	# asthma	87
                                      "q88",	# hours of sleep	88
                                      "q89"	# grades	89
))

#Variables from 2019
# Note: in the absence of a data dictionary with answer breakdowns, we assume that 2019 questions were asked in numeric order.
yrbs[[19]] <- yrbs[[19]] %>% select(-(starts_with("qn")))
yrbs[[19]] <- yrbs[[19]][,1:105]
yrbs[[19]] <- yrbs[[19]] %>% select(c("year",
                                      "survyear",
                                      "weight",
                                      "stratum",
                                      "PSU",
                                      "record",
                                      "age",
                                      "sex",
                                      "grade",
                                      "q8",
                                      "q9",
                                      "q10",
                                      "q11",
                                      "q12",
                                      "q13",
                                      "q14",
                                      "q15",
                                      "q16",
                                      "q17",
                                      "q18",
                                      "q19",
                                      "q20",
                                      "q21",
                                      "q22",
                                      "q23",
                                      "q24",
                                      "q25",
                                      "q26",
                                      "q27",
                                      "q28",
                                      "q29",
                                      "q30",
                                      "q31",
                                      "q32",
                                      "q33",
                                      "q34",
                                      "q35",
                                      "q36",
                                      "q37",
                                      "q38",
                                      "q39",
                                      "q40",
                                      "q41",
                                      "q42",
                                      "q43",
                                      "q44",
                                      "q45",
                                      "q46",
                                      "q47",
                                      "q48",
                                      "q49",
                                      "q50",
                                      "q51",
                                      "q52",
                                      "q53",
                                      "q54",
                                      "q55",
                                      "q56",
                                      "q57",
                                      "q58",
                                      "q59",
                                      "q60",
                                      "q61",
                                      "q62",
                                      "q63",
                                      "q64",
                                      "q65",
                                      "q66",
                                      "sexid",
                                      "q67",
                                      "q68",
                                      "q69",
                                      "q70",
                                      "q71",
                                      "q72",
                                      "q73",
                                      "q74",
                                      "q75",
                                      "q76",
                                      "q77",
                                      "q78",
                                      "q79",
                                      "q80",
                                      "q81",
                                      "q82",
                                      "q83",
                                      "q84",
                                      "q85",
                                      "q86",
                                      "q87",
                                      "q88"))

                                      #"q89")) removedd q89 (grades) for now 



# * remove those who apparently stopped the survey (based on missingness investigation
for (year in yrbs_years) {
  yrbs[[year]]$drop1 <- ifelse((is.na(yrbs[[year]]$q61) &
                                  is.na(yrbs[[year]]$q62) &
                                  is.na(yrbs[[year]]$q63) &
                                  is.na(yrbs[[year]]$q64) &
                                  is.na(yrbs[[year]]$q65) &
                                  is.na(yrbs[[year]]$q66) &        
                                  is.na(yrbs[[year]]$q67) &        
                                  is.na(yrbs[[year]]$q68) &         
                                  is.na(yrbs[[year]]$q69) &        
                                  is.na(yrbs[[year]]$q70) &        
                                  is.na(yrbs[[year]]$q71) &       
                                  is.na(yrbs[[year]]$q72) &      
                                  is.na(yrbs[[year]]$q73) &     
                                  is.na(yrbs[[year]]$q74) &    
                                  is.na(yrbs[[year]]$q75) &  
                                  is.na(yrbs[[year]]$q76) &  
                                  is.na(yrbs[[year]]$q77) & 
                                  is.na(yrbs[[year]]$q78) &
                                  is.na(yrbs[[year]]$q79) &
                                  is.na(yrbs[[year]]$q80) &
                                  is.na(yrbs[[year]]$q81) &
                                  is.na(yrbs[[year]]$q82) &
                                  is.na(yrbs[[year]]$q84) &
                                  is.na(yrbs[[year]]$q86) &
                                  is.na(yrbs[[year]]$q87) &
                                  is.na(yrbs[[year]]$q88)),1,0)
           
}

for (year in yrbs_years) {
  yrbs[[year]] <- subset(yrbs[[year]], drop1 != 1)
}


# * remove those who systematically were not asked sex questions
for (year in yrbs_years) {
  yrbs[[year]]$drop2 <- ifelse((is.na(yrbs[[year]]$q58) &
                                  is.na(yrbs[[year]]$q59) &
                                  is.na(yrbs[[year]]$q60) &
                                  is.na(yrbs[[year]]$q61) &
                                  is.na(yrbs[[year]]$q62) &
                                  is.na(yrbs[[year]]$q63) &
                                  is.na(yrbs[[year]]$q64) &
                                  is.na(yrbs[[year]]$q65) &
                                  is.na(yrbs[[year]]$q66)),1,0)
}

for (year in yrbs_years) {
  yrbs[[year]] <- subset(yrbs[[year]], drop2 != 1)
}

# * Sexual orientation variable ------
for (year in yrbs_years) {
  yrbs[[year]]$sexid[is.na(yrbs[[year]]$sexid)] <- 5
  
  yrbs[[year]]$so <- factor(yrbs[[year]]$sexid,
                            levels = c(1:5),
                            labels = c(
                              "1_straight",
                              "2_lesgay",
                              "3_bi",
                              "4_dkns",
                              "5_pnta"
                            )
  )
  yrbs[[year]]$so_new <- factor(yrbs[[year]]$sexid,
                                levels = c(1:5),
                                labels = c(
                                  "1_straight",
                                  "2_lesgay",
                                  "3_bi",
                                  "4_dko",
                                  "5_ref"
                                )
  )
  
}

# * ever_sex variable ------
for (year in yrbs_years) {
  yrbs[[year]]$ever_sex <- 2-yrbs[[year]]$q58
  
  # * apply informative missingness to `ever_sex'
  yrbs[[year]]$ever_sex[yrbs[[year]]$q58 == 2 | #"Never had sex"
                          yrbs[[year]]$q59 == 1 | #"Never had sex"
                          yrbs[[year]]$q60 == 1 | #"Never had sex"
                          yrbs[[year]]$q61 == 1 | #"Never had sex"
                          yrbs[[year]]$q62 == 1 | #"Never had sex"
                          yrbs[[year]]$q63 == 1 | #"Never had sex"
                          yrbs[[year]]$q64 == 1 | #"Never had sex"
                          yrbs[[year]]$q65 == 1  #"Never had sex"
  ] <- 0 #any time R indicates never having had sex in a later question, ] 
  
  yrbs[[year]]$ever_sex[yrbs[[year]]$q58 != 2 & !is.na(yrbs[[year]]$q58)] <- 1
  yrbs[[year]]$ever_sex[yrbs[[year]]$q59 != 1 & !is.na(yrbs[[year]]$q59)] <- 1
  yrbs[[year]]$ever_sex[yrbs[[year]]$q60 != 1 & !is.na(yrbs[[year]]$q60)] <- 1
  yrbs[[year]]$ever_sex[yrbs[[year]]$q61 != 1 & !is.na(yrbs[[year]]$q61)] <- 1
  yrbs[[year]]$ever_sex[yrbs[[year]]$q62 != 1 & !is.na(yrbs[[year]]$q62)] <- 1
  yrbs[[year]]$ever_sex[yrbs[[year]]$q63 != 1 & !is.na(yrbs[[year]]$q63)] <- 1
  yrbs[[year]]$ever_sex[yrbs[[year]]$q64 != 1 & !is.na(yrbs[[year]]$q64)] <- 1
  yrbs[[year]]$ever_sex[yrbs[[year]]$q65 != 1 & !is.na(yrbs[[year]]$q65)] <- 1
  
}

# * sex_of_sps variable -----
for (year in yrbs_years) {
  yrbs[[year]]$sex_of_sps <- factor(yrbs[[year]]$q65,
                                    levels = c(1:4),
                                    labels = c(
                                      "1_never",
                                      "2_female",
                                      "3_male",
                                      "4_fem+mal"
                                    ))
  
  # * apply informative missingness to `sex_of_sps'
  yrbs[[year]]$sex_of_sps[yrbs[[year]]$q58 == 2 | #"Never had sex"
                            yrbs[[year]]$q59 == 1 | #"Never had sex"
                            yrbs[[year]]$q60 == 1 | #"Never had sex"
                            yrbs[[year]]$q61 == 1 | #"Never had sex"
                            yrbs[[year]]$q62 == 1 | #"Never had sex"
                            yrbs[[year]]$q63 == 1 | #"Never had sex"
                            yrbs[[year]]$q64 == 1 | #"Never had sex"
                            yrbs[[year]]$ever_sex == 0
  ] <- "1_never" #any time R indicates never having had sex in a later question, ]
  
}

### REMOVE observations with NA for `sex_of_sps` or `ever_sex` --------
for (year in yrbs_years) {
  yrbs[[year]] <- subset(yrbs[[year]], !is.na(sex_of_sps))
  yrbs[[year]] <- subset(yrbs[[year]], !is.na(ever_sex))
}

# * cohort --------
for (year in yrbs_years) {
  yrbs[[year]]$cohort <- yrbs[[year]]$year - yrbs[[year]]$age
}

# * so_21 --------
for (year in yrbs_years) {
  yrbs[[year]]$so_21 <- NA
}

# * source ----
for (year in yrbs_years) {
  yrbs[[year]]$source <- "YRBS"
}



#####################################################################################


# * restrict to on variables needed for analysis -----
varnames <- c("year", "weight", "PSU", "stratum", "age", "sex", "so", "so_new",
              "ever_sex", "sex_of_sps", "cohort", "so_21", "source"
)

for (year in yrbs_years) {
  yrbs[[year]] <- yrbs[[year]][,varnames]
}

# * generate a YRBS 2015:2019 dataset to merge with 2021 ------
yrbs1519_merge <- rbind(yrbs[[15]],
                        yrbs[[17]],
                        yrbs[[19]])



yrbs1519_merge <- yrbs1519_merge %>% 
  rename("psu" = "PSU")



# Write .rds file ------
write_rds(yrbs1519_merge,
          "data - raw/yrbs1519_tomerge.RDS")


#################### Prepare dataset for 2021 ####################

# * call in data ------------
# Note: before calling in data, ensure that the directory location is correct; you may need to change that used here.
# This is referenced in the README file associated with this project. 
yrbs21 <- read.csv("data - raw/yrbs2021.csv")

# * prepare data for cleanup ------
names(yrbs21) <- tolower(names(yrbs21)) # make variable names lowercase
yrbs21 <- yrbs21 %>% mutate(id = row_number()) # generate an index variable (id)
yrbs21[yrbs21 == "Missing"] <- NA # make "missing" NA

### Remove observations with missing for sex, age -------
yrbs21 <- subset(yrbs21, !is.na(q1) & !is.na(q2))


### Define new variables / apply informative missingness as needed -----------
# * year ------
yrbs21$year <- 2021

# * source ------
yrbs21$source <- "YRBS"

# * age ------
yrbs21$age <- as.numeric(substr(yrbs21$q1, 1, 2))

# REMOVE those under 14 (N=94) ----------
yrbs21 <- yrbs21[yrbs21$age >= 14,] 

# * sex --------
yrbs21$sex <- yrbs21$q2

# *'so_21' -----------
yrbs21$so_21 <- NA

yrbs21$so_21[yrbs21$q65 == "Heterosexual (straight)"] <- "1_straight"
yrbs21$so_21[yrbs21$q65 == "Gay or lesbian"] <- "2_lesgay"
yrbs21$so_21[yrbs21$q65 == "Bisexual"] <- "3_bi"
yrbs21$so_21[yrbs21$q65 == "Not sure"] <- "4_dkns"
yrbs21$so_21[is.na(yrbs21$q65)] <- "5_pnta"
yrbs21$so_21[yrbs21$q65 == "Don't know what this means"] <- "6_dkwtm" 
yrbs21$so_21[yrbs21$q65 == "Some other way"] <- "7_somethingelse" 

# *'so' ------------
yrbs21$so[yrbs21$so_21 == "1_straight"] <- "1_straight"
yrbs21$so[yrbs21$so_21 == "2_lesgay"] <- "2_lesgay"
yrbs21$so[yrbs21$so_21 == "3_bi"] <- "3_bi"
yrbs21$so[yrbs21$so_21 == "4_dkns" |
            yrbs21$so_21 == "6_dkwtm"] <- "4_dkns"
yrbs21$so[yrbs21$so_21 == "5_pnta" |
            yrbs21$so_21 == "7_somethingelse"] <- "5_pnta"

# * so_new -------
yrbs21$so_new[yrbs21$so_21 == "1_straight"] <- "1_straight"
yrbs21$so_new[yrbs21$so_21 == "2_lesgay"] <- "2_lesgay"
yrbs21$so_new[yrbs21$so_21 == "3_bi"] <- "3_bi"
yrbs21$so_new[yrbs21$so_21 == "4_dkns" |
                yrbs21$so_21 == "6_dkwtm" |
                yrbs21$so_21 == "7_somethingelse"] <- "4_dko"
yrbs21$so_new[yrbs21$so_21 == "5_pnta"] <- "5_ref"

# *'cohort' --------------
yrbs21$cohort <- 2021-yrbs21$age

# * ever_sex -----
yrbs21$ever_sex <- NA

# informative missingness
yrbs21$ever_sex[yrbs21$q58 == "Never had sex" | 
                  yrbs21$q59 == "Never had sex" |
                  yrbs21$q60 == "Never had sex" |
                  yrbs21$q61 == "Never had sex" |     
                  yrbs21$q62 == "Never had sex" |  
                  yrbs21$q64 == "Never had sexual contact"
] <- 0 #any time R indicates never having had sex in a later question, 

yrbs21$ever_sex[yrbs21$q58 != "Never had sex" & !is.na(yrbs21$q58)] <- 1
yrbs21$ever_sex[yrbs21$q59 != "Never had sex" & !is.na(yrbs21$q59)] <- 1
yrbs21$ever_sex[yrbs21$q60 != "Never had sex" & !is.na(yrbs21$q60)] <- 1
yrbs21$ever_sex[yrbs21$q61 != "Never had sex" & !is.na(yrbs21$q61)] <- 1
yrbs21$ever_sex[yrbs21$q62 != "Never had sex" & !is.na(yrbs21$q62)] <- 1
yrbs21$ever_sex[yrbs21$q64 != "Never had sexual contact" & !is.na(yrbs21$q64)] <- 1

yrbs21$ever_sex[yrbs21$q57 == "No"] <- 0
yrbs21$ever_sex[yrbs21$q57 == "Yes"] <- 1

# * sex_of_sps ------
yrbs21$sex_of_sps <- NA

yrbs21$sex_of_sps[yrbs21$q64 == "Never had sexual contact"] <- "1_never"
yrbs21$sex_of_sps[yrbs21$q64 == "Females"] <- "2_female"
yrbs21$sex_of_sps[yrbs21$q64 == "Males"] <- "3_male"
yrbs21$sex_of_sps[yrbs21$q64 == "Females and males"] <- "4_fem+mal"


# NOTe: As before, not all of these variables will make it into the final data set, but they are initially
# included to identify participants who stopped the survey.

### Remove the "Survey Stoppers" ---------
yrbs21$drop <- ifelse((is.na(yrbs21$q60) &
                         is.na(yrbs21$q61) &
                         is.na(yrbs21$q62) &
                         is.na(yrbs21$q63) &
                         is.na(yrbs21$q64) &
                         is.na(yrbs21$q65) &
                         is.na(yrbs21$q66) &
                         is.na(yrbs21$q67) &
                         is.na(yrbs21$q68) &
                         is.na(yrbs21$q69) &
                         is.na(yrbs21$q70) &
                         is.na(yrbs21$q71) &
                         is.na(yrbs21$q72) &
                         is.na(yrbs21$q73) &
                         is.na(yrbs21$q74) &
                         is.na(yrbs21$q75) &
                         is.na(yrbs21$q76) &
                         is.na(yrbs21$q77) &
                         is.na(yrbs21$q78) &
                         is.na(yrbs21$q71) &
                         is.na(yrbs21$q72) &
                         is.na(yrbs21$q73) &
                         is.na(yrbs21$q74) &
                         is.na(yrbs21$q75) &
                         is.na(yrbs21$q76) &
                         is.na(yrbs21$q77) &
                         is.na(yrbs21$q78) &
                         is.na(yrbs21$q79) &
                         is.na(yrbs21$q80) &
                         is.na(yrbs21$q81) &
                         is.na(yrbs21$q82) &
                         is.na(yrbs21$q83) &
                         is.na(yrbs21$q84) &
                         is.na(yrbs21$q85) &
                         is.na(yrbs21$q86) &
                         is.na(yrbs21$q87) &
                         is.na(yrbs21$q88) &
                         is.na(yrbs21$q89) &
                         is.na(yrbs21$q90) &
                         is.na(yrbs21$q91) &
                         is.na(yrbs21$q92) &
                         is.na(yrbs21$q93) &
                         is.na(yrbs21$q94) &
                         is.na(yrbs21$q95) &
                         is.na(yrbs21$q96) &
                         is.na(yrbs21$q97) &
                         is.na(yrbs21$q98) &
                         is.na(yrbs21$q99)), 1, 0)

yrbs21 <- subset(yrbs21, drop == 0)

### Remove the people who answered none of the sexual behavior questions --------
yrbs21$drop2 <- ifelse((is.na(yrbs21$q57) &
                          is.na(yrbs21$q58) &
                          is.na(yrbs21$q59) &
                          is.na(yrbs21$q60) &
                          is.na(yrbs21$q61) &
                          is.na(yrbs21$q62) &
                          is.na(yrbs21$q63) &
                          is.na(yrbs21$q64)), 1, 0)

yrbs21 <- subset(yrbs21, drop2 == 0)

### Examine missingness for `so_21`, `sex_of_sps` and `ever_sex` -----
round(prop.table(tableNA(yrbs21$so_21))*100,1) #1.5% PNTA
round(prop.table(tableNA(yrbs21$sex_of_sps))*100,1) # 3.0% NA
round(prop.table(tableNA(yrbs21$ever_sex))*100,1) # 1.9% NA

### REMOVE Rs with NA for `sex_of_sps` and `ever_sex` --------
yrbs21 <- subset(yrbs21, !is.na(sex_of_sps))
yrbs21 <- subset(yrbs21, !is.na(ever_sex))

### Examine missingness after removing NAs for `sex_of_sps` and `ever_sex` --------
round(prop.table(tableNA(yrbs21$so_21))*100,1) #1.0% PNTA
round(prop.table(tableNA(yrbs21$sex_of_sps))*100,1) # 0 NA
round(prop.table(tableNA(yrbs21$ever_sex))*100,1) # 0 NA


# * restrict to only needed variables and merge with prior years ------
varnames <- c("year", "weight", "psu", "stratum", "age", "sex",
              "so", "so_new", "so_21", "ever_sex", "sex_of_sps",
              "cohort", "source"
              )

yrbs21_new <- yrbs21[,varnames]


# Write .rds file ------
write_rds(yrbs21_new,
          "data - raw/yrbs21_tomerge.RDS")



# merge datasets -------
yrbs_merge_new <- rbind(
  yrbs21_new,
  yrbs1519_merge
)



yrbs_wt <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~weight,
  data = yrbs_merge_new,
  nest = TRUE
)


########################### Make the discordant variables 

# Create a new variable straight_gay based on so_new
yrbs_merge_new$straight_gay <- ifelse(yrbs_merge_new$so_new == "1_straight", "straight", 
                                 ifelse(yrbs_merge_new$so_new == "2_lesgay", "gay", NA))

# Check the result
table(yrbs_merge_new$straight_gay, useNA = "ifany")


# Create the new variable ever_sex_sps
yrbs_merge_new$ever_sex_sps <- ifelse(yrbs_merge_new$sex_of_sps == "1_never", NA, yrbs_merge_new$sex_of_sps)

# Check the result
table(yrbs_merge_new$ever_sex_sps, useNA = "ifany")

table(yrbs_merge_new$sex)
table(yrbs_merge_new$straight_gay)
table(yrbs_merge_new$ever_sex_sps)



# Create the new variable discord_1
yrbs_merge_new$discord_1 <- with(yrbs_merge_new, ifelse(
(sex == "Female" & straight_gay == "gay" & ever_sex_sps == "2_female") |
  (sex == "Male" & straight_gay == "gay" & ever_sex_sps == "3_male") |
  (sex == "Female" & straight_gay == "straight" & ever_sex_sps == "3_male") |
  (sex == "Male" & straight_gay == "straight" & ever_sex_sps == "2_female"), 0,
  ifelse(
(sex == "Female" & straight_gay == "gay" & ever_sex_sps == "3_male") |
  (sex == "Female" & straight_gay == "gay" & ever_sex_sps == "4_fem+mal") |
  (sex == "Female" & straight_gay == "straight" & ever_sex_sps == "2_female") |
  (sex == "Female" & straight_gay == "straight" & ever_sex_sps == "4_fem+mal") |
  (sex == "Male" & straight_gay == "gay" & ever_sex_sps == "2_female") |
  (sex == "Male" & straight_gay == "gay" & ever_sex_sps == "4_fem+mal") |
  (sex == "Male" & straight_gay == "straight" & ever_sex_sps == "3_male") |
  (sex == "Male" & straight_gay == "straight" & ever_sex_sps == "4_fem+mal"), 1, NA)
))

# Check the result
table(yrbs_merge_new$discord_1, useNA = "ifany")

# Create the new variable discord_2
yrbs_merge_new$discord_2 <- with(yrbs_merge_new, ifelse(
  (sex == "Female" & straight_gay == "gay" & ever_sex_sps == "2_female") |
    (sex == "Male" & straight_gay == "gay" & ever_sex_sps == "3_male") |
    (sex == "Female" & straight_gay == "straight" & ever_sex_sps == "3_male") |
    (sex == "Male" & straight_gay == "straight" & ever_sex_sps == "2_female") |
    (sex == "Female" & straight_gay == "gay" & ever_sex_sps == "4_fem+mal") |
    (sex == "Female" & straight_gay == "straight" & ever_sex_sps == "4_fem+mal") |
    (sex == "Male" & straight_gay == "gay" & ever_sex_sps == "4_fem+mal") |
    (sex == "Male" & straight_gay == "straight" & ever_sex_sps == "4_fem+mal"), 0,
  ifelse(
    (sex == "Female" & straight_gay == "gay" & ever_sex_sps == "3_male") |
      (sex == "Female" & straight_gay == "straight" & ever_sex_sps == "2_female") |
      (sex == "Male" & straight_gay == "gay" & ever_sex_sps == "2_female") |
      (sex == "Male" & straight_gay == "straight" & ever_sex_sps == "3_male"), 1, NA)
))

# Check the result
table(yrbs_merge_new$discord_2, useNA = "ifany")


# Write .rds file ------
write_rds(yrbs_merge_new,
          "data - clean/yrbs_final.RDS")


########################################################################################
# Additional analyses to assess predictors of missingness

# The following code will create new data sets that are used to identify any variables 
# predicting missingness in the dat aset

ssp_predictor_data1 <- yrbs_data_in

table(ssp_predictor_data1$year)

ssp_predictor_data1 <- ssp_predictor_data1[ssp_predictor_data1$year != 2013, ]

ssp_predictor_data1$year <- as.numeric(ssp_predictor_data1$year)
ssp_predictor_data1 <- ssp_predictor_data1[ssp_predictor_data1$year != 2013, ]

table(ssp_predictor_data1$year)

table(ssp_predictor_data1$age)

ssp_predictor_data1$age <- factor(ssp_predictor_data1$age,
                                  levels = 1:7,
                                  labels = c("<14", "<14", "14", "15", "16", "17", "18+"))


table(ssp_predictor_data1$age)

ssp_predictor_data1 <- ssp_predictor_data1[ssp_predictor_data1$age != "<14", ]

ssp_predictor_data1$age <- droplevels(ssp_predictor_data1$age)

table(ssp_predictor_data1$age)


ssp_predictor_data1$sex <- factor(ssp_predictor_data1$sex,
                                  levels = 1:2,
                                  labels = c("Female", "Male"))





# * so variable ------

ssp_predictor_data1$sexid[is.na(ssp_predictor_data1$sexid)] <- 5


ssp_predictor_data1$so <- factor(ssp_predictor_data1$sexid,
                                 levels = c(1:5),
                                 labels = c(
                                   "1_straight",
                                   "2_lesgay",
                                   "3_bi",
                                   "4_dkns",
                                   "5_pnta"
                                 )
)

ssp_predictor_data1$so_new <- factor(ssp_predictor_data1$sexid,
                                     levels = c(1:5),
                                     labels = c(
                                       "1_straight",
                                       "2_lesgay",
                                       "3_bi",
                                       "4_dko",
                                       "5_ref"
                                     )
)



# * ever_sex ------

ssp_predictor_data1$ever_sex <- 2-ssp_predictor_data1$q58


# * sex_of_sps -----

ssp_predictor_data1$sex_of_sps <- factor(ssp_predictor_data1$q65,
                                         levels = c(1:4),
                                         labels = c(
                                           "1_never",
                                           "2_female",
                                           "3_male",
                                           "4_fem+mal"
                                         ))



# * so_21 --------

ssp_predictor_data1$so_21 <- NA


# * source ----
ssp_predictor_data1$source <- "YRBS"


ssp_predictor_data1$missing_stopped <- NA

ssp_predictor_data1$missing_stopped <- ifelse((is.na(ssp_predictor_data1$q61) &
                                                 is.na(ssp_predictor_data1$q62) &
                                                 is.na(ssp_predictor_data1$q63) &
                                                 is.na(ssp_predictor_data1$q64) &
                                                 is.na(ssp_predictor_data1$q65) &
                                                 is.na(ssp_predictor_data1$q66) &        
                                                 is.na(ssp_predictor_data1$q67) &        
                                                 is.na(ssp_predictor_data1$q68) &         
                                                 is.na(ssp_predictor_data1$q69) &        
                                                 is.na(ssp_predictor_data1$q70) &        
                                                 is.na(ssp_predictor_data1$q71) &       
                                                 is.na(ssp_predictor_data1$q72) &      
                                                 is.na(ssp_predictor_data1$q73) &     
                                                 is.na(ssp_predictor_data1$q74) &    
                                                 is.na(ssp_predictor_data1$q75) &  
                                                 is.na(ssp_predictor_data1$q76) &  
                                                 is.na(ssp_predictor_data1$q77) & 
                                                 is.na(ssp_predictor_data1$q78) &
                                                 is.na(ssp_predictor_data1$q79) &
                                                 is.na(ssp_predictor_data1$q80) &
                                                 is.na(ssp_predictor_data1$q81) &
                                                 is.na(ssp_predictor_data1$q82) &
                                                 is.na(ssp_predictor_data1$q84) &
                                                 is.na(ssp_predictor_data1$q86) &
                                                 is.na(ssp_predictor_data1$q87) &
                                                 is.na(ssp_predictor_data1$q88)), 1, 0)


ssp_predictor_data1$missing_system_skip <- NA


ssp_predictor_data1$missing_system_skip <- ifelse((is.na(ssp_predictor_data1$q58) &
                                                     is.na(ssp_predictor_data1$q59) &
                                                     is.na(ssp_predictor_data1$q60) &
                                                     is.na(ssp_predictor_data1$q61) &
                                                     is.na(ssp_predictor_data1$q62) &
                                                     is.na(ssp_predictor_data1$q63) &
                                                     is.na(ssp_predictor_data1$q64) &
                                                     is.na(ssp_predictor_data1$q65) &
                                                     is.na(ssp_predictor_data1$q66)),1,0)


ssp_predictor_data1$missing_all_else <- ifelse((is.na(ssp_predictor_data1$ever_sex) &
                                                  is.na(ssp_predictor_data1$sex_of_sps)),1,0)


table(ssp_predictor_data1$missing_stopped)
table(ssp_predictor_data1$missing_system_skip)
table(ssp_predictor_data1$missing_all_else)


ssp_predictor_data1$missing_ssp <- NA

ssp_predictor_data1$missing_ssp <- ifelse(
  ssp_predictor_data1$missing_stopped == 1 | 
    ssp_predictor_data1$missing_system_skip == 1 | 
    ssp_predictor_data1$missing_all_else == 1, 
  1, 
  0
)

table(ssp_predictor_data1$missing_ssp)


###########################################################


# * restrict to only variables needed for analysis -----
varnames2 <- c("year", "weight", "PSU", "stratum", "age", "sex", "so", "so_new",
               "ever_sex", "sex_of_sps", "so_21", "source", "missing_ssp" 
)


# Restrict dataset to only include specified variables
ssp_predictor_data1 <- ssp_predictor_data1 %>% 
  select(all_of(varnames2))


ssp_predictor_data1 <- ssp_predictor_data1 %>% 
  rename("psu" = "PSU")

# Write .rds file ------
write_rds(ssp_predictor_data1,
          "data - raw/ssp_predictor_data1.RDS")


#################### Prepare dataset for 2021 ####################

# * call in data ------------
# Note: before calling in data, ensure that the directory location is correct; you may need to change that used here.
ssp_predictor_data2 <- read.csv("data - raw/yrbs2021.csv")

# * prepare data for cleanup ------
names(ssp_predictor_data2) <- tolower(names(ssp_predictor_data2)) # make variable names lowercase
ssp_predictor_data2 <- ssp_predictor_data2 %>% mutate(id = row_number()) # generate an index variable (id)
ssp_predictor_data2[ssp_predictor_data2 == "Missing"] <- NA # make "missing" NA



### Define new variables / apply informative missingness as needed -----------
# * year ------
ssp_predictor_data2$year <- 2021

# * source ------
ssp_predictor_data2$source <- "YRBS"

# * age ------
ssp_predictor_data2$age <- as.numeric(substr(ssp_predictor_data2$q1, 1, 2))

# REMOVE those under 14 (N=94) ----------
ssp_predictor_data2 <- ssp_predictor_data2[ssp_predictor_data2$age >= 14,] 

ssp_predictor_data2$age <- factor(ssp_predictor_data2$age)

levels(ssp_predictor_data2$age)[levels(ssp_predictor_data2$age) == "18"] <- "18+"

table(ssp_predictor_data2$age)

# *'so_21' -----------
ssp_predictor_data2$so_21 <- NA

ssp_predictor_data2$so_21[ssp_predictor_data2$q65 == "Heterosexual (straight)"] <- "1_straight"
ssp_predictor_data2$so_21[ssp_predictor_data2$q65 == "Gay or lesbian"] <- "2_lesgay"
ssp_predictor_data2$so_21[ssp_predictor_data2$q65 == "Bisexual"] <- "3_bi"
ssp_predictor_data2$so_21[ssp_predictor_data2$q65 == "Not sure"] <- "4_dkns"
ssp_predictor_data2$so_21[is.na(ssp_predictor_data2$q65)] <- "5_pnta"
ssp_predictor_data2$so_21[ssp_predictor_data2$q65 == "Don't know what this means"] <- "6_dkwtm" 
ssp_predictor_data2$so_21[ssp_predictor_data2$q65 == "Some other way"] <- "7_somethingelse" 

# *'so' ------------
ssp_predictor_data2$so[ssp_predictor_data2$so_21 == "1_straight"] <- "1_straight"
ssp_predictor_data2$so[ssp_predictor_data2$so_21 == "2_lesgay"] <- "2_lesgay"
ssp_predictor_data2$so[ssp_predictor_data2$so_21 == "3_bi"] <- "3_bi"
ssp_predictor_data2$so[ssp_predictor_data2$so_21 == "4_dkns" |
                         ssp_predictor_data2$so_21 == "6_dkwtm"] <- "4_dkns"
ssp_predictor_data2$so[ssp_predictor_data2$so_21 == "5_pnta" |
                         ssp_predictor_data2$so_21 == "7_somethingelse"] <- "5_pnta"

# * so_new -------
ssp_predictor_data2$so_new[ssp_predictor_data2$so_21 == "1_straight"] <- "1_straight"
ssp_predictor_data2$so_new[ssp_predictor_data2$so_21 == "2_lesgay"] <- "2_lesgay"
ssp_predictor_data2$so_new[ssp_predictor_data2$so_21 == "3_bi"] <- "3_bi"
ssp_predictor_data2$so_new[ssp_predictor_data2$so_21 == "4_dkns" |
                             ssp_predictor_data2$so_21 == "6_dkwtm" |
                             ssp_predictor_data2$so_21 == "7_somethingelse"] <- "4_dko"
ssp_predictor_data2$so_new[ssp_predictor_data2$so_21 == "5_pnta"] <- "5_ref"

ssp_predictor_data2$ever_sex[ssp_predictor_data2$q57 == "No"] <- 0
ssp_predictor_data2$ever_sex[ssp_predictor_data2$q57 == "Yes"] <- 1

# * sex_of_sps ------
ssp_predictor_data2$sex_of_sps <- NA

ssp_predictor_data2$sex_of_sps[ssp_predictor_data2$q64 == "Never had sexual contact"] <- "1_never"
ssp_predictor_data2$sex_of_sps[ssp_predictor_data2$q64 == "Females"] <- "2_female"
ssp_predictor_data2$sex_of_sps[ssp_predictor_data2$q64 == "Males"] <- "3_male"
ssp_predictor_data2$sex_of_sps[ssp_predictor_data2$q64 == "Females and males"] <- "4_fem+mal"

# * sex --------
ssp_predictor_data2$sex <- ssp_predictor_data2$q2


ssp_predictor_data2$missing_stopped <- NA

ssp_predictor_data2$missing_stopped <- ifelse((is.na(ssp_predictor_data2$q60) &
                                                 is.na(ssp_predictor_data2$q61) &
                                                 is.na(ssp_predictor_data2$q62) &
                                                 is.na(ssp_predictor_data2$q63) &
                                                 is.na(ssp_predictor_data2$q64) &
                                                 is.na(ssp_predictor_data2$q65) &
                                                 is.na(ssp_predictor_data2$q66) &
                                                 is.na(ssp_predictor_data2$q67) &
                                                 is.na(ssp_predictor_data2$q68) &
                                                 is.na(ssp_predictor_data2$q69) &
                                                 is.na(ssp_predictor_data2$q70) &
                                                 is.na(ssp_predictor_data2$q71) &
                                                 is.na(ssp_predictor_data2$q72) &
                                                 is.na(ssp_predictor_data2$q73) &
                                                 is.na(ssp_predictor_data2$q74) &
                                                 is.na(ssp_predictor_data2$q75) &
                                                 is.na(ssp_predictor_data2$q76) &
                                                 is.na(ssp_predictor_data2$q77) &
                                                 is.na(ssp_predictor_data2$q78) &
                                                 is.na(ssp_predictor_data2$q71) &
                                                 is.na(ssp_predictor_data2$q72) &
                                                 is.na(ssp_predictor_data2$q73) &
                                                 is.na(ssp_predictor_data2$q74) &
                                                 is.na(ssp_predictor_data2$q75) &
                                                 is.na(ssp_predictor_data2$q76) &
                                                 is.na(ssp_predictor_data2$q77) &
                                                 is.na(ssp_predictor_data2$q78) &
                                                 is.na(ssp_predictor_data2$q79) &
                                                 is.na(ssp_predictor_data2$q80) &
                                                 is.na(ssp_predictor_data2$q81) &
                                                 is.na(ssp_predictor_data2$q82) &
                                                 is.na(ssp_predictor_data2$q83) &
                                                 is.na(ssp_predictor_data2$q84) &
                                                 is.na(ssp_predictor_data2$q85) &
                                                 is.na(ssp_predictor_data2$q86) &
                                                 is.na(ssp_predictor_data2$q87) &
                                                 is.na(ssp_predictor_data2$q88) &
                                                 is.na(ssp_predictor_data2$q89) &
                                                 is.na(ssp_predictor_data2$q90) &
                                                 is.na(ssp_predictor_data2$q91) &
                                                 is.na(ssp_predictor_data2$q92) &
                                                 is.na(ssp_predictor_data2$q93) &
                                                 is.na(ssp_predictor_data2$q94) &
                                                 is.na(ssp_predictor_data2$q95) &
                                                 is.na(ssp_predictor_data2$q96) &
                                                 is.na(ssp_predictor_data2$q97) &
                                                 is.na(ssp_predictor_data2$q98) &
                                                 is.na(ssp_predictor_data2$q99)), 1, 0)


ssp_predictor_data2$missing_system_skip <- NA

ssp_predictor_data2$missing_system_skip <- ifelse((is.na(ssp_predictor_data2$q57) &
                                                     is.na(ssp_predictor_data2$q58) &
                                                     is.na(ssp_predictor_data2$q59) &
                                                     is.na(ssp_predictor_data2$q60) &
                                                     is.na(ssp_predictor_data2$q61) &
                                                     is.na(ssp_predictor_data2$q62) &
                                                     is.na(ssp_predictor_data2$q63) &
                                                     is.na(ssp_predictor_data2$q64)), 1, 0)

table(ssp_predictor_data2$missing_system_skip)

ssp_predictor_data2$missing_all_else <- NA

ssp_predictor_data2$missing_all_else <- ifelse((is.na(ssp_predictor_data2$ever_sex) &
                                                  is.na(ssp_predictor_data2$sex_of_sps)),1,0)


table(ssp_predictor_data2$missing_all_else)


ssp_predictor_data2$missing_ssp <- NA

ssp_predictor_data2$missing_ssp <- ifelse(
  ssp_predictor_data2$missing_stopped == 1 | 
    ssp_predictor_data2$missing_system_skip == 1 | 
    ssp_predictor_data2$missing_all_else == 1, 
  1, 
  0
)

table(ssp_predictor_data2$missing_ssp)



varnames2 <- c("year", "weight", "psu", "stratum", "age", "sex",
               "so", "so_new", "so_21", "ever_sex", "sex_of_sps",
               "source", "missing_ssp"
               )


ssp_predictor_data2_new <- ssp_predictor_data2[,varnames2]


# Write .rds file ------
write_rds(ssp_predictor_data2_new,
          "data - raw/ssp_predictor_data2.RDS")


# Convert factors to characters
ssp_predictor_data1 <- ssp_predictor_data1 %>% mutate(across(where(is.factor), as.character))
ssp_predictor_data2_new <- ssp_predictor_data2_new %>% mutate(across(where(is.factor), as.character))

#select shared columns and merge 
shared_cols <- intersect(names(ssp_predictor_data1), names(ssp_predictor_data2_new))
ssp_predictor_data1 <- ssp_predictor_data1 %>% select(all_of(shared_cols))
ssp_predictor_data2_new <- ssp_predictor_data2_new %>% select(all_of(shared_cols))

ssp_predictor_data_final <- rbind(ssp_predictor_data1, ssp_predictor_data2_new)



#Factor age, sex, so_new, sex_of_sps, year, missing ssp
ssp_predictor_data_final$age <- factor(ssp_predictor_data_final$age)
ssp_predictor_data_final$sex <- factor(ssp_predictor_data_final$sex)
ssp_predictor_data_final$so_new <- factor(ssp_predictor_data_final$so_new)
ssp_predictor_data_final$sex_of_sps <- factor(ssp_predictor_data_final$sex_of_sps)
ssp_predictor_data_final$year <- factor(ssp_predictor_data_final$year)
ssp_predictor_data_final$missing_ssp <- factor(ssp_predictor_data_final$missing_ssp)
##############################################

