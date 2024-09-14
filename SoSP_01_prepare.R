###################################################################################
## This is the new YRBS prepare script
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
# Note: before calling in data, ensure that the directory location is correct; you may need to change that used here.

#################### Prepare dataset for 2015 to 2019 ####################
# `yrbs_years` object 
yrbs_years <- seq(15, 19, 2)
yrbs_data_in <- read.csv("data - raw/yrbs_clean.csv")

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

# * so variable ------
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

# * ever_sex ------
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

# * sex_of_sps -----
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
# Notes on the meaning of each variable 
#q15 = unsafe at school
#q19 = sexual coercion ever
#q20 = sexual coercion last year
#q21 = dating sexual coercion
#q22 = dating violence
#q23 = bullied at school
#q24 = cyberbullying
#q25- depression 
#q26- SI 
#q27- suicide plan 
#q28- suicide attempt 
#q29- treated for suicide attempt 
#q30- tried smoking 
#q32- smoking days 
#q34- ever vaped 
#q41- drinking days 
#q43- number of drinks max 
#q45- number of times weed 
#q47- weed days 
#q50- cocaine 
#q51- inhalants 
#q52- opioids 
#q53- meth 
#q54- MDMA 
#q84- HIV test ever

# * restrict to on variables needed for analysis -----
varnames <- c("year", "weight", "PSU", "stratum", "age", "sex", "so", "so_new",
              "ever_sex", "sex_of_sps", "cohort", "so_21", "source",
              "q15", "q19", "q21", "q22", "q23", "q24", "q25", "q26", 
              "q27", "q28", "q29", "q30", "q32", "q34", "q41", "q43", "q45", 
              "q47", "q50", "q51", "q52", "q53", "q54", "q84" 
)

for (year in yrbs_years) {
  yrbs[[year]] <- yrbs[[year]][,varnames]
}

# * generate a YRBS 2015:2021 dataset to merge with 2021 ------
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
              "cohort", "source", #added new vars for analyses starting with q15
              "q14", "q19", "q21", "q22",  "q23", "q24", "q25", "q26", "q27", "q28", 
              "q29", "q30", "q32", "q34", "q41", "q43", "q45", "q47", "q50", "q51", 
              "q52", "q53", "q54", "q84" )

yrbs21_new <- yrbs21[,varnames]

names(yrbs21_new)[names(yrbs21_new) == "q14"] <- "q15"



# Write .rds file ------
write_rds(yrbs21_new,
          "data - raw/yrbs21_tomerge.RDS")



# merge datasets -------
yrbs_merge_new <- rbind(
  yrbs21_new,
  yrbs1519_merge
)


yrbs_merge_new <- yrbs_merge_new %>% 
  rename("unsafe_schl" = "q15")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("ever_forced_sex" = "q19")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("forced_sex_dating" = "q21")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("dating_violence" = "q22")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("bullied" = "q23")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("cyberbullied" = "q24")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("depression" = "q25")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("suic_idea" = "q26")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("suic_plan" = "q27")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("suic_attempt" = "q28")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("suic_treat" = "q29")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("ever_smoke" = "q30")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("smoke_days" = "q32")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("ever_vape" = "q34")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("drink_days" = "q41")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("max_drinks" = "q43")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("ever_weed" = "q45")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("weed_days" = "q47")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("cocaine" = "q50")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("inhalants" = "q51")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("heroin" = "q52")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("meth" = "q53")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("mdma" = "q54")

yrbs_merge_new <- yrbs_merge_new %>% 
  rename("ever_hiv_test" = "q84")

##############################################
## Recode variables 

#Missing school for safety concerns
#unsafe_schl
#1 = 0 days 
#2 = 1 day
#3 = 2 or 3 days 
#4 = 4 or 5 days 
#5 = 6 or more days 

yrbs_merge_new <- yrbs_merge_new %>% mutate(unsafe_schl = case_when(
  unsafe_schl == "1" ~ "0 days",
  unsafe_schl == "2" ~ "1 day",
  unsafe_schl == "3" ~ "2 or 3 days",
  unsafe_schl == "4" ~ "4 or 5 days",
  unsafe_schl == "5" ~ "6 or more days",
  TRUE ~ unsafe_schl  # Keep other values unchanged
))

yrbs_merge_new$unsafe_schl_D <- ifelse(yrbs_merge_new$unsafe_schl == "0 days", 0, 1)

table(yrbs_merge_new$unsafe_schl)
table(yrbs_merge_new$unsafe_schl_D)

yrbs_merge_new <- yrbs_merge_new %>% mutate(ever_forced_sex = case_when(
  ever_forced_sex == "1" ~ "Yes",
  ever_forced_sex == "2" ~ "No",
  TRUE ~ ever_forced_sex # Keep other values unchanged
))

yrbs_merge_new$ever_forced_sex <- ifelse(yrbs_merge_new$ever_forced_sex == "No", 0, 1)

yrbs_merge_new <- yrbs_merge_new %>% mutate(forced_sex_dating = case_when(
  forced_sex_dating == "1" ~ "0 times",
  forced_sex_dating == "2" ~ "1 time",
  forced_sex_dating == "3" ~ "2 or 3 times",
  forced_sex_dating == "4" ~ "4 or 5 times",
  forced_sex_dating == "5" ~ "6 or more times",
  forced_sex_dating == "6" ~ "Did not date",
  TRUE ~ forced_sex_dating  # Keep other values unchanged
))

table(yrbs_merge_new$forced_sex_dating)


yrbs_merge_new <- yrbs_merge_new %>% mutate(dating_violence = case_when(
  dating_violence == "1" ~ "0 times",
  dating_violence == "2" ~ "1 time",
  dating_violence == "3" ~ "2 or 3 times",
  dating_violence == "4" ~ "4 or 5 times",
  dating_violence == "5" ~ "6 or more times",
  dating_violence == "6" ~ "Did not date",
  TRUE ~ dating_violence  # Keep other values unchanged
))

table(yrbs_merge_new$dating_violence)

yrbs_merge_new <- yrbs_merge_new %>%
  mutate(any_dating_violence = case_when(
    dating_violence == "did not date" ~ NA_character_,
    dating_violence == "0 times" ~ "No",
    TRUE ~ "Yes"
  ))


table(yrbs_merge_new$any_dating_violence)
yrbs_merge_new$any_dating_violence <- ifelse(yrbs_merge_new$any_dating_violence == "No", 0, 1)

#bullied 
# 1 = yes 
# 2 = no

yrbs_merge_new <- yrbs_merge_new %>% mutate(bullied = case_when(
  bullied == "1" ~ "Yes",
  bullied == "2" ~ "No",
  TRUE ~ bullied  # Keep other values unchanged
))

table(yrbs_merge_new$bullied)

yrbs_merge_new$bullied <- ifelse(yrbs_merge_new$bullied == "No", 0, 1)

#cyberbullied
#1 = yes
#2 = no

yrbs_merge_new <- yrbs_merge_new %>% mutate(cyberbullied = case_when(
  cyberbullied == "1" ~ "Yes",
  cyberbullied == "2" ~ "No",
  TRUE ~ cyberbullied  # Keep other values unchanged
))

yrbs_merge_new$cyberbullied <- ifelse(yrbs_merge_new$cyberbullied == "No", 0, 1)

#depression
#1 = yes
#2 = no 

yrbs_merge_new <- yrbs_merge_new %>% mutate(depression = case_when(
  depression == "1" ~ "Yes",
  depression == "2" ~ "No",
  TRUE ~ depression  # Keep other values unchanged
))


yrbs_merge_new$depression <- ifelse(yrbs_merge_new$depression == "Yes", 1, 0)


#suicidal ideation 
#suic_idea
#1 = yes
#2 = no

yrbs_merge_new <- yrbs_merge_new %>% mutate(suic_idea = case_when(
  suic_idea == "1" ~ "Yes",
  suic_idea == "2" ~ "No",
  TRUE ~ suic_idea  # Keep other values unchanged
))

yrbs_merge_new$suic_idea <- ifelse(yrbs_merge_new$suic_idea == "No", 0, 1)

#suicide plan
#suic_plan
#1 = yes
#2 = no

yrbs_merge_new <- yrbs_merge_new %>% mutate(suic_plan = case_when(
  suic_plan == "1" ~ "Yes",
  suic_plan == "2" ~ "No",
  TRUE ~ suic_plan  # Keep other values unchanged
))

table(yrbs_merge_new$suic_plan)
yrbs_merge_new$suic_plan <- ifelse(yrbs_merge_new$suic_plan == "No", 0, 1)

#suicide attempts 
#suic_attempt
#1 = 0 times
#2 = 1 time
#3 = 2 or 3 times 
#4 = 4 or 5 times 
#5 = 6 or more times

yrbs_merge_new <- yrbs_merge_new %>% mutate(suic_attempt = case_when(
  suic_attempt == "1" ~ "0 times",
  suic_attempt == "2" ~ "1 time",
  suic_attempt == "3" ~ "2 or 3 times",
  suic_attempt == "4" ~ "4 or 5 times",
  suic_attempt == "5" ~ "6 or more times",
  TRUE ~ suic_attempt  # Keep other values unchanged
))

yrbs_merge_new$suic_attempt_D <- ifelse(yrbs_merge_new$suic_attempt == "0 times", 0, 1)

table(yrbs_merge_new$suic_attempt)
table(yrbs_merge_new$suic_attempt_D)

#Received treatment for suicide attempt 
#suic_treat
#1 = yes
#2 = no

yrbs_merge_new <- yrbs_merge_new %>% mutate(suic_treat = case_when(
  suic_treat == "1" ~ "Yes",
  suic_treat == "2" ~ "No",
  suic_treat == "3" ~ "Did not attempt suicide",
  TRUE ~ suic_treat  # Keep other values unchanged
))

#Ever smoked tobacco
#ever_smoke 
#1 = yes
#2 = no

yrbs_merge_new <- yrbs_merge_new %>% mutate(ever_smoke = case_when(
  ever_smoke == "1" ~ "Yes",
  ever_smoke == "2" ~ "No",
  TRUE ~ ever_smoke  # Keep other values unchanged
))

yrbs_merge_new$ever_smoke <- ifelse(yrbs_merge_new$ever_smoke == "No", 0, 1)

#Number of days in a month smoking tobacco
#smoke_days 
#1 = 0 days 
#2 = 1 or 2 days 
#3 = 3 to 5 days 
#4 = 6 to 9 days 
#5 = 10 to 19 days 
#6 = 20 to 29 days 
#7 = all 30 days 

yrbs_merge_new <- yrbs_merge_new %>% mutate(smoke_days = case_when(
  smoke_days == "1" ~ "0 days",
  smoke_days == "2" ~ "1 or 2 days",
  smoke_days == "3" ~ "3 to 5 days",
  smoke_days == "4" ~ "6 to 9 days",
  smoke_days == "5" ~ "10 to 19 days",
  smoke_days == "6" ~ "20 to 29 days",
  smoke_days == "7" ~ "All 30 days",
  TRUE ~ smoke_days  # Keep other values unchanged
))



#Ever used an electronic cigarette or a vape pen
#ever_vape
#1 = yes
#2 = no

yrbs_merge_new <- yrbs_merge_new %>% mutate(ever_vape = case_when(
  ever_vape == "1" ~ "Yes",
  ever_vape == "2" ~ "No",
  TRUE ~ ever_vape  # Keep other values unchanged
))

# Create the new variable
yrbs_merge_new$smoke_vape <- ifelse(yrbs_merge_new$ever_smoke == "Yes" | yrbs_merge_new$ever_vape == "Yes", "Yes", "No")

table(yrbs_merge_new$smoke_vape)

yrbs_merge_new$smoke_vape <- ifelse(yrbs_merge_new$smoke_vape == "Yes", 1, 0)


# Create the new variable
yrbs_merge_new$any_bully <- ifelse(yrbs_merge_new$cyberbullied == 1 | yrbs_merge_new$bullied == 1, 1, 0)

table(yrbs_merge_new$any_bully)

#Number of days drinking in a month
#drink_days
#1 = 0 days 
#2 = 1 or 2 days 
#3 = 3 to 5 days 
#4 = 6 to 9 days 
#5 = 10 to 19 days 
#6 = 20 to 29 days 
#7 = all 30 days 

yrbs_merge_new <- yrbs_merge_new %>% mutate(drink_days = case_when(
  drink_days == "1" ~ "0 days",
  drink_days == "2" ~ "1 or 2 days",
  drink_days == "3" ~ "3 to 5 days",
  drink_days == "4" ~ "6 to 9 days",
  drink_days == "5" ~ "10 to 19 days",
  drink_days == "6" ~ "20 to 29 days",
  drink_days == "7" ~ "All 30 days",
  TRUE ~ drink_days  # Keep other values unchanged
))

#maximum number if drinks 
#max_drinks
#1 = Did not drink alcohol in past 30 days 
#2 = 1 or 2 drinks 
#3 = 3 drinks 
#4 = 4 drinks 
#5 = 5 drinks 
#6 = 6 or 7 drinks 
#7 = 8 or 9 drinks 
#8 = 10 or more drinks 

yrbs_merge_new <- yrbs_merge_new %>% mutate(max_drinks = case_when(
  max_drinks == "1" ~ "Did not drink in past 30 days",
  max_drinks == "2" ~ "1 or 2 drinks",
  max_drinks == "3" ~ "3 drinks",
  max_drinks == "4" ~ "4 drinks",
  max_drinks == "5" ~ "5 drinks",
  max_drinks == "6" ~ "6 or 7 drinks",
  max_drinks == "7" ~ "8 or 9 drinks",
  max_drinks == "8" ~ "10 or more drinks",
  TRUE ~ max_drinks  # Keep other values unchanged
))

yrbs_merge_new$ever_drink_D <- ifelse(yrbs_merge_new$drink_days == "0 days", 0, 1)

#Ever used marijuana 
#ever_weed 
#1 = 0 times 
#2 = 1 or 2 times 
#3 = 3 to 9 times 
#4 = 10 to 19 times 
#5 = 20 to 39 times 
#6 = 40 to 99 times 
#7 = 100 or more times 

yrbs_merge_new <- yrbs_merge_new %>% mutate(ever_weed = case_when(
  ever_weed == "1" ~ "0 times",
  ever_weed == "2" ~ "1 or 2 times",
  ever_weed == "3" ~ "3 to 9 times",
  ever_weed == "4" ~ "10 to 19 times",
  ever_weed == "5" ~ "20 to 39 times",
  ever_weed == "6" ~ "40 to 99 times",
  ever_weed == "7" ~ "100 or more times",
  TRUE ~ ever_weed  # Keep other values unchanged
))

yrbs_merge_new$ever_weed_D <- ifelse(yrbs_merge_new$ever_weed == "0 times", 0, 1)

table(yrbs_merge_new$ever_weed)
table(yrbs_merge_new$ever_weed_D)

#Number of days of marijuana use
#weed_days 
#1 = 0 times 
#2 = 1 or 2 times 
#3 = 3 to 9 times 
#4 = 10 to 19 times 
#5 = 20 to 39 times 
#6 = 40 or more times 

yrbs_merge_new <- yrbs_merge_new %>% mutate(weed_days = case_when(
  weed_days == "1" ~ "0 times",
  weed_days == "2" ~ "1 or 2 times",
  weed_days == "3" ~ "3 to 9 times",
  weed_days == "4" ~ "10 to 19 times",
  weed_days == "5" ~ "20 to 39 times",
  weed_days == "6" ~ "40 or more times",
  TRUE ~ weed_days  # Keep other values unchanged
))

#cocaine use
#cocaine
#1 = 0 times 
#2 = 1 or 2 times 
#3 = 3 to 9 times 
#4 = 10 to 19 times 
#5 = 20 to 39 times 
#6 = 40 or more times 

yrbs_merge_new <- yrbs_merge_new %>% mutate(cocaine = case_when(
  cocaine == "1" ~ "0 times",
  cocaine == "2" ~ "1 or 2 times",
  cocaine == "3" ~ "3 to 9 times",
  cocaine == "4" ~ "10 to 19 times",
  cocaine == "5" ~ "20 to 39 times",
  cocaine == "6" ~ "40 or more times",
  TRUE ~ cocaine  # Keep other values unchanged
))

yrbs_merge_new$ever_cocaine_D <- ifelse(yrbs_merge_new$cocaine == "0 times", 0, 1)

table(yrbs_merge_new$cocaine)
table(yrbs_merge_new$ever_cocaine_D)

#Use of inhalents 
#inhalants 
#1 = 0 times 
#2 = 1 or 2 times 
#3 = 3 to 9 times 
#4 = 10 to 19 times 
#5 = 20 to 39 times 
#6 = 40 or more times 

yrbs_merge_new <- yrbs_merge_new %>% mutate(inhalants = case_when(
  inhalants == "1" ~ "0 times",
  inhalants == "2" ~ "1 or 2 times",
  inhalants == "3" ~ "3 to 9 times",
  inhalants == "4" ~ "10 to 19 times",
  inhalants == "5" ~ "20 to 39 times",
  inhalants == "6" ~ "40 or more times",
  TRUE ~ inhalants  # Keep other values unchanged
))

yrbs_merge_new$ever_inhalants_D <- ifelse(yrbs_merge_new$inhalants == "0 times", 0, 1)

table(yrbs_merge_new$inhalants)
table(yrbs_merge_new$ever_inhalants_D)

#Heroin use
#heroin
#1 = 0 times 
#2 = 1 or 2 times 
#3 = 3 to 9 times 
#4 = 10 to 19 times 
#5 = 20 to 39 times 
#6 = 40 or more times 

yrbs_merge_new <- yrbs_merge_new %>% mutate(heroin = case_when(
  heroin == "1" ~ "0 times",
  heroin == "2" ~ "1 or 2 times",
  heroin == "3" ~ "3 to 9 times",
  heroin == "4" ~ "10 to 19 times",
  heroin == "5" ~ "20 to 39 times",
  heroin == "6" ~ "40 or more times",
  TRUE ~ heroin  # Keep other values unchanged
))

yrbs_merge_new$ever_heroin_D <- ifelse(yrbs_merge_new$heroin == "0 times", 0, 1)

table(yrbs_merge_new$heroin)
table(yrbs_merge_new$ever_heroin_D)
]

#Meth use
#meth
#1 = 0 times 
#2 = 1 or 2 times 
#3 = 3 to 9 times 
#4 = 10 to 19 times 
#5 = 20 to 39 times 
#6 = 40 or more times 

yrbs_merge_new <- yrbs_merge_new %>% mutate(meth = case_when(
  meth == "1" ~ "0 times",
  meth == "2" ~ "1 or 2 times",
  meth == "3" ~ "3 to 9 times",
  meth == "4" ~ "10 to 19 times",
  meth == "5" ~ "20 to 39 times",
  meth == "6" ~ "40 or more times",
  TRUE ~ meth  # Keep other values unchanged
))

yrbs_merge_new$ever_meth_D <- ifelse(yrbs_merge_new$meth == "0 times", 0, 1)

table(yrbs_merge_new$meth)
table(yrbs_merge_new$ever_meth_D)

#MDMA use
#mdma
#1 = 0 times 
#2 = 1 or 2 times 
#3 = 3 to 9 times 
#4 = 10 to 19 times 
#5 = 20 to 39 times 
#6 = 40 or more times 

yrbs_merge_new <- yrbs_merge_new %>% mutate(mdma = case_when(
  mdma == "1" ~ "0 times",
  mdma == "2" ~ "1 or 2 times",
  mdma == "3" ~ "3 to 9 times",
  mdma == "4" ~ "10 to 19 times",
  mdma == "5" ~ "20 to 39 times",
  mdma == "6" ~ "40 or more times",
  TRUE ~ mdma  # Keep other values unchanged
))

yrbs_merge_new$ever_mdma_D <- ifelse(yrbs_merge_new$mdma == "0 times", 0, 1)

table(yrbs_merge_new$mdma)
table(yrbs_merge_new$ever_mdma_D)




# Create the new variable for ever reporting drug use
yrbs_merge_new$ever_drugs <- ifelse(yrbs_merge_new$ever_mdma_D == 1 | 
                                      yrbs_merge_new$ever_meth_D == 1 | 
                                      yrbs_merge_new$ever_heroin_D == 1 | 
                                      yrbs_merge_new$ever_inhalants_D == 1 | 
                                      yrbs_merge_new$ever_cocaine_D == 1 | 
                                      yrbs_merge_new$ever_weed_D == 1, 
                               1, 0)

table(yrbs_merge_new$ever_drugs)

# Create the new variable for ever reporting drug use
yrbs_merge_new$ever_hard_drugs <- ifelse(yrbs_merge_new$ever_mdma_D == 1 | 
                                      yrbs_merge_new$ever_meth_D == 1 | 
                                      yrbs_merge_new$ever_heroin_D == 1 | 
                                      yrbs_merge_new$ever_inhalants_D == 1 | 
                                      yrbs_merge_new$ever_cocaine_D == 1, 
                                    1, 0)

table(yrbs_merge_new$ever_hard_drugs)

#Convert btwn 12 and 24 months, during past 12 months, and more than 24 months ago to YES
#convert never to NO
yrbs_merge_new <- yrbs_merge_new %>% mutate(ever_hiv_test = case_when(
  ever_hiv_test == "Between 12 and 24 months ago" ~ "Yes",
  ever_hiv_test == "During the past 12 months" ~ "Yes",
  ever_hiv_test == "More than 24 months ago" ~ "Yes",
  ever_hiv_test == "1" ~ "Yes",
  ever_hiv_test == "2" ~ "No",
  ever_hiv_test == "3" ~ "Not sure",
  ever_hiv_test == "Never" ~ "No",
  TRUE ~ ever_hiv_test  # Keep other values unchanged
))


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
table(yrbs_merge_new$discord_2, useNA = "ifany")

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
          "data - clean/yrbs_final_new.RDS")
