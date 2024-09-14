##### This script contains all packages used for YRBS SoSP Analyses -----


#NOTE: These packages will only run if you already have them installed on your computer. 
# If any of these packages still need to be installed on your computer, use the following command 
# before using this list to open the package in your library
# install.packages("name_of_package_here") <- put the name of the package you need to install in qoutes

library(visdat) #used to visualize missing data patterns in script 01
library(tidyverse) #used for data management
library(haven) #used for data management
library(dplyr) #used for data management
library(tidyr) #used for data management
library(tidyselect) # used for data management
library(DescTools) # for obtaining 95% CI for multi-nomial proportions (comparing 18 YO in BRFSS, YRBS)
library(magrittr) # used for piping
library(questionr) # used for applying weights to tables in BRFSS and YRBS
library(colorspace) # used for colors in graphs
library(dichromat) # used for colors in graphs
library(labelled) # used for data management
library(weights) # for using chi squared test with weighted data 
library(survey) # for weighted prev estimates and 95% CI (comparing 18 YO)
library(ggplot2) # for data visualization
library(epitools) # used for analyses
library(RColorBrewer) # used for additional color palettes in graphs and figures
library(coin) # used for analyses
library(multiCA) # used to run the Multi-nomial Cochran-Armitage Trend Tests
library(nnet) # used for analyses
library(mlogit) # used for multi-nomial logit models
library(lmtest) # used for diagnostics in analyses 
library(car) # used in analyses for applied regression

