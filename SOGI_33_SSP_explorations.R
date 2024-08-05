### Script for SSP analysis

# This script's code uses the YRBS dataset, previously prepared in script "SOGI_31_SSP_prepare"
### to conduct SSP-related analyses

### Prepare workspace ----- 
# clear environment
#rm(list = ls())
library(RColorBrewer)

# packages
source("SOGI_00_packages.R")

# define functions
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}

# call in data
yrbs_final <- readRDS("data - clean/yrbs_final_new.rds")


#####

yrs <- seq(2015, 2021, 2)
nyrs <- length(yrs)
yrpos <- 1:nyrs

yrbs_by_yr <- lapply(yrs, function(x) yrbs_final[yrbs_final$year == x,])
yrbs_by_yr_F <- lapply(yrs, function(x) yrbs_final[yrbs_final$year == x  & yrbs_final$sex == "Female",])
yrbs_by_yr_M <- lapply(yrs, function(x) yrbs_final[yrbs_final$year == x  & yrbs_final$sex == "Male",])


df_prop_fem_uncond_by_age <- array(NA, dim=c(5,4,nyrs))
df_prop_mal_uncond_by_age <- array(NA, dim=c(5,4,nyrs))

for(yr in yrpos) {
  df_prop_fem_uncond_by_age[,,yr] <- as.matrix(yrbs_by_yr_F[[yr]] %>%
  group_by(age) %>%
  summarise(
    none_prop = sum(sex_of_sps == "1_never") / n() * 100,
    females_prop = sum(sex_of_sps == "2_female") / n() * 100,
    males_prop = sum(sex_of_sps == "3_male") / n() * 100,
    both_prop = sum(sex_of_sps == "4_fem+mal") / n() * 100
  ))[,2:5]

  df_prop_mal_uncond_by_age[,,yr] <- as.matrix(yrbs_by_yr_M[[yr]] %>%
     group_by(age) %>%
     summarise(
       none_prop = sum(sex_of_sps == "1_never") / n() * 100,
       malales_prop = sum(sex_of_sps == "2_female") / n() * 100,
       males_prop = sum(sex_of_sps == "3_male") / n() * 100,
       both_prop = sum(sex_of_sps == "4_fem+mal") / n() * 100
     ))[,2:5]
}

##############################################################################################
#######################################
########  PLOTS #######################
#######################################
##############################################################################################

###############################################################################################
# FIGURE 2: PLOTS BY SO AND YEAR FOR FEMALES (CONDITIONED ON EHHS)
# tables 5-9
#Straight female, lesbian female, bisexual female, don't know female, declined to answer female
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SSP/SSP_by_SO_and_year_fem_one_panel.png", 
    width = 12*300, height = 12*300, res = 300
)
{
  par(mfrow=c(2,2))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 3
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
  col_sps = c("#31BAF6", "#EEC441", "#37C817")
  pt_text_cex = 0.8
  
  lines(df_proportions_strfem$year, df_proportions_strfem$females_prop/100, lty = 2, 
        col = col_sps[1], lwd = my_lwd)
  lines(df_proportions_strfem$year, df_proportions_strfem$males_prop/100, lty = 3, 
        col = col_sps[2], lwd = my_lwd)
  lines(df_proportions_strfem$year, df_proportions_strfem$both_prop/100, lty = 4, 
        col = col_sps[3], lwd = my_lwd)
  
  lines(df_proportions_lesfem$year, df_proportions_lesfem$females_prop/100, lty = 2, 
        col = col_sps[1], lwd = my_lwd)
  lines(df_proportions_lesfem$year, df_proportions_lesfem$males_prop/100, lty = 3, 
        col = col_sps[2], lwd = my_lwd)
  lines(df_proportions_lesfem$year, df_proportions_lesfem$both_prop/100, lty = 4, 
        col = col_sps[3], lwd = my_lwd)

  lines(df_proportions_bifem$year, df_proportions_bifem$females_prop/100, lty = 2, 
        col = col_sps[1], lwd = my_lwd)
  lines(df_proportions_bifem$year, df_proportions_bifem$males_prop/100, lty = 3, 
        col = col_sps[2], lwd = my_lwd)
  lines(df_proportions_bifem$year, df_proportions_bifem$both_prop/100, lty = 4, 
        col = col_sps[3], lwd = my_lwd)
  
  lines(df_proportions_dkofem$year, df_proportions_dkofem$females_prop/100, lty = 2, 
        col = col_sps[1], lwd = my_lwd)
  lines(df_proportions_dkofem$year, df_proportions_dkofem$males_prop/100, lty = 3, 
        col = col_sps[2], lwd = my_lwd)
  lines(df_proportions_dkofem$year, df_proportions_dkofem$both_prop/100, lty = 4, 
        col = col_sps[3], lwd = my_lwd)
  
  lines(df_proportions_reffem$year, df_proportions_reffem$females_prop/100, lty = 2, 
        col = col_sps[1], lwd = my_lwd)
  lines(df_proportions_reffem$year, df_proportions_reffem$males_prop/100, lty = 3, 
        col = col_sps[2], lwd = my_lwd)
  lines(df_proportions_reffem$year, df_proportions_reffem$both_prop/100, lty = 4, 
        col = col_sps[3], lwd = my_lwd)

  points(df_proportions_strfem$year, df_proportions_strfem$females_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_strfem$year, df_proportions_strfem$males_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_strfem$year, df_proportions_strfem$both_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_lesfem$year, df_proportions_lesfem$females_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_lesfem$year, df_proportions_lesfem$males_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_lesfem$year, df_proportions_lesfem$both_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_bifem$year, df_proportions_bifem$females_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_bifem$year, df_proportions_bifem$males_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_bifem$year, df_proportions_bifem$both_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_dkofem$year, df_proportions_dkofem$females_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_dkofem$year, df_proportions_dkofem$males_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_dkofem$year, df_proportions_dkofem$both_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_reffem$year, df_proportions_reffem$females_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_reffem$year, df_proportions_reffem$males_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  points(df_proportions_reffem$year, df_proportions_reffem$both_prop/100, 
         col = "#FFFFFF", pch=19,cex=1.5)
  
  text(df_proportions_strfem$year, df_proportions_strfem$females_prop/100, 
         "S", col=col_sps[1], cex=pt_text_cex)
  text(df_proportions_strfem$year, df_proportions_strfem$males_prop/100, 
         "S", col=col_sps[2], cex=pt_text_cex)
  text(df_proportions_strfem$year, df_proportions_strfem$both_prop/100, 
         "S", col=col_sps[3], cex=pt_text_cex)
  text(df_proportions_lesfem$year, df_proportions_lesfem$females_prop/100, 
         "L", col=col_sps[1], cex=pt_text_cex)
  text(df_proportions_lesfem$year, df_proportions_lesfem$males_prop/100, 
         "L", col=col_sps[2], cex=pt_text_cex)
  text(df_proportions_lesfem$year, df_proportions_lesfem$both_prop/100, 
         "L", col=col_sps[3], cex=pt_text_cex)
  text(df_proportions_bifem$year, df_proportions_bifem$females_prop/100, 
         "B", col=col_sps[1], cex=pt_text_cex)
  text(df_proportions_bifem$year, df_proportions_bifem$males_prop/100, 
         "B", col=col_sps[2], cex=pt_text_cex)
  text(df_proportions_bifem$year, df_proportions_bifem$both_prop/100, 
         "B", col=col_sps[3], cex=pt_text_cex)
  text(df_proportions_dkofem$year, df_proportions_dkofem$females_prop/100, 
         "D", col=col_sps[1], cex=pt_text_cex)
  text(df_proportions_dkofem$year, df_proportions_dkofem$males_prop/100, 
         "O", col=col_sps[2], cex=pt_text_cex)
  text(df_proportions_dkofem$year, df_proportions_dkofem$both_prop/100, 
         "O", col=col_sps[3], cex=pt_text_cex)
  text(df_proportions_reffem$year, df_proportions_reffem$females_prop/100, 
         "O", col=col_sps[1], cex=pt_text_cex)
  text(df_proportions_reffem$year, df_proportions_reffem$males_prop/100, 
         "D", col=col_sps[2], cex=pt_text_cex)
  text(df_proportions_reffem$year, df_proportions_reffem$both_prop/100, 
         "D", col=col_sps[3], cex=pt_text_cex)

  # Create an empty plot for the legend
  #plot.new()
  # legend("center", c("Female partners only", 
  #                    "Male partners only", 
  #                    "Female and male partners"),
  #        col=c(col_sps[1], col_sps[2], col_sps[3]), 
  #        lty = 1:4, cex=0.8, bty = "o"
  # )
  
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

# FIGURE 4: PLOTS BY AGE AND YEAR
# tables 15-19
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SSP/SSP_by_age_and_year_four_panel.png", 
    width = 10*300, height = 10*300, res = 300
)
{
  par(mfrow=c(2,2))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  pt_text_cex = 0.6
  
  #col_age = c("red", "orange", "green", "blue", "black")
  col_age = brewer.pal(7, "YlOrBr")[3:7]    
  lty_sps <- c("F3","dotted","dashed","solid")
  
  
  # Females unconditional
  
  plot(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Female respondents", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  # col_age <- gray(c(0.6, 0.4, 0.2, 0.1, 0.0))

  for(age in 1:5) {
    matplot(yrs, t(df_prop_fem_uncond_by_age[age,,])/100, type='l', lty = lty_sps, 
        col = col_age[age], lwd = my_lwd, add=T)
    matplot(yrs, t(df_prop_fem_uncond_by_age[age,,])/100, type='p',
           col = "#FFFFFF", pch=19, cex=1.5, add=T)
    text(yrs, t(df_prop_fem_uncond_by_age[age,,])/100, age+13, 
                col = col_age[age], cex=pt_text_cex)
  }
  
  # Males unconditional
  
  plot(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Male respondents", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  
  for(age in 1:5) {
    matplot(yrs, t(df_prop_mal_uncond_by_age[age,,])/100, type='l', lty = lty_sps, 
            col = col_age[age], lwd = my_lwd, add=T)
    matplot(yrs, t(df_prop_mal_uncond_by_age[age,,])/100, type='p',
            col = "#FFFFFF", pch=19, cex=1.5, add=T)
    text(yrs, t(df_prop_mal_uncond_by_age[age,,])/100, age+13, 
         col = col_age[age], cex=pt_text_cex)
  }
  
  # Females conditional
  
  plot(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)

  lines(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[1], lwd = my_lwd)
  
  lines(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[2], lwd = my_lwd)
  
  lines(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[3], lwd = my_lwd)
  
  lines(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[4], lwd = my_lwd)
  
  lines(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[5], lwd = my_lwd)
  
  points(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_14yoF$year, df_proportions_14yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_14yoF$year, df_proportions_14yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoF$year, df_proportions_15yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoF$year, df_proportions_15yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoF$year, df_proportions_16yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoF$year, df_proportions_16yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoF$year, df_proportions_17yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoF$year, df_proportions_17yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoF$year, df_proportions_18yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoF$year, df_proportions_18yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  
  text(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  text(df_proportions_14yoF$year, df_proportions_14yoF$males_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  text(df_proportions_14yoF$year, df_proportions_14yoF$both_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  
  text(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  text(df_proportions_15yoF$year, df_proportions_15yoF$males_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  text(df_proportions_15yoF$year, df_proportions_15yoF$both_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  
  text(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  text(df_proportions_16yoF$year, df_proportions_16yoF$males_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  text(df_proportions_16yoF$year, df_proportions_16yoF$both_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  
  text(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  text(df_proportions_17yoF$year, df_proportions_17yoF$males_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  text(df_proportions_17yoF$year, df_proportions_17yoF$both_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  
  text(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  text(df_proportions_18yoF$year, df_proportions_18yoF$males_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  text(df_proportions_18yoF$year, df_proportions_18yoF$both_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  
  
  # Males conditional

  plot(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)

  lines(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[1], lwd = my_lwd)
  
  lines(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[2], lwd = my_lwd)
  
  lines(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[3], lwd = my_lwd)
  
  lines(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[4], lwd = my_lwd)
  
  lines(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[5], lwd = my_lwd)
  
  points(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_14yoM$year, df_proportions_14yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_14yoM$year, df_proportions_14yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoM$year, df_proportions_15yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoM$year, df_proportions_15yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoM$year, df_proportions_16yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoM$year, df_proportions_16yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoM$year, df_proportions_17yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoM$year, df_proportions_17yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoM$year, df_proportions_18yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoM$year, df_proportions_18yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  
  text(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  text(df_proportions_14yoM$year, df_proportions_14yoM$males_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  text(df_proportions_14yoM$year, df_proportions_14yoM$both_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  
  text(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  text(df_proportions_15yoM$year, df_proportions_15yoM$males_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  text(df_proportions_15yoM$year, df_proportions_15yoM$both_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  
  text(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  text(df_proportions_16yoM$year, df_proportions_16yoM$males_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  text(df_proportions_16yoM$year, df_proportions_16yoM$both_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  
  text(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  text(df_proportions_17yoM$year, df_proportions_17yoM$males_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  text(df_proportions_17yoM$year, df_proportions_17yoM$both_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  
  text(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  text(df_proportions_18yoM$year, df_proportions_18yoM$males_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  text(df_proportions_18yoM$year, df_proportions_18yoM$both_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  
   legend("center", c("Never had sex",
                      "Female partners only", 
                      "Male partners only", 
                      "Female and male partners",
                      "",
                      "Age 14", "Age 15", "Age 16", "Age 17", "Age 18"),
          col=c("black", "black", "black", "black", "white", col_age),
          lty = c(lty_sps, "blank", "solid", "solid", "solid", "solid", "solid"), 
          seg.len=5, cex=0.8, bty = "o"
   ) 
}

dev.off()


png("plots/SSP/SSP_by_age_and_year_uncond_panel.png", 
    width = 10*300, height = 4*300, res = 300
)
{
  par(mfrow=c(1,3))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  pt_text_cex = 0.6
  
  #col_age = c("red", "orange", "green", "blue", "black")
  col_age = brewer.pal(7, "YlOrBr")[3:7]    
  lty_sps <- c("F3","dotted","dashed","solid")
  
  
  # Females unconditional
  
  plot(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Female respondents", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  # col_age <- gray(c(0.6, 0.4, 0.2, 0.1, 0.0))
  
  for(age in 1:5) {
    matplot(yrs, t(df_prop_fem_uncond_by_age[age,,])/100, type='l', lty = lty_sps, 
            col = col_age[age], lwd = my_lwd, add=T)
    matplot(yrs, t(df_prop_fem_uncond_by_age[age,,])/100, type='p',
            col = "#FFFFFF", pch=19, cex=1.5, add=T)
    text(yrs, t(df_prop_fem_uncond_by_age[age,,])/100, age+13, 
         col = col_age[age], cex=pt_text_cex)
  }
  
  # Males unconditional
  
  plot(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(4, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 0.7, las = 1)
  mtext("Male respondents", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  
  for(age in 1:5) {
    matplot(yrs, t(df_prop_mal_uncond_by_age[age,,])/100, type='l', lty = lty_sps, 
            col = col_age[age], lwd = my_lwd, add=T)
    matplot(yrs, t(df_prop_mal_uncond_by_age[age,,])/100, type='p',
            col = "#FFFFFF", pch=19, cex=1.5, add=T)
    text(yrs, t(df_prop_mal_uncond_by_age[age,,])/100, age+13, 
         col = col_age[age], cex=pt_text_cex)
  }
  
  
  plot.new() 
  legend("center", c("Never had sex",
                     "Female partners only", 
                     "Male partners only", 
                     "Female and male partners",
                     "",
                     "Age 14", "Age 15", "Age 16", "Age 17", "Age 18"),
         col=c("black", "black", "black", "black", "white", col_age),
         lty = c(lty_sps, "blank", "solid", "solid", "solid", "solid", "solid"), 
         seg.len=5, cex=0.8, bty = "o"
  ) 
  
}

dev.off()







png("plots/SSP/SSP_by_age_and_year_cond_panel.png", 
    width = 10*300, height = 4*300, res = 300
)
{
  par(mfrow=c(1,3))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  pt_text_cex = 0.6
  
  #col_age = c("red", "orange", "green", "blue", "black")
  col_age = brewer.pal(7, "YlOrBr")[3:7]    
  lty_sps <- c("F3","dotted","dashed","solid")
  
  
  # Females conditional
  
  plot(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  
  lines(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[1], lwd = my_lwd)
  
  lines(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[2], lwd = my_lwd)
  
  lines(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[3], lwd = my_lwd)
  
  lines(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[4], lwd = my_lwd)
  
  lines(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, lty = lty_sps[2], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$males_prop/100, lty = lty_sps[3], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$both_prop/100, lty = lty_sps[4], 
        col = col_age[5], lwd = my_lwd)
  
  points(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_14yoF$year, df_proportions_14yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_14yoF$year, df_proportions_14yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoF$year, df_proportions_15yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoF$year, df_proportions_15yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoF$year, df_proportions_16yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoF$year, df_proportions_16yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoF$year, df_proportions_17yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoF$year, df_proportions_17yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoF$year, df_proportions_18yoF$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoF$year, df_proportions_18yoF$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  
  text(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  text(df_proportions_14yoF$year, df_proportions_14yoF$males_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  text(df_proportions_14yoF$year, df_proportions_14yoF$both_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  
  text(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  text(df_proportions_15yoF$year, df_proportions_15yoF$males_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  text(df_proportions_15yoF$year, df_proportions_15yoF$both_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  
  text(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  text(df_proportions_16yoF$year, df_proportions_16yoF$males_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  text(df_proportions_16yoF$year, df_proportions_16yoF$both_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  
  text(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  text(df_proportions_17yoF$year, df_proportions_17yoF$males_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  text(df_proportions_17yoF$year, df_proportions_17yoF$both_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  
  text(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  text(df_proportions_18yoF$year, df_proportions_18yoF$males_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  text(df_proportions_18yoF$year, df_proportions_18yoF$both_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  
  
  # Males conditional
  
  plot(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(4, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 0.7, las = 1)
  mtext("Male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  
  lines(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[1], lwd = my_lwd)
  
  lines(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[2], lwd = my_lwd)
  
  lines(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[3], lwd = my_lwd)
  
  lines(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[4], lwd = my_lwd)
  
  lines(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, lty = lty_sps[2], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$males_prop/100, lty = lty_sps[3], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$both_prop/100, lty = lty_sps[4], 
        col = col_age[5], lwd = my_lwd)
  
  points(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_14yoM$year, df_proportions_14yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_14yoM$year, df_proportions_14yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoM$year, df_proportions_15yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_15yoM$year, df_proportions_15yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoM$year, df_proportions_16yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_16yoM$year, df_proportions_16yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoM$year, df_proportions_17yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_17yoM$year, df_proportions_17yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoM$year, df_proportions_18yoM$males_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  points(df_proportions_18yoM$year, df_proportions_18yoM$both_prop/100, 
         col = "#FFFFFF", pch=19, cex=1.5)
  
  text(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  text(df_proportions_14yoM$year, df_proportions_14yoM$males_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  text(df_proportions_14yoM$year, df_proportions_14yoM$both_prop/100, "14", 
       col = col_age[1], cex=pt_text_cex)
  
  text(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  text(df_proportions_15yoM$year, df_proportions_15yoM$males_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  text(df_proportions_15yoM$year, df_proportions_15yoM$both_prop/100, "15", 
       col = col_age[2], cex=pt_text_cex)
  
  text(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  text(df_proportions_16yoM$year, df_proportions_16yoM$males_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  text(df_proportions_16yoM$year, df_proportions_16yoM$both_prop/100, "16", 
       col = col_age[3], cex=pt_text_cex)
  
  text(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  text(df_proportions_17yoM$year, df_proportions_17yoM$males_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  text(df_proportions_17yoM$year, df_proportions_17yoM$both_prop/100, "17", 
       col = col_age[4], cex=pt_text_cex)
  
  text(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  text(df_proportions_18yoM$year, df_proportions_18yoM$males_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  text(df_proportions_18yoM$year, df_proportions_18yoM$both_prop/100, "18", 
       col = col_age[5], cex=pt_text_cex)
  
  plot.new()
  legend("center", c("Never had sex",
                     "Female partners only", 
                     "Male partners only", 
                     "Female and male partners",
                     "",
                     "Age 14", "Age 15", "Age 16", "Age 17", "Age 18"),
         col=c("black", "black", "black", "black", "white", col_age),
         lty = c(lty_sps, "blank", "solid", "solid", "solid", "solid", "solid"), 
         seg.len=5, cex=0.8, bty = "o"
  ) 
}

dev.off()

