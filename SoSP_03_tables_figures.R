### Script for SoSP tables and figures

# This script's code uses the YRBS dataset, previously prepared in script "SoSP_01_prepare" 
# and analyzed in script "SoSP_02_analyses"

#All information used in the tables and figures in the manuscript can be found in this script

##############################################################################################
#######################################
########  PLOTS #######################
#######################################
##############################################################################################

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

####

###############################################################################################
###############################################################################################
# FIGURE 1. Proportion of respondents by sex of past sexual contacts
#+++++++++++++++++++++++++++++++++++++++++
#####
png("plots/SoSP/SSP_by_sex_and_year.png", 
    width = 4*300, height = 6*300, res = 300
)
{
  par(mfrow=c(2,2))
  par(mar=c(2,0,3,0))
  par(oma=c(2,5,0,1))
  my_lwd = 2.5
  my_xlim = c(2014.5, 2021.5)
  
  
  #Calculate the N's for each year to populate Fig. 1
  Ns_all_fem <- with(yrbs_final, tapply(sex == "Female", year, sum, na.rm = TRUE))
  Ns_all_mal <- with(yrbs_final, tapply(sex == "Male", year, sum, na.rm = TRUE))
  
  Ns_cond_fem <- with(yrbs_final, tapply(sex == "Female" & sex_of_sps != "1_never", year, sum, na.rm = TRUE))
  Ns_cond_mal <- with(yrbs_final, tapply(sex == "Male" & sex_of_sps != "1_never", year, sum, na.rm = TRUE))
  
  #Also have the Ns for each year appear in the console
  Ns_list <- list(
    Ns_all_fem = Ns_all_fem,
    Ns_all_mal = Ns_all_mal,
    Ns_cond_fem = Ns_cond_fem,
    Ns_cond_mal = Ns_cond_mal
  )
  
  sapply(names(Ns_list), function(name) {
    cat(name, ":\n")
    print(Ns_list[[name]])
    cat("\n")
  })
  
  
  
  
  plot(df_proportions_fem$year, df_proportions_fem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("All female respondents", 3, cex = 0.7, line =0.3)
  mtext("Prop. of respondents by SoSP", 2, cex = 0.7, line = 3, outer = TRUE)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=0.7)
  lines(df_proportions_fem$year, df_proportions_fem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_fem$year, df_proportions_fem$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_fem$year, df_proportions_fem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_fem$year, df_proportions_fem$none_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  
  mtext(paste0("n=", Ns_all_fem), side = 1, line = 1, at=df_proportions_fem$year, cex=0.40)
  
  text(2021.3, 0.55, "*")
  text(2021.3, 0.33, "*", col = "#E6A820")
  text(2021.3, 0.10, "*", col = "#37C817")
  text(2021.3, 0.03, "*", col = "#31BAF6")
  
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
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_mal$year, df_proportions_mal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_mal$year, df_proportions_mal$none_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  mtext(paste0("n=", Ns_all_mal), side = 1, line = 1, at=df_proportions_mal$year, cex=0.40)
  
  text(2021.3, 0.57, "*")
  text(2021.3, 0.40, "*", col = "#31BAF6")
  
  legend(2016, 1, c("None", "Female partners only", 
                    "Male partners only", 
                    "Female and male partners"),
         col=c("#000000", "#31BAF6", "#E6A820", "#37C817"), 
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
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_fem2$year, df_proportions_fem2$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  mtext(paste0("n=", Ns_cond_fem), side = 1, line = 1, at=df_proportions_fem2$year, cex=0.40)
  
  text(2021.3, 0.72, "*", col = "#E6A820")
  text(2021.3, 0.21, "*", col = "#37C817")
  text(2021.3, 0.07, "*", col = "#31BAF6")
  
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
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_mal2$year, df_proportions_mal2$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  mtext(paste0("n=", Ns_cond_mal), side = 1, line = 1, at=df_proportions_mal2$year, cex=0.40)
  
  text(2021.3, 0.04, "*", col = "#E6A820")
}
dev.off()



###############################################################################################
# FIGURE 2. Proportion of respondents sexual identity by year, split by sex
#+++++++++++++++++++++++++++++++++++++++++
#####
png("plots/SoSP/SO_by_year_two_panel.png", 
    width = 6*300, height = 3.5*300, res = 300
)
{
  
  par(mfrow=c(1,2))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 2.5
  my_xlim = c(2014.5, 2021.5)
  #pt_text_cex = 0.6
  
  
  plot(df_proportions_fem_so$year, df_proportions_fem_so$straight_prop/100, type = "l", 
       col = "pink", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.7)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Females", 3, cex = 0.7, line =0.3)
  mtext("Prop. of adolescents of a \ngiven sexual identity by year", 2, cex = 0.7, line = 3, outer = TRUE)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=0.7)
  lines(df_proportions_fem_so$year, df_proportions_fem_so$lesgay_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_fem_so$year, df_proportions_fem_so$bi_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_fem_so$year, df_proportions_fem_so$dko_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_fem_so$year, df_proportions_fem_so$ref_prop/100, lty = 5, 
        col = "red", lwd = my_lwd)
  
  text(2021.3, 0.61, "*", col = "pink")
  text(2021.3, 0.04, "*", col = "#31BAF6")
  text(2021.3, 0.19, "*", col = "#E6A820")
  text(2021.3, 0.15, "*", col = "#37C817")
  
  plot(df_proportions_mal_so$year, df_proportions_mal_so$straight_prop/100, type = "l", 
       col = "pink", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.7)
  #axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=1.2, las=1)
  mtext("Males", 3, cex = 0.7, line =0.3)
  #mtext("Prop. of males of a given sexual identity by year", 2, cex = 1, line = 3, outer = TRUE)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=0.7)
  lines(df_proportions_mal_so$year, df_proportions_mal_so$lesgay_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_mal_so$year, df_proportions_mal_so$bi_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_mal_so$year, df_proportions_mal_so$dko_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_mal_so$year, df_proportions_mal_so$ref_prop/100, lty = 5, 
        col = "red", lwd = my_lwd)
  
  text(2021.3, 0.86, "*", col = "pink")
  text(2021.3, 0.05, "*", col = "#E6A820")
  text(2021.3, 0.07, "*", col = "#37C817")
  
  
  
  #plot.new()
  legend(2018, 0.65, 
         c("Straight",
           "Gay/lesbian", 
           "Bisexual", 
           "Not sure",
           "Declined to respond"),
         col=c("pink", "#31BAF6", "#E6A820", "#37C817", "red"), 
         lty = 1:5, cex=0.5, bty = "o"
  )
  
}

dev.off()


###############################################################################################

###############################################################################################
# FIGURE 3: Proportion of female respondents by sex of past sexual contacts, 
# including those with no prior sexual contact, broken out by reported sexual identity
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SoSP/SSP_by_SO_and_year_fem_uncond.png", 
    width = 14*300, height = 4*300, res = 300
)
{
  par(mfrow=c(1,5))
  par(mar=c(2,0,3,0))
  par(oma=c(0,9,0,1))
  my_lwd = 2.5
  my_xlim = c(2014.5, 2021.5)
  
  lty_sps <- c("F3","dotted","dashed","solid")
  
  plot(df_proportions_strfem_unc$year, df_proportions_strfem_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=1.2, las=1)
  mtext("Straight", 3, cex = 1.2, line =0.3)
  mtext("Prop. of female \nrespondents by SoSP", 2, cex = 1, line = 4, outer = TRUE)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=1.2)
  lines(df_proportions_strfem_unc$year, df_proportions_strfem_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_strfem_unc$year, df_proportions_strfem_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_strfem_unc$year, df_proportions_strfem_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_strfem_unc$year, df_proportions_strfem_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  
  plot(df_proportions_lesfem_unc$year, df_proportions_lesfem_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Lesbian", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=1.2)
  lines(df_proportions_lesfem_unc$year, df_proportions_lesfem_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_lesfem_unc$year, df_proportions_lesfem_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_lesfem_unc$year, df_proportions_lesfem_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_lesfem_unc$year, df_proportions_lesfem_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  
  
  plot(df_proportions_bifem_unc$year, df_proportions_bifem_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Bisexual", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "C", cex=1.2)
  lines(df_proportions_bifem_unc$year, df_proportions_bifem_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_bifem_unc$year, df_proportions_bifem_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_bifem_unc$year, df_proportions_bifem_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_bifem_unc$year, df_proportions_bifem_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  legend(2015.1, 1, c("Never had sex",
                      "Female partners only", 
                      "Male partners only", 
                      "Female and male partners"),
         col=c("black", "#31BAF6", "#E6A820", "#37C817"), 
         lty = 1:4, cex=1.2, bty = "o"
  )
  
  
  text(2021.3, 0.23, "*", col = "#E6A820", cex=1.8)
  text(2021.3, 0.31, "*", col = "#37C817", cex=1.8)
  
  
  
  plot(df_proportions_dkofem_unc$year, df_proportions_dkofem_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Not sure", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "D", cex=1.2)
  lines(df_proportions_dkofem_unc$year, df_proportions_dkofem_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_dkofem_unc$year, df_proportions_dkofem_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_dkofem_unc$year, df_proportions_dkofem_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_dkofem_unc$year, df_proportions_dkofem_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  text(2021.3, 0.62, "*", col = "black", cex=1.8)
  text(2021.3, 0.19, "*", col = "#E6A820", cex=1.8)
  
  
  plot(df_proportions_reffem_unc$year, df_proportions_reffem_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Declined to answer", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "E", cex=1.2)
  lines(df_proportions_reffem_unc$year, df_proportions_reffem_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_reffem_unc$year, df_proportions_reffem_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_reffem_unc$year, df_proportions_reffem_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_reffem_unc$year, df_proportions_reffem_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  
  text(2021.3, 0.65, "*", col = "black", cex=1.8)
  text(2021.3, 0.22, "*", col = "#E6A820", cex=1.8)
  
  
}
dev.off()

###############################################################################################
# FIGURE 4: Proportion of male respondents by sex of past sexual contacts, 
# including those with no prior sexual contact, broken out by reported sexual identity
#+++++++++++++++++++++++++++++++++++++++++
#####


png("plots/SoSP/SSP_by_SO_and_year_mal_uncond.png", 
    width = 14*300, height = 4*300, res = 300
)
{
  par(mfrow=c(1,5))
  par(mar=c(2,0,3,0))
  par(oma=c(0,9,0,1))
  my_lwd = 2.5
  my_xlim = c(2014.5, 2021.5)
  
  lty_sps <- c("F3","dotted","dashed","solid")
  
  
  plot(df_proportions_strmal_unc$year, df_proportions_strmal_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=1.2, las=1)
  mtext("Straight", 3, cex = 1.2, line =0.3)
  mtext("Prop. of male \nrespondents by SoSP", 2, cex = 1, line = 3, outer = TRUE)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=1.2)
  lines(df_proportions_strmal_unc$year, df_proportions_strmal_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_strmal_unc$year, df_proportions_strmal_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_strmal_unc$year, df_proportions_strmal_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_strmal_unc$year, df_proportions_strmal_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  text(2021.3, 0.58, "*", col = "black", cex=1.8)
  text(2021.3, 0.42, "*", col = "#31BAF6", cex=1.8)
  
  
  plot(df_proportions_gaymal_unc$year, df_proportions_gaymal_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Gay", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=1.2)
  lines(df_proportions_gaymal_unc$year, df_proportions_gaymal_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_gaymal_unc$year, df_proportions_gaymal_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_gaymal_unc$year, df_proportions_gaymal_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_gaymal_unc$year, df_proportions_gaymal_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  text(2021.3, 0.02, "*", col = "#31BAF6", cex=1.8) 
  
  plot(df_proportions_bimal_unc$year, df_proportions_bimal_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Bisexual", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "C", cex=1.2)
  lines(df_proportions_bimal_unc$year, df_proportions_bimal_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_bimal_unc$year, df_proportions_bimal_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_bimal_unc$year, df_proportions_bimal_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_bimal_unc$year, df_proportions_bimal_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  legend(2015.1, 1, c("Never had sex",
                      "Female partners only", 
                      "Male partners only", 
                      "Female and male partners"),
         col=c("black", "#31BAF6", "#E6A820", "#37C817"), 
         lty = 1:4, cex=1.2, bty = "o"
  )
  
  plot(df_proportions_dkomal_unc$year, df_proportions_dkomal_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Not sure", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "D", cex=1.2)
  lines(df_proportions_dkomal_unc$year, df_proportions_dkomal_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_dkomal_unc$year, df_proportions_dkomal_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_dkomal_unc$year, df_proportions_dkomal_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_dkomal_unc$year, df_proportions_dkomal_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  
  text(2021.3, 0.11, "*", col = "#37C817", cex=1.8)
  
  
  plot(df_proportions_refmal_unc$year, df_proportions_refmal_unc$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Declined to answer", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "E", cex=1.2)
  lines(df_proportions_refmal_unc$year, df_proportions_refmal_unc$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_refmal_unc$year, df_proportions_refmal_unc$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_refmal_unc$year, df_proportions_refmal_unc$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  lines(df_proportions_refmal_unc$year, df_proportions_refmal_unc$never_prop/100, lty = 1, 
        col = "black", lwd = my_lwd)
  
  text(2021.3, 0.85, "*", col = "#31BAF6")
  text(2021.3, 0.00, "*", col = "#E6A820")
  
  
  
  
}

dev.off()




###############################################################################################

# FIGURE 5: Proportion of respondents by sex of past sexual contacts, by respondent age
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SoSP/SSP_by_age_and_year_uncond_panel.png", 
    width = 9*300, height = 3.5*300, res = 300
)
{
  par(mfrow=c(1,3))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  pt_text_cex = 0.8
  
  #col_age = c("red", "orange", "green", "blue", "black")
  col_age = brewer.pal(7, "YlOrBr")[3:7]    
  lty_sps <- c("F3","dotted","dashed","solid")
  
  
  # Females unconditional
  
  plot(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.7)
  mtext("Prop. of respondents by SoSP", 2, cex = 0.9, line = 3, outer = TRUE)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=1.2, las=1)
  mtext("Female respondents", 3, cex = 0.9, line =0.3)
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
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.7)
  #axis(4, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 0.7, las = 1)
  mtext("Male respondents", 3, cex = 0.9, line =0.3)
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
                     "Age 14", "Age 15", "Age 16", "Age 17", "Age 18+"),
         col=c("black", "black", "black", "black", "white", col_age),
         lty = c(lty_sps, "blank", "solid", "solid", "solid", "solid", "solid"), 
         seg.len=5, cex=0.8, bty = "o"
  ) 
  
}

dev.off()



##############################################################
# FIGURE S1: Proportion of female respondents by sex of past sexual contacts, 
# among those who have ever had sexual contact, broken out by reported sexual identity 
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SoSP/SSP_by_SO_and_year_fem.png", 
    width = 14*300, height = 4*300, res = 300
)
{
  par(mfrow=c(1,5))
  par(mar=c(2,0,3,0))
  par(oma=c(2,8,0,3))
  my_lwd = 2.5
  my_xlim = c(2014.5, 2021.5)
  lty_sps <- c("F3","dotted","dashed")
  
  plot(df_proportions_strfem$year, df_proportions_strfem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.0)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=1.2, las=1)
  mtext("Straight", 3, cex = 1.2, line =0.3)
  mtext("Prop. of females by SoSP, among \nthose who have ever had sex", 2, cex = 1.0, line = 4, outer = TRUE)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=1.0)
  lines(df_proportions_strfem$year, df_proportions_strfem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_strfem$year, df_proportions_strfem$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_strfem$year, df_proportions_strfem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  plot(df_proportions_lesfem$year, df_proportions_lesfem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.0)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Lesbian", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=1.0)
  lines(df_proportions_lesfem$year, df_proportions_lesfem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_lesfem$year, df_proportions_lesfem$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_lesfem$year, df_proportions_lesfem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  
  plot(df_proportions_bifem$year, df_proportions_bifem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.0)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Bisexual", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "C", cex=1.0)
  lines(df_proportions_bifem$year, df_proportions_bifem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_bifem$year, df_proportions_bifem$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_bifem$year, df_proportions_bifem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  legend(2015.1, 1, c("Female partners only", 
                      "Male partners only", 
                      "Female and male partners"),
         col=c("#31BAF6", "#E6A820", "#37C817"), 
         lty = 2:4, cex=1.2, bty = "o"
  )
  
  text(2021.3, 0.53, "*", col = "#37C817", cex=1.8)
  text(2021.3, 0.40, "*", col = "#E6A820", cex=1.8)
  
  
  plot(df_proportions_dkofem$year, df_proportions_dkofem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.0)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Not sure", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "D", cex=1.0)
  lines(df_proportions_dkofem$year, df_proportions_dkofem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_dkofem$year, df_proportions_dkofem$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_dkofem$year, df_proportions_dkofem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  text(2021.3, 0.51, "*", col = "#E6A820", cex=1.8)
  text(2021.3, 0.39, "*", col = "#37C817", cex=1.8)
  
  
  plot(df_proportions_reffem$year, df_proportions_reffem$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.0)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Declined to answer", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "E", cex=1.0)
  lines(df_proportions_reffem$year, df_proportions_reffem$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_reffem$year, df_proportions_reffem$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_reffem$year, df_proportions_reffem$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
}
dev.off()

###############################################################################################

# FIGURE S2: PLOTS BY SO AND YEAR FOR MALES (CONDITIONED ON EHHS)
# tables 10-14
#Straight male, gay male, bisexual male, don't know male, declined to answer male
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SoSP/SSP_by_SO_and_year_mal.png", 
    width = 14*300, height = 4*300, res = 300
)
{
  par(mfrow=c(1,5))
  par(mar=c(2,0,3,0))
  par(oma=c(2,8,0,3))
  my_lwd = 2.5
  my_xlim = c(2014.5, 2021.5)
  lty_sps <- c("F3","dotted","dashed")
  
  plot(df_proportions_strmal$year, df_proportions_strmal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=1.2, las=1)
  mtext("Straight", 3, cex = 1.2, line =0.3)
  mtext("Prop. of males by SoSP, among \nthose who have ever had sex", 2, cex = 1, line = 4, outer = TRUE)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "A", cex=1.0)
  lines(df_proportions_strmal$year, df_proportions_strmal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_strmal$year, df_proportions_strmal$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_strmal$year, df_proportions_strmal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  cex=0.8  
  plot(df_proportions_gaymal$year, df_proportions_gaymal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Gay", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "B", cex=1.0)
  lines(df_proportions_gaymal$year, df_proportions_gaymal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_gaymal$year, df_proportions_gaymal$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_gaymal$year, df_proportions_gaymal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  
  
  plot(df_proportions_bimal$year, df_proportions_bimal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Bisexual", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "C", cex=1.0)
  lines(df_proportions_bimal$year, df_proportions_bimal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_bimal$year, df_proportions_bimal$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_bimal$year, df_proportions_bimal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  legend(2015.1, 1, c("Female partners only", 
                      "Male partners only", 
                      "Female and male partners"),
         col=c("#31BAF6", "#E6A820", "#37C817"), 
         lty = 2:4, cex=1.2, bty = "o"
  )
  
  plot(df_proportions_dkomal$year, df_proportions_dkomal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Not sure", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "D", cex=1.0)
  lines(df_proportions_dkomal$year, df_proportions_dkomal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_dkomal$year, df_proportions_dkomal$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_dkomal$year, df_proportions_dkomal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  text(2021.3, 0.60, "*", col = "#31BAF6", cex=1.8)
  text(2021.3, 0.30, "*", col = "#37C817", cex=1.8)
  
  
  plot(df_proportions_refmal$year, df_proportions_refmal$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=1.2)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.8)
  mtext("Declined to answer", 3, cex = 1.2, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  text(2014.6, 0.99, "E", cex=1.0)
  lines(df_proportions_refmal$year, df_proportions_refmal$females_prop/100, lty = 2, 
        col = "#31BAF6", lwd = my_lwd)
  lines(df_proportions_refmal$year, df_proportions_refmal$males_prop/100, lty = 3, 
        col = "#E6A820", lwd = my_lwd)
  lines(df_proportions_refmal$year, df_proportions_refmal$both_prop/100, lty = 4, 
        col = "#37C817", lwd = my_lwd)
  
  text(2021.3, 0.85, "*", col = "#31BAF6", cex=1.8)
  text(2021.3, 0.00, "*", col = "#E6A820", cex=1.8)
  
  
}

dev.off()


###############################################################################################

# FIGURE S3: Proportion of respondents by sex of past sexual contacts, 
# by respondent age, among those who have ever had sexual contact
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SoSP/SSP_by_age_and_year_cond_panel.png", 
    width = 6*300, height = 3.5*300, res = 300
)
{
  par(mfrow=c(1,2))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 2
  my_xlim = c(2014.5, 2021.5)
  pt_text_cex = 0.5
  
  col_age = brewer.pal(7, "YlOrBr")[3:7]    
  lty_sps <- c("dotted","dashed","solid")
  
  # Females conditional
  
  plot(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.7)
  mtext("Prop. of respondents by SoSP,\namong those who have ever had sex", 2, cex = 0.7, line = 3, outer = TRUE)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  
  lines(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, lty = lty_sps[1], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$males_prop/100, lty = lty_sps[2], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoF$year, df_proportions_14yoF$both_prop/100, lty = lty_sps[3], 
        col = col_age[1], lwd = my_lwd)
  
  lines(df_proportions_15yoF$year, df_proportions_15yoF$females_prop/100, lty = lty_sps[1], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$males_prop/100, lty = lty_sps[2], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoF$year, df_proportions_15yoF$both_prop/100, lty = lty_sps[3], 
        col = col_age[2], lwd = my_lwd)
  
  lines(df_proportions_16yoF$year, df_proportions_16yoF$females_prop/100, lty = lty_sps[1], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$males_prop/100, lty = lty_sps[2], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoF$year, df_proportions_16yoF$both_prop/100, lty = lty_sps[3], 
        col = col_age[3], lwd = my_lwd)
  
  lines(df_proportions_17yoF$year, df_proportions_17yoF$females_prop/100, lty = lty_sps[1], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$males_prop/100, lty = lty_sps[2], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoF$year, df_proportions_17yoF$both_prop/100, lty = lty_sps[3], 
        col = col_age[4], lwd = my_lwd)
  
  lines(df_proportions_18yoF$year, df_proportions_18yoF$females_prop/100, lty = lty_sps[1], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$males_prop/100, lty = lty_sps[2], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoF$year, df_proportions_18yoF$both_prop/100, lty = lty_sps[3], 
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
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.7)
  #axis(4, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 0.7, las = 1)
  mtext("Male respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
  
  lines(df_proportions_14yoM$year, df_proportions_14yoM$females_prop/100, lty = lty_sps[1], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$males_prop/100, lty = lty_sps[2], 
        col = col_age[1], lwd = my_lwd)
  lines(df_proportions_14yoM$year, df_proportions_14yoM$both_prop/100, lty = lty_sps[3], 
        col = col_age[1], lwd = my_lwd)
  
  lines(df_proportions_15yoM$year, df_proportions_15yoM$females_prop/100, lty = lty_sps[1], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$males_prop/100, lty = lty_sps[2], 
        col = col_age[2], lwd = my_lwd)
  lines(df_proportions_15yoM$year, df_proportions_15yoM$both_prop/100, lty = lty_sps[3], 
        col = col_age[2], lwd = my_lwd)
  
  lines(df_proportions_16yoM$year, df_proportions_16yoM$females_prop/100, lty = lty_sps[1], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$males_prop/100, lty = lty_sps[2], 
        col = col_age[3], lwd = my_lwd)
  lines(df_proportions_16yoM$year, df_proportions_16yoM$both_prop/100, lty = lty_sps[3], 
        col = col_age[3], lwd = my_lwd)
  
  lines(df_proportions_17yoM$year, df_proportions_17yoM$females_prop/100, lty = lty_sps[1], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$males_prop/100, lty = lty_sps[2], 
        col = col_age[4], lwd = my_lwd)
  lines(df_proportions_17yoM$year, df_proportions_17yoM$both_prop/100, lty = lty_sps[3], 
        col = col_age[4], lwd = my_lwd)
  
  lines(df_proportions_18yoM$year, df_proportions_18yoM$females_prop/100, lty = lty_sps[1], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$males_prop/100, lty = lty_sps[2], 
        col = col_age[5], lwd = my_lwd)
  lines(df_proportions_18yoM$year, df_proportions_18yoM$both_prop/100, lty = lty_sps[3], 
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
  
  legend(2017, 0.7, c("Female partners only", 
                      "Male partners only", 
                      "Female and male partners",
                      "",
                      "Age 14", "Age 15", "Age 16", "Age 17", "Age 18+"),
         col=c("black", "black", "black", "white", col_age),
         lty = c(lty_sps, "blank", "solid", "solid", "solid", "solid", "solid"), 
         seg.len=3, cex=0.5, bty = "o"
  ) 
}

dev.off()

##############################################################################################
#######################################
######## TABLES #######################
#######################################
##############################################################################################

#Table S1: Predictors of missingness in sex of sex partners (SoSP) over the lifetime, 
# Youth Risk Behavior Survey (YRBS), 2015-2021. 5


#Logistic regression predicting missing SSP from sex, age,
#sexual identity, and year of survey

#each set as a categorical variable
#reference cats = female, 14, hetero, 2015

class(ssp_predictor_data_final$sex)
class(ssp_predictor_data_final$age)
class(ssp_predictor_data_final$so_new)
class(ssp_predictor_data_final$year)
class(ssp_predictor_data_final$sex_of_sps)
class(ssp_predictor_data_final$missing_ssp)


table(ssp_predictor_data_final$sex)
table(ssp_predictor_data_final$age)
table(ssp_predictor_data_final$so_new)
table(ssp_predictor_data_final$year)



# logistic regression model predicting missing_ssp
logistic_model <- glm(missing_ssp ~ sex + age + so_new + year, 
                      data = ssp_predictor_data_final, 
                      family = binomial)

# Summarize the model
summary(logistic_model)


# Calculate odds ratios and 95% CIs
odds_ratios <- exp(coef(logistic_model))

conf_int <- confint(logistic_model)
conf_int_odds_ratios <- exp(conf_int)

odds_ratios_df <- data.frame(
  Estimate = odds_ratios,
  Lower_95_CI = conf_int_odds_ratios[, 1],
  Upper_95_CI = conf_int_odds_ratios[, 2]
)

print(odds_ratios_df)


############################################################################################
#Table S2: Proportion of respondents by sex of sex partners (SSP) over the lifetime, 
#Youth Risk Behavior Survey (YRBS), 2015-2021	

#frequency of each SSP by year, by sex (includes 1_never)

#Females
print(df_proportions_fem)
#p values
print(multiCA_szabo_tbl_F)

# Males
print(df_proportions_mal)
#p values 
print(multiCA_szabo_tbl_M)

#frequency of each SSP by year, by sex (does not includes 1_never)

# Females
print(df_proportions_fem2)
#p values
print(multiCA_szabo_tbl_F2)

# Males
print(df_proportions_mal2)
#p values 
print(multiCA_szabo_tbl_M2)

############################################################################################
#Table S3: Proportion of respondents by self-reported sexual identity (SI), 
#Youth Risk Behavior Survey (YRBS), 2015-2021	

#frequency of sexual identity across years, by sex

#Females
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
multiCA_so_fem_year

#Males
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
multiCA_so_mal_year
############################################################################################
#Table S4: Sensitivity analysis on trend tests for sexual identity (SI), 
#Youth Risk Behavior Survey (YRBS), 2015-2021, with alternative definition for 
#“not sure” in 2021
#################################################################################

##Sensitivity analysis##

#redo the analysis in Figure 2/ Table S2 in two ways for sens analysis:

#Table S2: Proportion of respondents by self-reported sexual identity (SI), 
#Youth Risk Behavior Survey (YRBS), 2015-2021	

#frequency of sexual identity across years, by sex

yrbs_sens_subset1 <- subset(yrbs_final, so_21 != "7_somethingelse" | is.na(so_21))

yrbs_sens_subset2 <- subset(yrbs_sens_subset1, so_21 != "6_dkwtm" | is.na(so_21))

table(yrbs_merge_new$so_21)
table(yrbs_sens_subset1$so_21)
table(yrbs_sens_subset2$so_21)


yrbs_sens_subset1_fem <- subset(yrbs_sens_subset1, sex == "Female")
yrbs_sens_subset1_mal <- subset(yrbs_sens_subset1, sex == "Male")
yrbs_sens_subset2_fem <- subset(yrbs_sens_subset2, sex == "Female")
yrbs_sens_subset2_mal <- subset(yrbs_sens_subset2, sex == "Male")

#Sensitivity analysis 1- Take the people who responded “I describe my sexual 
#identity some other way” in 2021, and remove them altogether


df_proportions_fem_so_sens1 <- yrbs_sens_subset1_fem %>%
  group_by(year) %>%
  summarise(
    N = n(),
    straight_prop = sum(so_new == "1_straight") / n() * 100,
    lesgay_prop = sum(so_new == "2_lesgay") / n() * 100,
    bi_prop = sum(so_new == "3_bi") / n() * 100,
    dko_prop = sum(so_new == "4_dko") / n() * 100,
    ref_prop = sum(so_new == "5_ref") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_fem_so_sens1)


szabo_table_fem_sens1 <- table(yrbs_sens_subset1_fem$so_new, yrbs_sens_subset1_fem$year)
prop.table(szabo_table_fem_sens1)

## using formula interface
multiCA_szabo_tbl_fem_sens1 <- multiCA.test(so_new ~ year, data=yrbs_sens_subset1_fem)
#p values
print(multiCA_szabo_tbl_fem_sens1)



df_proportions_mal_so_sens1 <- yrbs_sens_subset1_mal %>%
  group_by(year) %>%
  summarise(
    N = n(),
    straight_prop = sum(so_new == "1_straight") / n() * 100,
    lesgay_prop = sum(so_new == "2_lesgay") / n() * 100,
    bi_prop = sum(so_new == "3_bi") / n() * 100,
    dko_prop = sum(so_new == "4_dko") / n() * 100,
    ref_prop = sum(so_new == "5_ref") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_mal_so_sens1)


szabo_table_mal_sens1 <- table(yrbs_sens_subset1_mal$so_new, yrbs_sens_subset1_mal$year)
prop.table(szabo_table_mal_sens1)

## using formula interface
multiCA_szabo_tbl_mal_sens1 <- multiCA.test(so_new ~ year, data=yrbs_sens_subset1_mal)
#p values
print(multiCA_szabo_tbl_mal_sens1)



#2.  Take the people who responded “I describe my sexual identity some other way” 
#or “I do not know what this question is asking” in 2021, and remove them.

df_proportions_fem_so_sens2 <- yrbs_sens_subset2_fem %>%
  group_by(year) %>%
  summarise(
    straight_prop = sum(so_new == "1_straight") / n() * 100,
    lesgay_prop = sum(so_new == "2_lesgay") / n() * 100,
    bi_prop = sum(so_new == "3_bi") / n() * 100,
    dko_prop = sum(so_new == "4_dko") / n() * 100,
    ref_prop = sum(so_new == "5_ref") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_fem_so_sens2)


szabo_table_fem_sens2 <- table(yrbs_sens_subset2_fem$so_new, yrbs_sens_subset2_fem$year)

## using formula interface
multiCA_szabo_tbl_fem_sens2 <- multiCA.test(so_new ~ year, data=yrbs_sens_subset2_fem)
# p values
print(multiCA_szabo_tbl_fem_sens2)



df_proportions_mal_so_sens2 <- yrbs_sens_subset2_mal %>%
  group_by(year) %>%
  summarise(
    straight_prop = sum(so_new == "1_straight") / n() * 100,
    lesgay_prop = sum(so_new == "2_lesgay") / n() * 100,
    bi_prop = sum(so_new == "3_bi") / n() * 100,
    dko_prop = sum(so_new == "4_dko") / n() * 100,
    ref_prop = sum(so_new == "5_ref") / n() * 100
  )

# Print the resulting data frame with proportions
print(df_proportions_mal_so_sens2)


szabo_table_mal_sens2 <- table(yrbs_sens_subset2_mal$so_new, yrbs_sens_subset2_mal$year)

## using formula interface
multiCA_szabo_tbl_mal_sens2 <- multiCA.test(so_new ~ year, data=yrbs_sens_subset2_mal)
#p values
print(multiCA_szabo_tbl_mal_sens2)


##############################################################################################
#Table S5: Proportion of respondents by sex of sex of sex of sex partners (SSP) over the lifetime, 
#by reported sexual identity (SI), including those with no prior sexual contact, Youth Risk 
#Behavior Survey (YRBS), 2015-2021	

##(frequency by year by sexual identity, unconditional)

#straight female
print(df_proportions_strfem_unc)
#p values
print(multiCA_szabo_tbl_strfem_unc)

#lesbian female
print(df_proportions_lesfem_unc)
#p values
print(multiCA_szabo_tbl_lesfem_unc)

#bisexual female
print(df_proportions_bifem_unc)
#p values
print(multiCA_szabo_tbl_bifem_unc)

#don't know female
print(df_proportions_dkofem_unc)
#p values
print(multiCA_szabo_tbl_dkofem_unc)

#refused female
print(df_proportions_reffem_unc)
#p values
print(multiCA_szabo_tbl_reffem_unc)


#straight male
print(df_proportions_strmal_unc)
#p values
print(multiCA_szabo_tbl_strmal_unc)

#gay male
print(df_proportions_gaymal_unc)
#p values
print(multiCA_szabo_tbl_gaymal_unc)

#bisexual male
print(df_proportions_bimal_unc)
#p values
print(multiCA_szabo_tbl_bimal_unc)

#don't know male
print(df_proportions_dkomal_unc)
#p values
print(multiCA_szabo_tbl_dkomal_unc)

#refused male
print(df_proportions_refmal_unc)
#p values
print(multiCA_szabo_tbl_refmal_unc)

##############################################################################################
#Table S6: Proportion of respondents by sex of sex of sex partners (SSP) over the lifetime, 
#by reported sexual identity (SI), among those who have ever had sex, Youth Risk Behavior 
#Survey (YRBS), 2015-2021	

##(frequency by year by sexual identity, conditional)

#straight female
print(df_proportions_strfem)
#p values
print(multiCA_szabo_tbl_strfem)

#lesbian female
print(df_proportions_lesfem)
#p values
print(multiCA_szabo_tbl_lesfem)

#bisexual female
print(df_proportions_bifem)
#p values
print(multiCA_szabo_tbl_bifem)

#dont know female
print(df_proportions_dkofem)
#p values
print(multiCA_szabo_tbl_dkofem)

#declined female
print(df_proportions_reffem)
#p values
print(multiCA_szabo_tbl_reffem)

#straight male
print(df_proportions_strmal)
#p values
print(multiCA_szabo_tbl_strmal)


#gay male
print(df_proportions_gaymal)
#p values
print(multiCA_szabo_tbl_gaymal)

#bisexual male
print(df_proportions_bimal)
#p values
print(multiCA_szabo_tbl_bimal)

#don't know male
print(df_proportions_dkomal)
#p values
print(multiCA_szabo_tbl_dkomal)

#declined to answer male
print(df_proportions_refmal)
#p values
print(multiCA_szabo_tbl_refmal)

################################################################################################
#Table S7: Proportion of respondents by sex of sex of sex partners (SSP) over the lifetime, 
#by age, including those with no prior sexual contact, Youth Risk Behavior Survey 
#(YRBS), 2015-2021

#14yo female
print(df_proportions_14yoF_unc)
#p values
print(multiCA_szabo_tbl_14yoF_unc)

#15yo female
print(df_proportions_15yoF_unc)
#p values
print(multiCA_szabo_tbl_15yoF_unc)

#16yo female
print(df_proportions_16yoF_unc)
#p values
print(multiCA_szabo_tbl_16yoF_unc)

#17yo female
print(df_proportions_17yoF_unc)
#p values
print(multiCA_szabo_tbl_17yoF_unc)

#18yo female
print(df_proportions_18yoF_unc)
#p values
print(multiCA_szabo_tbl_18yoF_unc)


#14yo male
print(df_proportions_14yoM_unc)
#p values
print(multiCA_szabo_tbl_14yoM_unc)

#15yo male
print(df_proportions_15yoM_unc)
#p values
print(multiCA_szabo_tbl_15yoM_unc)

#16yo male
print(df_proportions_16yoM_unc)
#p values
print(multiCA_szabo_tbl_16yoM_unc)

#17yo male
print(df_proportions_17yoM_unc)
#p values
print(multiCA_szabo_tbl_17yoM_unc)

#18yo male
print(df_proportions_18yoM_unc)
#p values
print(multiCA_szabo_tbl_18yoM_unc)

###########################################################################################
#Table S8: Proportion of respondents by sex of sex of sex partners (SSP) over the lifetime, 
#by age, among those who have ever had sex, Youth Risk Behavior Survey (YRBS), 2015-2021	
## Frequency of SSP by age, split by year and by sex

#14yo female
print(df_proportions_14yoF)
#p values
print(multiCA_szabo_tbl_14yoF)

#15yo female
print(df_proportions_15yoF)
#p values
print(multiCA_szabo_tbl_15yoF)

#16yo female
print(df_proportions_16yoF)
#p values
print(multiCA_szabo_tbl_16yoF)

#17yo female
print(df_proportions_17yoF)
#p values
print(multiCA_szabo_tbl_17yoF)

#18yo female
print(df_proportions_18yoF)
#p values
print(multiCA_szabo_tbl_18yoF)


#14yo male
print(df_proportions_14yoM)
#p values
print(multiCA_szabo_tbl_14yoM)

#15yo male
print(df_proportions_15yoM)
#p values
print(multiCA_szabo_tbl_15yoM)

#16yo male
print(df_proportions_16yoM)
#p values
print(multiCA_szabo_tbl_16yoM)

#17yo male
print(df_proportions_17yoM)
#p values
print(multiCA_szabo_tbl_17yoM)

#18yo male
print(df_proportions_18yoM)
#p values
print(multiCA_szabo_tbl_18yoM)


