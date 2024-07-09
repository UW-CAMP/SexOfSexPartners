### Script for SSP analysis

# This script's code uses the YRBS dataset, previously prepared in script "SOGI_31_SSP_prepare"
### to conduct SSP-related analyses

### Prepare workspace ----- 
# clear environment
#rm(list = ls())

# packages
source("SOGI_00_packages.R")

# define functions
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}

# call in data
yrbs_final <- readRDS("data - clean/yrbs_final_new.rds")

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

# FIGURE 4: PLOTS BY AGE AND YEAR FOR FEMALES (CONDITIONED ON EHHS)
# tables 15-19
#Females Ages 14, 15, 16, 17, and 18 
#+++++++++++++++++++++++++++++++++++++++++
#####

png("plots/SSP/SSP_by_age_and_year_fem_one_panel.png", 
    width = 12*300, height = 12*300, res = 300
)
{
  par(mfrow=c(2,2))
  par(mar=c(2,0,3,0))
  par(oma=c(0,5,0,1))
  my_lwd = 1.5
  my_xlim = c(2014.5, 2021.5)
  pt_text_cex = 0.6
  
  plot(df_proportions_14yoF$year, df_proportions_14yoF$females_prop/100, type = "l", 
       col = "white", lwd = my_lwd, xlab = "", ylab = "Proportion", 
       xlim = my_xlim, ylim = c(0, 1), main = "", frame.plot = TRUE, axes = FALSE)
  axis(1, seq(2015,2021,2), labels=FALSE, cex.axis=0.7)
  mtext(seq(2015,2021,2), side = 1, line = 0.5, at=seq(2015,2021,2), cex=0.58)
  axis(2, seq(0,1,0.1), seq(0,1,0.1), cex.axis=0.7, las=1)
  mtext("Female respondents\nwho have ever had sex", 3, cex = 0.7, line =0.3)
  abline(h=seq(0,1,0.1), col='lightgray', lwd=0.5)
# col_age <- gray(c(0.6, 0.4, 0.2, 0.1, 0.0))
  col_age = c("red", "orange", "green", "blue", "black")
  lty_sps <- c(2,3,1)
    
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


 
  # Create an empty plot for the legend
  # legend("center", c("Female partners only", 
  #                    "Male partners only", 
  #                    "Female and male partners"),
  #        col=c("#31BAF6", "#EEC441", "#37C817"), 
  #        lty = 1:4, cex=0.8, bty = "o"
  # ) 

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

