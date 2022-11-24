# install required packages
 install.packages("fitdistrplus")
 install.packages("goft")
 install.packages("mgcv")
 install.packages("ggplot2")
 install.packages("tictoc")
 install.packages("itsadug")
 install.packages("moments")

# load packages
library(readr)
library(dplyr)
library(fitdistrplus)
library(goft)
library(mgcv)
library(ggplot2)
library(tictoc)
library(itsadug)
library(moments)

# set working directory
setwd("C:\\Users\\Marius Braun\\Documents\\A Real-Options Analysis of Climate Change and International Migration\\Revise and resubmit 2\\Real_Options_Climate_Migration")

# clear workspace
rm(list = ls())

# load dataset (non-OECD countries only)
df_total = read.csv("prepared\\Dataset_final.csv")
df_total$mig_rate_new = df_total$mig_rate * 100

# create separate datasets for low- and middle-income countries
# low-income countries
df_lowinc = df_total[which(df_total$low_income == 1), ]
# middle-income countries
df_midinc = df_total[which(df_total$low_income == 0), ]



#### Main results ####

# formulas for regression models
baseline_formula_gam = "mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period + X...origin + destination"
baseline_formula_glm = "mig_rate_new ~ temp_anom + precip_anom + period + X...origin + destination"

# GAM for total sample
gc()
tic()
gam_total = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                     data = df_total, method = "REML")
toc()
summary(gam_total)

# linear model for comparison
g_total = mgcv::gam(baseline_formula_glm, family = Gamma(link="log"),
                     data = df_total, method = "REML")
# Chi square test compared semiparametric and linear models
compareML(gam_total, g_total)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_total, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_total, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_total)

# GAM for low-income countries
gc()
tic()
gam_lowinc = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                     data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc)

# linear model for comparison
g_lowinc = mgcv::gam(baseline_formula_glm, family = Gamma(link="log"),
                     data = df_lowinc, method = "REML")
# Chi square test compared semiparametric and linear models
compareML(gam_lowinc, g_lowinc)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_lowinc, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc)

# GAM for middle-income countries
gc()
tic()
gam_midinc = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                     data = df_midinc, method = "REML")
toc()
summary(gam_midinc)

# linear model for comparison
g_midinc = mgcv::gam(baseline_formula_glm, family = Gamma(link="log"),
                     data = df_midinc, method = "REML")
# Chi-squared test to compare semiparametric and linear models
compareML(gam_midinc, g_midinc)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc)



#### Additional results 1: interaction terms for contiguity ####

# formula for regression models
contig_formula_gam = "mig_rate_new ~ contiguity + s(temp_anom, bs='cr')  + s(temp_anom,by = contiguity, bs='cr') + s(precip_anom, bs='cr')+ s(precip_anom, by = contiguity, bs='cr') + period + X...origin + destination"

# GAM for low-income countries
gc()
tic()
gam_lowinc_contig = mgcv::gam(contig_formula_gam, family = Gamma(link = "log"),
                            data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc_contig)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_lowinc_contig, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_contig, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_contig)

# GAM for middle-income countries
gc()
tic()
gam_midinc_contig = mgcv::gam(contig_formula_gam, family = Gamma(link="log"),
                            data = df_midinc, method = "REML")
toc()
summary(gam_midinc_contig)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc_contig, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_contig, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_contig)



#### Additional results 2: interaction terms for OECD destination ####

# formula for regression models
OECD_formula_gam = "mig_rate_new ~ OECD_dest + s(temp_anom, bs='cr') + s(temp_anom, by = OECD_dest, bs='cr') + s(precip_anom, bs='cr') + s(precip_anom, by = OECD_dest, bs='cr') + period + X...origin + destination"

# GAM for low-income countries
gc()
tic()
gam_lowinc_OECD = mgcv::gam(OECD_formula_gam, family = Gamma(link = "log"),
                            data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc_OECD)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_lowinc_OECD, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_OECD, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_OECD)

# GAM for middle-income countries
gc()
tic()
gam_midinc_OECD = mgcv::gam(OECD_formula_gam, family = Gamma(link = "log"),
                            data = df_midinc, method = "REML")
toc()
summary(gam_midinc_OECD)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc_OECD, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_OECD, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_OECD)
