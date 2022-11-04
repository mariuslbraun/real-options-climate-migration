# install required packages
install.packages("fitdistrplus")
install.packages("goft")
install.packages("mgcv")
install.packages("ggplot2")
install.packages("tictoc")
install.packages("itsadug")

# load packages
library(readr)
library(dplyr)
library(fitdistrplus)
library(goft)
library(mgcv)
library(ggplot2)
library(tictoc)
library(itsadug)

# set working directory
setwd("C:\\Users\\Marius Braun\\Documents\\A Real-Options Analysis of Climate Change and International Migration\\Revise and resubmit 2\\Real_Options_Climate_Migration")

# clear workspace
rm(list = ls())

# load dataset (non-OECD countries only)
df_total = read.csv("prepared\\Dataset_final.csv")
df_total$mig_rate_new = df_total$mig_rate * 100

# create histogram of migration rates
df_total$mig_rate_scaled = df_total$mig_rate * 1000000
new_df = df_total[which(df_total$mig_rate_scaled < 10), ]
d = density(new_df$mig_rate_scaled)
par(mar = c(5, 4, 2, 2) + 0.1)
plot(d, main = "")
df_total = subset(df_total, select = -c(mig_rate_scaled))
remove(d, new_df)

# create separate datasets for low- and middle-income countries
# low-income countries
df_lowinc = df_total[which(df_total$low_income == 1), ]
# middle-income countries
df_midinc = df_total[which(df_total$low_income == 0), ]

#### Descriptive statistics ####

# bilateral migration rate
# middle-income countries
mean(df_midinc$mig_rate_new)
sd(df_midinc$mig_rate_new)
sum(!is.na(df_midinc$mig_rate_new))

# low-income countries
mean(df_lowinc$mig_rate_new)
sd(df_lowinc$mig_rate_new)
sum(!is.na(df_lowinc$mig_rate_new))

# temperature anomaly
# middle-income countries
mean(df_midinc$temp_anom)
sd(df_midinc$temp_anom)
sum(!is.na(unique(df_midinc$temp_anom)))

# low-income countries
mean(df_lowinc$temp_anom)
sd(df_lowinc$temp_anom)
sum(!is.na(unique(df_lowinc$temp_anom)))

# precipitation anomaly
# middle-income countries
mean(df_midinc$precip_anom)
sd(df_midinc$precip_anom)
sum(!is.na(unique(df_midinc$precip_anom)))

# low-income countries
mean(df_lowinc$precip_anom)
sd(df_lowinc$precip_anom)
sum(!is.na(unique(df_lowinc$precip_anom)))

# share of heat months
# middle-income countries
mean(df_midinc$share_temp_greater_1SD)
sd(df_midinc$share_temp_greater_1SD)
sum(!is.na(unique(df_midinc$share_temp_greater_1SD)))

# low-income countries
mean(df_lowinc$share_temp_greater_1SD)
sd(df_lowinc$share_temp_greater_1SD)
sum(!is.na(unique(df_lowinc$share_temp_greater_1SD)))

# share of drought months
# middle-income countries
mean(df_midinc$share_precip_less_1SD)
sd(df_midinc$share_precip_less_1SD)
sum(!is.na(unique(df_midinc$share_precip_less_1SD)))

# low-income countries
mean(df_lowinc$share_precip_less_1SD)
sd(df_lowinc$share_precip_less_1SD)
sum(!is.na(unique(df_lowinc$share_precip_less_1SD)))

# GDP per capita
# middle-income countries
mean(df_midinc$gdp_pc_origin)
sd(df_midinc$gdp_pc_origin)
sum(!is.na(unique(df_midinc$gdp_pc_origin)))

# low-income countries
mean(df_lowinc$gdp_pc_origin)
sd(df_lowinc$gdp_pc_origin)
sum(!is.na(unique(df_lowinc$gdp_pc_origin)))

# migration rates to neighboring countries
mig_rate_midinc_contig = df_midinc %>% filter(contiguity == 1) %>%
                                          dplyr::select(mig_rate_new)
mean(mig_rate_midinc_contig$mig_rate_new)
sd(mig_rate_midinc_contig$mig_rate_new)

mig_rate_lowinc_contig = df_lowinc %>% filter(contiguity == 1) %>%
                                          dplyr::select(mig_rate_new)
mean(mig_rate_lowinc_contig$mig_rate_new)
sd(mig_rate_lowinc_contig$mig_rate_new)

# migration rates to non-neighboring countries
mig_rate_midinc_noncontig = df_midinc %>% filter(contiguity == 0) %>%
                                   dplyr::select(mig_rate_new)
mean(mig_rate_midinc_noncontig$mig_rate_new)
sd(mig_rate_midinc_noncontig$mig_rate_new)

mig_rate_lowinc_noncontig = df_lowinc %>% filter(contiguity == 0) %>%
                                   dplyr::select(mig_rate_new)
mean(mig_rate_lowinc_noncontig$mig_rate_new)
sd(mig_rate_lowinc_noncontig$mig_rate_new)

# migration rates to OECD countries
mig_rate_midinc_OECD_dest = df_midinc %>% filter(OECD_dest == 1) %>%
                                   dplyr::select(mig_rate_new)
mean(mig_rate_midinc_OECD_dest$mig_rate_new)
sd(mig_rate_midinc_OECD_dest$mig_rate_new)

mig_rate_lowinc_OECD_dest = df_lowinc %>% filter(OECD_dest == 1) %>%
                                   dplyr::select(mig_rate_new)
mean(mig_rate_lowinc_OECD_dest$mig_rate_new)
sd(mig_rate_lowinc_OECD_dest$mig_rate_new)

# migration rates to non-OECD countries
mig_rate_midinc_nonOECD_dest = df_midinc %>% filter(OECD_dest == 0) %>%
                                   dplyr::select(mig_rate_new)
mean(mig_rate_midinc_nonOECD_dest$mig_rate_new)
sd(mig_rate_midinc_nonOECD_dest$mig_rate_new)

mig_rate_lowinc_nonOECD_dest = df_lowinc %>% filter(OECD_dest == 0) %>%
                                   dplyr::select(mig_rate_new)
mean(mig_rate_lowinc_nonOECD_dest$mig_rate_new)
sd(mig_rate_lowinc_nonOECD_dest$mig_rate_new)

# t-test difference between means of low- and middle-income countries
ttest.mig_rate_new = t.test(df_lowinc$mig_rate_new, df_midinc$mig_rate_new)
ttest.mig_rate_new
ttest.temp_anom = t.test(df_lowinc$temp_anom, df_midinc$temp_anom)
ttest.temp_anom
ttest.precip_anom = t.test(df_lowinc$precip_anom, df_midinc$precip_anom)
ttest.precip_anom
ttest.share_temp_greater_1SD = t.test(df_lowinc$share_temp_greater_1SD, df_midinc$share_temp_greater_1SD)
ttest.share_temp_greater_1SD
ttest.share_precip_less_1SD = t.test(df_lowinc$share_precip_less_1SD, df_midinc$share_precip_less_1SD)
ttest.share_precip_less_1SD
ttest.gdp_per_capita = t.test(df_lowinc$gdp_pc_origin, df_midinc$gdp_pc_origin)
ttest.gdp_per_capita

remove(mig_rate_midinc_contig, mig_rate_lowinc_contig,
       mig_rate_midinc_noncontig, mig_rate_lowinc_noncontig,
       mig_rate_midinc_OECD_dest, mig_rate_lowinc_OECD_dest,
       mig_rate_midinc_nonOECD_dest, mig_rate_lowinc_nonOECD_dest)
remove(ttest.mig_rate_new, ttest.temp_anom, ttest.precip_anom, ttest.share_temp_greater_1SD,
       ttest.share_precip_less_1SD, ttest.gdp_per_capita)



#### Main results ####

# GAM for total sample
gc()
tic()
gam_total = mgcv::gam(df_total$mig_rate_new ~ s(df_total$temp_anom, bs='cr') +
                     s(df_total$precip_anom, bs='cr') + df_total$period + df_total$X...origin +
                     df_total$destination, family = Gamma(link="log"), data = df_total, method = "REML")
toc()
summary(gam_total)

# linear model for comparison
g_total = mgcv::gam(df_total$mig_rate_new ~ df_total$temp_anom + df_total$precip_anom +
                     df_total$period + df_total$X...origin + df_total$destination,
                     family = Gamma(link="log"), data = df_total, method = "REML")
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
gam_lowinc = mgcv::gam(df_lowinc$mig_rate_new ~ s(df_lowinc$temp_anom, bs='cr') +
                     s(df_lowinc$precip_anom, bs='cr') + df_lowinc$period + df_lowinc$X...origin +
                     df_lowinc$destination, family = Gamma(link="log"), data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc)

# linear model for comparison
g_lowinc = mgcv::gam(df_lowinc$mig_rate_new ~ df_lowinc$temp_anom + df_lowinc$precip_anom +
                     df_lowinc$period + df_lowinc$X...origin + df_lowinc$destination,
                     family = Gamma(link="log"), data = df_lowinc, method = "REML")
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
gam_midinc = mgcv::gam(df_midinc$mig_rate_new ~ s(df_midinc$temp_anom, bs='cr') +
                     s(df_midinc$precip_anom, bs='cr') + df_midinc$period + df_midinc$X...origin +
                     df_midinc$destination, family = Gamma(link="log"), data = df_midinc, method = "REML")
toc()
summary(gam_midinc)

# linear model for comparison
g_midinc = mgcv::gam(df_midinc$mig_rate_new ~ df_midinc$temp_anom + df_midinc$precip_anom +
                     df_midinc$period + df_midinc$X...origin + df_midinc$destination,
                     family = Gamma(link="log"), data = df_midinc, method = "REML")
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

# GAM for low-income countries
gc()
tic()
gam_lowinc_contig = mgcv::gam(df_lowinc$mig_rate_new ~ df_lowinc$contiguity +
                            s(df_lowinc$temp_anom, bs='cr')  + s(df_lowinc$temp_anom,
                            by = df_lowinc$contiguity, bs='cr') + s(df_lowinc$precip_anom, bs='cr')
                            + s(df_lowinc$precip_anom, by = df_lowinc$contiguity, bs='cr') +
                            df_lowinc$period + df_lowinc$X...origin + df_lowinc$destination,
                            family = Gamma(link = "log"), data = df_lowinc, method = "REML")
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
gam_midinc_contig = mgcv::gam(df_midinc$mig_rate_new ~ df_midinc$contiguity +
                            s(df_midinc$temp_anom, bs='cr')  + s(df_midinc$temp_anom,
                            by = df_midinc$contiguity, bs='cr') + s(df_midinc$precip_anom, bs='cr')
                            + s(df_midinc$precip_anom, by = df_midinc$contiguity, bs='cr')
                            + df_midinc$period + df_midinc$X...origin + df_midinc$destination,
                            family = Gamma(link="log"), data = df_midinc, method = "REML")
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

# GAM for low-income countries
gc()
tic()
gam_lowinc_OECD = mgcv::gam(df_lowinc$mig_rate_new ~ df_lowinc$OECD_dest +
                            s(df_lowinc$temp_anom, bs='cr') + s(df_lowinc$temp_anom,
                            by = df_lowinc$OECD_dest, bs='cr') + s(df_lowinc$precip_anom, bs='cr')
                            + s(df_lowinc$precip_anom, by = df_lowinc$OECD_dest, bs='cr') +
                            df_lowinc$period + df_lowinc$X...origin + df_lowinc$destination,
                            family = Gamma(link = "log"), data = df_lowinc, method = "REML")
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
gam_midinc_OECD = mgcv::gam(df_midinc$mig_rate_new ~ df_midinc$OECD_dest +
                            s(df_midinc$temp_anom, bs='cr') + s(df_midinc$temp_anom,
                            by = df_midinc$OECD_dest, bs='cr') + s(df_midinc$precip_anom, bs='cr') +
                            s(df_midinc$precip_anom, by = df_midinc$OECD_dest, bs='cr') +
                            df_midinc$period + df_midinc$X...origin + df_midinc$destination,
                            family = Gamma(link = "log"), data = df_midinc, method = "REML")
toc()
summary(gam_midinc_OECD)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc_OECD, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_OECD, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_OECD)



#### Robustness check 1: vary thresholds for low-income countries ####

# lower threshold: bottom 20 % of GDP per capita distribution defined as low-income countries
df_lowinc0.2 = df_total[which(df_total$low_income0.2 == 1), ]
df_midinc0.2 = df_total[which(df_total$low_income0.2 == 0), ]

# GAM for low-income countries
gc()
tic()
gam_lowinc0.2 = mgcv::gam(df_lowinc0.2$mig_rate_new ~ s(df_lowinc0.2$temp_anom, bs='cr') +
                            s(df_lowinc0.2$precip_anom, bs='cr') + df_lowinc0.2$period +
                            df_lowinc0.2$X...origin + df_lowinc0.2$destination,
                            family = Gamma(link = "log"), data = df_lowinc0.2, method = "REML")
toc()
summary(gam_lowinc0.2)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_lowinc0.2, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc0.2, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc0.2)

# GAM for middle-income countries
gc()
tic()
gam_midinc0.2 = mgcv::gam(df_midinc0.2$mig_rate_new ~ s(df_midinc0.2$temp_anom, bs='cr') +
                            s(df_midinc0.2$precip_anom, bs='cr') + df_midinc0.2$period +
                            df_midinc0.2$X...origin + df_midinc0.2$destination,
                            family = Gamma(link = "log"), data = df_midinc0.2, method = "REML")
toc()
summary(gam_midinc0.2)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc0.2, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc0.2, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc0.2)

# higher threshold: bottom 30 % of GDP per capita distribution defined as low-income countries
df_lowinc0.3 = df_total[which(df_total$low_income0.3 == 1), ]
df_midinc0.3 = df_total[which(df_total$low_income0.3 == 0), ]

# GAM for low-income countries
gc()
tic()
gam_lowinc0.3 = mgcv::gam(df_lowinc0.3$mig_rate_new ~ s(df_lowinc0.3$temp_anom, bs='cr') +
                            s(df_lowinc0.3$precip_anom, bs='cr') + df_lowinc0.3$period +
                            df_lowinc0.3$X...origin + df_lowinc0.3$destination,
                            family = Gamma(link = "log"), data = df_lowinc0.3, method = "REML")
toc()
summary(gam_lowinc0.3)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_lowinc0.3, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc0.3, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc0.3)

# GAM for middle-income countries
gc()
tic()
gam_midinc0.3 = mgcv::gam(df_midinc0.3$mig_rate_new ~ s(df_midinc0.3$temp_anom, bs='cr') +
                            s(df_midinc0.3$precip_anom, bs='cr') + df_midinc0.3$period +
                            df_midinc0.3$X...origin + df_midinc0.3$destination,
                            family = Gamma(link = "log"), data = df_midinc0.3, method = "REML")
toc()
summary(gam_midinc0.3)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc0.3, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc0.3, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc0.3)

remove(df_lowinc0.2, df_midinc0.2, df_lowinc0.3, df_midinc0.3)



#### Robustness check 2: smoothing parameter selection using GCV instead of REML ####

# GAM for low-income countries
gc()
tic()
gam_lowinc_gcv = mgcv::gam(df_lowinc$mig_rate_new ~ s(df_lowinc$temp_anom, bs='cr') + s(df_lowinc$precip_anom, bs='cr') + df_lowinc$period + df_lowinc$X...origin + df_lowinc$destination, family = Gamma(link = "log"), data = df_lowinc, method = "GCV.Cp")
toc()
summary(gam_lowinc_gcv)



plot(gam_lowinc_gcv, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_gcv, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_gcv)



# GAM for middle-income countries
gc()
tic()
gam_midinc_gcv = mgcv::gam(df_midinc$mig_rate_new ~ s(df_midinc$temp_anom, bs='cr') + s(df_midinc$precip_anom, bs='cr') + df_midinc$period + df_midinc$X...origin + df_midinc$destination, family = Gamma(link = "log"), data = df_midinc, method = "GCV.Cp")
toc()
summary(gam_midinc_gcv)



plot(gam_midinc_gcv, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_gcv, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_gcv)





# more robustness checks: estimate GAMs for agriculturally dependent and not agriculturally dependent (rather than low- and middle-income)

# only including low-income countries
df_agri = df_total[which(df_total$preval_agri == 1), ]
# only including middle-income countries
df_nonagri = df_total[which(df_total$preval_agri == 0), ]



# GAM for agriculturally dependent countries
gc()
tic()
gam_agri = mgcv::gam(df_agri$mig_rate_new ~ s(df_agri$temp_anom, bs='cr') + s(df_agri$precip_anom, bs='cr') + df_agri$period + df_agri$X...origin + df_agri$destination, family = Gamma(link="log"), data = df_agri, method = "REML")
toc()
summary(gam_agri)




plot(gam_agri, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_agri, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_agri)




# GAM for not agriculturally dependent countries
gc()
tic()
gam_nonagri = mgcv::gam(df_nonagri$mig_rate_new ~ s(df_nonagri$temp_anom, bs='cr') + s(df_nonagri$precip_anom, bs='cr') + df_nonagri$period + df_nonagri$X...origin + df_nonagri$destination, family = Gamma(link="log"), data = df_nonagri, method = "REML")
toc()
summary(gam_nonagri)




plot(gam_nonagri, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_nonagri, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_nonagri)

remove(df_agri, df_nonagri)



# more robustness checks: include economic control variables
df_total$log_gdp_pc_ratio_origin_dest = log(df_totalc$gdp_pc_ratio_dest_origin)
df_total$log_dist = log(df_total$dist)
df_lowinc$log_gdp_pc_ratio_origin_dest = log(df_lowinc$gdp_pc_ratio_dest_origin)
df_lowinc$log_dist = log(df_lowinc$dist)
df_midinc$log_gdp_pc_ratio_origin_dest = log(df_midinc$gdp_pc_ratio_dest_origin)
df_midinc$log_dist = log(df_midinc$dist)

# GAM for total sample
gc()
tic()
gam_total_controls = mgcv::gam(df_total$mig_rate_new ~ s(df_total$temp_anom, bs='cr') + s(df_total$precip_anom, bs='cr') + df_total$log_gdp_pc_ratio_origin_dest + df_total$common_lang + df_total$log_dist + df_total$civil_war + df_total$period + df_total$X...origin + df_total$destination, family = Gamma(link="log"), data = df_total, method = "REML")
toc()
summary(gam_total_controls)



# plot(gam_total_controls, shade = TRUE, xlab = "Temperature anomalies", ylab = "log of bilateral migration rates", cex.lab = 1.5)
# plot(gam_total_controls, shade = TRUE, xlab = "Precipitation anomalies", ylab = "log of bilateral migration rates", cex.lab = 1.5)
# abline(h=0, v=0, lty=2)
# gam.check(gam_total_controls)


# GAM for low-income countries
gc()
tic()
gam_lowinc_controls = mgcv::gam(df_lowinc$mig_rate_new ~ s(df_lowinc$temp_anom, bs='cr') + s(df_lowinc$precip_anom, bs='cr') + df_lowinc$log_gdp_pc_ratio_origin_dest + df_lowinc$common_lang + df_lowinc$log_dist + df_lowinc$civil_war + df_lowinc$period + df_lowinc$X...origin + df_lowinc$destination, family = Gamma(link="log"), data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc_controls)



plot(gam_lowinc_controls, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_controls, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_controls)





# GAM for middle-income countries
gc()
tic()
gam_midinc_controls = mgcv::gam(df_midinc$mig_rate_new ~ s(df_midinc$temp_anom, bs='cr') + s(df_midinc$precip_anom, bs='cr') + df_midinc$log_gdp_pc_ratio_origin_dest + df_midinc$common_lang + df_midinc$log_dist + df_midinc$civil_war + df_midinc$period + df_midinc$X...origin + df_midinc$destination, family = Gamma(link="log"), data = df_midinc, method = "REML")
toc()
summary(gam_midinc_controls)



plot(gam_midinc_controls, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_controls, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_controls)



# more robustness checks: estimate GAM using heat and drought month shares
# GAM for total sample
gc()
tic()
gam_total_1sd = mgcv::gam(df_total$mig_rate_new ~ s(df_total$share_temp_greater_1SD, bs='cr') + s(df_total$share_precip_less_1SD, bs='cr') + df_total$period + df_total$X...origin + df_total$destination, family = Gamma(link="log"), data = df_total, method = "REML")
toc()
summary(gam_total_1sd)


# plot(gam_total_1sd, shade = TRUE, xlab = "Heat month share", ylab = "", cex.lab = 1.6, cex.axis = 1.3)
# title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.6)
# plot(gam_total_1sd, shade = TRUE, xlab = "Drought month share", ylab = "", cex.lab = 1.6, cex.axis = 1.3)
# title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.6)
# abline(h=0, v=0, lty=2)
# gam.check(gam_total_1sd)



# GAM for low-income countries
gc()
tic()
gam_lowinc_1sd = mgcv::gam(df_lowinc$mig_rate_new ~ s(df_lowinc$share_temp_greater_1SD, bs='cr') + s(df_lowinc$share_precip_less_1SD, bs='cr') + df_lowinc$period + df_lowinc$X...origin + df_lowinc$destination, family = Gamma(link="log"), data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc_1sd)



plot(gam_lowinc_1sd, shade = TRUE, xlab = "Heat month share", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_1sd, shade = TRUE, xlab = "Drought month share", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_1sd)



# GAM for middle-income countries
gc()
tic()
gam_midinc_1sd = mgcv::gam(df_midinc$mig_rate_new ~ s(df_midinc$share_temp_greater_1SD, bs='cr') + s(df_midinc$share_precip_less_1SD, bs='cr') + df_midinc$period + df_midinc$X...origin + df_midinc$destination, family = Gamma(link="log"), data = df_midinc, method = "REML")
toc()
summary(gam_midinc_1sd)




plot(gam_midinc_1sd, shade = TRUE, xlab = "Heat month share", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_1sd, shade = TRUE, xlab = "Drought month share", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_1sd)

