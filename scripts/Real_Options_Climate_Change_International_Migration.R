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



#### Descriptive statistics ####

# create density plot of migration rates
df_total$mig_rate_scaled = df_total$mig_rate * 1000000
new_df = df_total[which(df_total$mig_rate_scaled < 10), ]
d = density(new_df$mig_rate_scaled)
par(mar = c(5, 4, 2, 2) + 0.1)
plot(d, main = "")
abline(v = mean(new_df$mig_rate_scaled), lty = 2)
df_total = subset(df_total, select = -c(mig_rate_scaled))
remove(d, new_df)

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

rm(mig_rate_midinc_contig, mig_rate_lowinc_contig,
       mig_rate_midinc_noncontig, mig_rate_lowinc_noncontig,
       mig_rate_midinc_OECD_dest, mig_rate_lowinc_OECD_dest,
       mig_rate_midinc_nonOECD_dest, mig_rate_lowinc_nonOECD_dest)
rm(ttest.mig_rate_new, ttest.temp_anom, ttest.precip_anom, ttest.share_temp_greater_1SD,
       ttest.share_precip_less_1SD, ttest.gdp_per_capita)

# density plots of temperature and precipitation anomalies

# temperature anomalies in low-income countries
d_temp_anom_lowinc = density(df_lowinc$temp_anom)
par(mar = c(5, 4, 2, 2) + 0.1)
plot(d_temp_anom_lowinc, main = "")
abline(v = mean(df_lowinc$temp_anom), lty = 2)

# precipitation anomalies in low-income countries
d_precip_anom_lowinc = density(df_lowinc$precip_anom)
par(mar = c(5, 4, 2, 2) + 0.1)
plot(d_precip_anom_lowinc, main = "")
abline(v = mean(df_lowinc$precip_anom), lty = 2)

# temperature anomalies in middle-income countries
d_temp_anom_midinc = density(df_midinc$temp_anom)
par(mar = c(5, 4, 2, 2) + 0.1)
plot(d_temp_anom_midinc, main = "")
abline(v = mean(df_midinc$temp_anom), lty = 2)

# precipitation anomalies in middle-income countries
d_precip_anom_midinc = density(df_midinc$precip_anom)
par(mar = c(5, 4, 2, 2) + 0.1)
plot(d_precip_anom_midinc, main = "")
abline(v = mean(df_midinc$precip_anom), lty = 2)

rm(d_precip_anom_lowinc, d_precip_anom_midinc, d_temp_anom_lowinc,
   d_temp_anom_midinc)

# skewness and excess kurtosis of temperature and precipitation anomalies
# low-income countries
skewness(df_lowinc$temp_anom)
kurtosis(df_lowinc$temp_anom)
skewness(df_lowinc$precip_anom)
kurtosis(df_lowinc$precip_anom)

# middle-income countries
skewness(df_midinc$temp_anom)
kurtosis(df_midinc$temp_anom)
skewness(df_midinc$precip_anom)
kurtosis(df_midinc$precip_anom)

# countries that experienced extreme temperature and precipitation anomalies
# (+2 sd and -2 sd respectively)
# low-income countries
countries_lowinc_temp_plus_2sd = (df_lowinc %>% filter(temp_anom > (mean(temp_anom) +
                                  2 * sd(temp_anom))))$X...origin
# extreme positive temperature anomalies
unique(countries_lowinc_temp_plus_2sd)
# calculate share of observations of low-income sample
(length(countries_lowinc_temp_plus_2sd) / length(df_lowinc$temp_anom)) * 100

countries_lowinc_precip_minus_2sd = (df_lowinc %>% filter(precip_anom <
                                     (mean(precip_anom) - 2 * sd(precip_anom))))$X...origin
# extreme negative precipitation anomalies
unique(countries_lowinc_precip_minus_2sd)
# calculate share of observations of low-income sample
(length(countries_lowinc_precip_minus_2sd) / length(df_lowinc$precip_anom)) * 100

# middle-income countries
countries_midinc_temp_plus_2sd = (df_midinc %>% filter(temp_anom > (mean(temp_anom) +
                                  2 * sd(temp_anom))))$X...origin
# extreme positive temperature anomalies
unique(countries_midinc_temp_plus_2sd)
# calculate share of observations of middle-income sample
(length(countries_midinc_temp_plus_2sd) / length(df_midinc$temp_anom)) * 100

countries_midinc_precip_minus_2sd = (df_midinc %>% filter(precip_anom <
                                     (mean(precip_anom) - 2 * sd(precip_anom))))$X...origin
# extreme negative precipitation anomalies
unique(countries_midinc_precip_minus_2sd)
# calculate share of observations of low-income sample
(length(countries_midinc_precip_minus_2sd) / length(df_midinc$precip_anom)) * 100

rm(countries_lowinc_temp_plus_2sd, countries_lowinc_precip_minus_2sd,
   countries_midinc_temp_plus_2sd, countries_midinc_precip_minus_2sd)



#### Main results ####

# GAM for total sample
gc()
tic()
gam_total = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period + X...origin +
                     destination, family = Gamma(link="log"), data = df_total, method = "REML")
toc()
summary(gam_total)

# linear model for comparison
g_total = mgcv::gam(mig_rate_new ~ temp_anom + precip_anom + period + X...origin + destination,
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
gam_lowinc = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period + X...origin +
                     destination, family = Gamma(link="log"), data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc)

# linear model for comparison
g_lowinc = mgcv::gam(mig_rate_new ~ temp_anom + precip_anom + period + X...origin + destination,
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
gam_midinc = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period + X...origin +
                     destination, family = Gamma(link="log"), data = df_midinc, method = "REML")
toc()
summary(gam_midinc)

# linear model for comparison
g_midinc = mgcv::gam(mig_rate_new ~ temp_anom + precip_anom +period + X...origin + destination,
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
gam_lowinc_contig = mgcv::gam(mig_rate_new ~ contiguity + s(temp_anom, bs='cr')  + s(temp_anom,by = contiguity,
                            bs='cr') + s(precip_anom, bs='cr')+ s(precip_anom, by = contiguity, bs='cr') +
                            period + X...origin + destination, family = Gamma(link = "log"), data = df_lowinc,
                            method = "REML")
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
gam_midinc_contig = mgcv::gam(mig_rate_new ~ contiguity + s(temp_anom, bs='cr')  + s(temp_anom, by = contiguity,
                            bs='cr') + s(precip_anom, bs='cr') + s(precip_anom, by = contiguity, bs='cr') +
                            period + X...origin + destination, family = Gamma(link="log"), data = df_midinc,
                            method = "REML")
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
gam_lowinc_OECD = mgcv::gam(mig_rate_new ~ OECD_dest + s(temp_anom, bs='cr') + s(temp_anom, by = OECD_dest,
                            bs='cr') + s(precip_anom, bs='cr') + s(precip_anom, by = OECD_dest, bs='cr') +
                            period + X...origin + destination, family = Gamma(link = "log"), data = df_lowinc,
                            method = "REML")
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
gam_midinc_OECD = mgcv::gam(mig_rate_new ~ OECD_dest + s(temp_anom, bs='cr') + s(temp_anom, by = OECD_dest,
                            bs='cr') + s(precip_anom, bs='cr') + s(precip_anom, by = OECD_dest, bs='cr') +
                            period + X...origin + destination, family = Gamma(link = "log"), data = df_midinc,
                            method = "REML")
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
gam_lowinc0.2 = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period +
                            X...origin + destination, family = Gamma(link = "log"), data = df_lowinc0.2,
                            method = "REML")
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
gam_midinc0.2 = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period +
                            X...origin + destination, family = Gamma(link = "log"), data = df_midinc0.2,
                            method = "REML")
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
gam_lowinc0.3 = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period +
                            X...origin + destination, family = Gamma(link = "log"), data = df_lowinc0.3,
                            method = "REML")
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
gam_midinc0.3 = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period +
                            X...origin + destination, family = Gamma(link = "log"), data = df_midinc0.3,
                            method = "REML")
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

rm(df_lowinc0.2, df_midinc0.2, df_lowinc0.3, df_midinc0.3)



#### Robustness check 2: smoothing parameter selection using GCV instead of REML ####

# GAM for low-income countries
gc()
tic()
gam_lowinc_gcv = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period +
                            X...origin + destination, family = Gamma(link = "log"), data = df_lowinc,
                            method = "GCV.Cp")
toc()
summary(gam_lowinc_gcv)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_lowinc_gcv, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_gcv, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_gcv)

# GAM for middle-income countries
gc()
tic()
gam_midinc_gcv = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period +
                            X...origin + destination, family = Gamma(link = "log"), data = df_midinc,
                            method = "GCV.Cp")
toc()
summary(gam_midinc_gcv)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc_gcv, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_gcv, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_gcv)



#### Robustness check 3: estimate GAMs for agriculturally dependent and not agriculturally dependent ####

# only including low-income countries
df_agri = df_total[which(df_total$preval_agri == 1), ]
# only including middle-income countries
df_nonagri = df_total[which(df_total$preval_agri == 0), ]

# GAM for agriculturally dependent countries
gc()
tic()
gam_agri = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period +
                     X...origin + destination, family = Gamma(link="log"), data = df_agri,
                     method = "REML")
toc()
summary(gam_agri)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_agri, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_agri, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_agri)

# GAM for not agriculturally dependent countries
gc()
tic()
gam_nonagri = mgcv::gam(mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period +
                     X...origin + destination, family = Gamma(link="log"), data = df_nonagri,
                     method = "REML")
toc()
summary(gam_nonagri)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_nonagri, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_nonagri, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_nonagri)

rm(df_agri, df_nonagri)



#### Robustness check 4: include economic control variables ####

# log GDP per capita ratio total sample
df_total$log_gdp_pc_ratio_origin_dest = log(df_totalc$gdp_pc_ratio_dest_origin)
# log distance total sample
df_total$log_dist = log(df_total$dist)
# log GDP per capita ratio low-income countries
df_lowinc$log_gdp_pc_ratio_origin_dest = log(df_lowinc$gdp_pc_ratio_dest_origin)
# log distance low-income countries
df_lowinc$log_dist = log(df_lowinc$dist)
# log GDP per capita ratio middle-income countries
df_midinc$log_gdp_pc_ratio_origin_dest = log(df_midinc$gdp_pc_ratio_dest_origin)
# log distance middle-income countries
df_midinc$log_dist = log(df_midinc$dist)

# GAM for low-income countries
gc()
tic()
gam_lowinc_controls = mgcv::gam(df_lowinc$mig_rate_new ~ s(df_lowinc$temp_anom, bs='cr') +
                            s(df_lowinc$precip_anom, bs='cr') + df_lowinc$log_gdp_pc_ratio_origin_dest +
                            df_lowinc$common_lang + df_lowinc$log_dist + df_lowinc$civil_war +
                            df_lowinc$period + df_lowinc$X...origin + df_lowinc$destination,
                            family = Gamma(link="log"), data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc_controls)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_lowinc_controls, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_controls, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_controls)

# GAM for middle-income countries
gc()
tic()
gam_midinc_controls = mgcv::gam(df_midinc$mig_rate_new ~ s(df_midinc$temp_anom, bs='cr') +
                            s(df_midinc$precip_anom, bs='cr') + df_midinc$log_gdp_pc_ratio_origin_dest +
                            df_midinc$common_lang + df_midinc$log_dist + df_midinc$civil_war +
                            df_midinc$period + df_midinc$X...origin + df_midinc$destination,
                            family = Gamma(link="log"), data = df_midinc, method = "REML")
toc()
summary(gam_midinc_controls)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc_controls, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_controls, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_controls)



#### Robustness check 5: estimate GAM using heat and drought month shares ####

# GAM for low-income countries
gc()
tic()
gam_lowinc_1sd = mgcv::gam(df_lowinc$mig_rate_new ~ s(df_lowinc$share_temp_greater_1SD, bs='cr') +
                            s(df_lowinc$share_precip_less_1SD, bs='cr') + df_lowinc$period +
                            df_lowinc$X...origin + df_lowinc$destination, family = Gamma(link="log"),
                            data = df_lowinc, method = "REML")
toc()
summary(gam_lowinc_1sd)

# plot smooth nonparametric functions of heat and drought month shares
plot(gam_lowinc_1sd, shade = TRUE, xlab = "Heat month share", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_1sd, shade = TRUE, xlab = "Drought month share", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_1sd)

# GAM for middle-income countries
gc()
tic()
gam_midinc_1sd = mgcv::gam(df_midinc$mig_rate_new ~ s(df_midinc$share_temp_greater_1SD, bs='cr') +
                            s(df_midinc$share_precip_less_1SD, bs='cr') + df_midinc$period +
                            df_midinc$X...origin + df_midinc$destination, family = Gamma(link="log"),
                            data = df_midinc, method = "REML")
toc()
summary(gam_midinc_1sd)

# plot smooth nonparametric functions of heat and drought month shares
plot(gam_midinc_1sd, shade = TRUE, xlab = "Heat month share", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_1sd, shade = TRUE, xlab = "Drought month share", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_1sd)



#### Robustness check 6: interaction between climatic anomalies and share of agricultural productivity ####

# create indicator of quantile in the distribution of share of agricultural GDP
df_total$agri_share_gdp_quantile = NA
agri_share_quantiles = quantile(df_lowinc$agri_share_gdp, na.rm = T)
df_total$agri_share_gdp_quantile = ifelse(df_total$agri_share_gdp <= agri_share_quantiles["25%"], "1",
                                   ifelse(df_total$agri_share_gdp > agri_share_quantiles["25%"] &
                                   df_total$agri_share_gdp <= agri_share_quantiles["50%"], "2",
                                   ifelse(df_total$agri_share_gdp > agri_share_quantiles["50%"] &
                                   df_total$agri_share_gdp <= agri_share_quantiles["75%"], "3", "4")))
df_total$agri_share_gdp_quantile = as.factor(df_total$agri_share_gdp_quantile)

# create separate dataframes for the quantiles
n = length(unique(df_total$agri_share_gdp_quantile))-sum(is.na(unique(df_total$agri_share_gdp_quantile)))
for(i in 1:n) {
  df = df_total %>% filter(agri_share_gdp_quantile == i)
  assign(paste0("df_agri", i), df)
  rm(df)
}
rm(i, n, agri_share_quantiles)

# GAMs for all four quantiles
# quantile 1
gc()
tic()
gam_agri1 = mgcv::gam(mig_rate_new ~ s(temp_anom, bs = 'cr') + s(precip_anom, bs = 'cr') + period +
                        X...origin + destination, family = Gamma(link="log"), data = df_agri1, method = "REML")
toc()
summary(gam_agri1)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_agri1, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_agri1, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_agri1)

# quantile 2
gc()
tic()
gam_agri2 = mgcv::gam(mig_rate_new ~ s(temp_anom, bs = 'cr') + s(precip_anom, bs = 'cr') + period +
                        X...origin + destination, family = Gamma(link="log"), data = df_agri2, method = "REML")
toc()
summary(gam_agri2)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_agri2, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_agri2, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_agri2)

# quantile 2
gc()
tic()
gam_agri3 = mgcv::gam(mig_rate_new ~ s(temp_anom, bs = 'cr') + s(precip_anom, bs = 'cr') + period +
                        X...origin + destination, family = Gamma(link="log"), data = df_agri3, method = "REML")
toc()
summary(gam_agri3)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_agri3, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_agri3, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_agri3)

# quantile 4
gc()
tic()
gam_agri4 = mgcv::gam(mig_rate_new ~ s(temp_anom, bs = 'cr') + s(precip_anom, bs = 'cr') + period +
                        X...origin + destination, family = Gamma(link="log"), data = df_agri4, method = "REML")
toc()
summary(gam_agri4)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_agri4, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_agri4, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
       cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_agri4)

rm(df_agri1, df_agri2, df_agri3, df_agri4)