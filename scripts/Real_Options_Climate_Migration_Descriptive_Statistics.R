# This R code produces descriptive statistics as well as some density plots 
# for the paper "A Real-Options Analysis of Climate Change and International Migration".
# The paper is available via SSRN: https://papers.ssrn.com/abstract=3951942
# Data is also available via Mendeley Data: https://doi.org/10.17632/7f5mmwxcpm.2

# Marius Braun, November 2022

# install required packages
 install.packages("dplyr")
 install.packages("readr")
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
