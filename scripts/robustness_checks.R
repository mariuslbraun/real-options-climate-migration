# This R code produces robustness checks for the paper "A Real-Options 
# Analysis of Climate Change and International Migration".
# The paper is available via SSRN: https://papers.ssrn.com/abstract=3951942
# Data is also available via Mendeley Data: https://doi.org/10.17632/7f5mmwxcpm.2

# Marius Braun, November 2022

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

# load functions
source("scripts/functions.R")

# load data frames
samples = c("total", "lowinc", "midinc")
for(i in 1:length(samples)) {
  assign(
    x = paste(
      "df",
      samples[i],
      sep = "_"
    ),
    value = get_df(samples[i])
  )
}
rm(i)



#### 3.1: vary thresholds for low-income countries ####

# formula for regression models
baseline_formula_gam = "mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period + X...origin + destination"
climate_vars = c("temp_anom", "precip_anom")

# lower threshold: bottom 20 % of GDP per capita distribution defined as low-income countries
df_lowinc0.2 = df_total[which(df_total$low_income0.2 == 1), ]
df_midinc0.2 = df_total[which(df_total$low_income0.2 == 0), ]

# GAM for low-income countries
gc()
tic()
gam_lowinc0.2 = mgcv::gam(baseline_formula_gam, family = Gamma(link = "log"),
                            data = df_lowinc0.2, method = "REML")
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
gam_midinc0.2 = mgcv::gam(baseline_formula_gam, family = Gamma(link = "log"),
                            data = df_midinc0.2, method = "REML")
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
gam_lowinc0.3 = mgcv::gam(baseline_formula_gam, family = Gamma(link = "log"),
                            data = df_lowinc0.3, method = "REML")
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
gam_midinc0.3 = mgcv::gam(baseline_formula_gam, family = Gamma(link = "log"),
                            data = df_midinc0.3, method = "REML")
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



#### 3.2: smoothing parameter selection using GCV instead of REML ####

# estimate GAMs for low-income and middle-income sample
tic()
for(i in 2:length(samples)) {
  # model name
  model_name = paste(
    "gam",
    samples[i],
    "gcv",
    sep = "_"
  )
  
  # estimate GAM
  assign(
    x = model_name,
    value = estimate_GAM(
      sample = samples[i],
      formulae = baseline_formula_gam,
      smoothing_method = "GCV.Cp",
      directory = "gcv"
    )
  )
  
  # plot smooths
  for(k in 1:length(climate_vars)) {
    plot = show_gam(
      sample = samples[i],
      climate_var = climate_vars[k],
      model = get(model_name),
      directory = "gcv"
    )
  }
}
toc()
rm(i, k, model_name, plot)



#### 3.3: include economic control variables ####

# formula for regression models
controls_formula_gam = "mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + log_gdp_pc_ratio_origin_dest + common_lang + log_dist + civil_war + period + X...origin + destination"

# log GDP per capita ratio total sample
df_total$log_gdp_pc_ratio_origin_dest = log(df_total$gdp_pc_ratio_dest_origin)
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
gam_lowinc_controls = mgcv::gam(controls_formula_gam, family = Gamma(link="log"),
                                   data = df_lowinc, method = "REML")
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
gam_midinc_controls = mgcv::gam(controls_formula_gam, family = Gamma(link="log"),
                                   data = df_midinc, method = "REML")
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



#### 3.4: estimate GAM using heat and drought month shares ####

# formula for regression models 
shares_formula_gam = "mig_rate_new ~ s(share_temp_greater_1SD, bs='cr') + s(share_precip_less_1SD, bs='cr') + period + X...origin + destination"
climate_shares = c("share_temp_greater_1SD", "share_precip_less_1SD")
# estimate GAMs for low-income and middle-income sample
tic()
for(i in 2:length(samples)) {
    # model name
    model_name = paste(
      "gam",
      samples[i],
      "shares",
      sep = "_"
    )
    
    # estimate GAM
    assign(
      x = model_name,
      value = estimate_GAM(
        sample = samples[i],
        formulae = shares_formula_gam,
        directory = "shares"
      )
    )
    
    # plot smooths
    for(k in 1:length(climate_shares)) {
      plot = show_gam(
        sample = samples[i],
        climate_var = climate_shares[k],
        model = get(model_name),
        directory = "shares"
      )
    }
}
toc()
rm(i, k, model_name, plot, shares_formula_gam, climate_shares)



#### 3.5: interaction between climatic anomalies and share of agricultural productivity ####

# create indicator of quartile in the distribution of share of agricultural GDP
df_total$agri_share_gdp_quantile = NA
agri_share_quantiles = quantile(df_lowinc$agri_share_gdp, na.rm = T)
df_total$agri_share_gdp_quantile = ifelse(df_total$agri_share_gdp <= agri_share_quantiles["25%"], "1",
                                   ifelse(df_total$agri_share_gdp > agri_share_quantiles["25%"] &
                                   df_total$agri_share_gdp <= agri_share_quantiles["50%"], "2",
                                   ifelse(df_total$agri_share_gdp > agri_share_quantiles["50%"] &
                                   df_total$agri_share_gdp <= agri_share_quantiles["75%"], "3", "4")))
df_total$agri_share_gdp_quantile = as.factor(df_total$agri_share_gdp_quantile)

# GAM with interaction between climatic anomalies and factor variable indicating 
# quartile of agricultural GDP
gc()
tic()
gam_total_agri = mgcv::gam(mig_rate_new ~ s(temp_anom, by = agri_share_gdp_quantile, bs = 'cr') +
                           s(precip_anom, by = agri_share_gdp_quantile, bs = 'cr') + period + X...origin +
                           destination, family = Gamma(link="log"), data = df_total, method = "REML")
toc()
summary(gam_total_agri)

# create separate dataframes for the quartiles
n = length(unique(df_total$agri_share_gdp_quantile))-sum(is.na(unique(df_total$agri_share_gdp_quantile)))
for(i in 1:n) {
  df = df_total %>% filter(agri_share_gdp_quantile == i)
  assign(paste0("df_agri", i), df)
  rm(df)
}
rm(i, n, agri_share_quantiles)

# separate GAMs for all four quantiles for plotting (estimated GAMs are equivalent)
# quantile 1
gc()
tic()
gam_agri1 = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                            data = df_agri1, method = "REML")
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
gam_agri2 = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                            data = df_agri2, method = "REML")
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
gam_agri3 = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                            data = df_agri3, method = "REML")
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
gam_agri4 = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                            data = df_agri4, method = "REML")
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



#### 3.6: exclude observations with extreme temperature and precipitation anomalies ####
# low-income countries
df_lowinc_no_2sd = df_lowinc %>%
                        filter(!(temp_anom > (mean(temp_anom) + 2 * sd(temp_anom)) |
                               precip_anom < (mean(precip_anom) - 2 * sd(precip_anom))
                               ))

# GAM for low-income countries excluding outliers
gc()
tic()
gam_lowinc_no_2sd = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                                   data = df_lowinc_no_2sd, method = "REML")
toc()
summary(gam_lowinc_no_2sd)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_lowinc_no_2sd, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
     cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_lowinc_no_2sd, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
     cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_lowinc_no_2sd)

# middle-income countries
df_midinc_no_2sd = df_midinc %>%
                        filter(!(temp_anom > (mean(temp_anom) + 2 * sd(temp_anom)) |
                               precip_anom < (mean(precip_anom) - 2 * sd(precip_anom))
                               ))

# GAM for middle-income countries excluding outliers
gc()
tic()
gam_midinc_no_2sd = mgcv::gam(baseline_formula_gam, family = Gamma(link="log"),
                                   data = df_midinc_no_2sd, method = "REML")
toc()
summary(gam_midinc_no_2sd)

# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_midinc_no_2sd, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4,
     cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_midinc_no_2sd, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4,
     cex.axis = 1.3)
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
abline(h=0, v=0, lty=2)
gam.check(gam_midinc_no_2sd)