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

# formula for regression models
baseline_formula_gam = "mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period + X...origin + destination"
# vector of climate variable names
climate_vars = c("temp_anom", "precip_anom")



#### 3.1: vary thresholds for low-income countries ####
thresholds = c("0.2", "0.3")
for(i in 2:length(samples)) {
  for(j in 1:length(thresholds)) {
    # create low- and middle-income samples with alternative thresholds
    sample_name = paste0(samples[i], thresholds[j])
    lowinc_dummy_name = paste0("low_income", thresholds[j])
    assign(
      x = paste("df", sample_name, sep = "_"),
      value = df_total[
        which(
          df_total[lowinc_dummy_name][, ] == as.numeric(samples[i] == "lowinc")
          ), 
        ]
    )
    
    # model name
    model_name = paste("gam", sample_name, sep = "_")
    
    # estimate GAM
    assign(
      x = model_name,
      value = estimate_GAM(
        sample = sample_name,
        formulae = baseline_formula_gam,
        directory = paste0("lowinc", thresholds[j])
      )
    )
    
    # plot smooths
    for(k in 1:length(climate_vars)) {
      plot = show_gam(
        sample = sample_name,
        climate_var = climate_vars[k],
        model = get(model_name),
        directory = paste0("lowinc", thresholds[j])
      )
    }
  }
}
rm(i, j, k, sample_name, lowinc_dummy_name, model_name, plot)



#### 3.2: smoothing parameter selection using GCV instead of REML ####

# estimate GAMs for low-income and middle-income sample
tic()
for(i in 2:length(samples)) {
  # model name
  model_name = paste("gam", samples[i], "gcv", sep = "_")
  
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
controls = "log_gdp_pc_ratio_dest_origin + common_lang + log_dist + civil_war"
controls_formula_gam = paste(baseline_formula_gam, controls, sep = " + ")
tic()
for(i in 2:length(samples)) {
  # create log GDP per capita and log distance variables
  df_name = paste("df", samples[i], sep = "_")
  log_gdp_pc_ratio_dest_origin = log(get(df_name)["gdp_pc_ratio_dest_origin"][, ])
  log_dist = log(get(df_name)["dist"][, ])
  df_value = cbind(
    get(df_name),
    log_gdp_pc_ratio_dest_origin,
    log_dist
  )
  
  # model name
  model_name = paste(
    "gam", samples[i], "controls", sep = "_"
  )
  
  # estimate GAM
  assign(
    x = model_name,
    value = estimate_GAM(
      sample = samples[i],
      formulae = controls_formula_gam,
      directory = "controls"
    )
  )
  
  # plot smooths
  for(k in 1:length(climate_vars)) {
    plot = show_gam(
      sample = samples[i],
      climate_var = climate_vars[k],
      model = get(model_name),
      directory = "controls"
    )
  }
}
toc()
rm(i, k, model_name, plot, controls_formula_gam,
   controls, df_value, df_name)



#### 3.4: estimate GAM using heat and drought month shares ####

# formula for regression models 
shares_formula_gam = "mig_rate_new ~ s(share_temp_greater_1SD, bs='cr') + s(share_precip_less_1SD, bs='cr') + period + X...origin + destination"
climate_shares = c("share_temp_greater_1SD", "share_precip_less_1SD")
# estimate GAMs for low-income and middle-income sample
tic()
for(i in 2:length(samples)) {
    # model name
    model_name = paste("gam", samples[i], "shares", sep = "_")
    
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