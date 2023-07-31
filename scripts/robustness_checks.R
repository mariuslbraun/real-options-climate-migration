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
    
    rm(list = paste("df", sample_name, sep = "_"))
  }
}
rm(i, j, k, sample_name, lowinc_dummy_name, model_name, plot)



#### 3.2: smoothing parameter selection using GCV instead of REML ####
# estimate GAMs for low-income and middle-income sample
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
rm(i, k, model_name, plot)



#### 3.3: include economic control variables ####
# formula for regression models
controls = "log_gdp_pc_ratio_dest_origin + common_lang + log_dist + civil_war"
controls_formula_gam = paste(baseline_formula_gam, controls, sep = " + ")
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
rm(i, k, model_name, plot, controls_formula_gam,
   controls, df_value, df_name)



#### 3.4: estimate GAM using heat and drought month shares ####
# formula for regression models 
shares_formula_gam = "mig_rate_new ~ s(share_temp_greater_1SD, bs='cr') + s(share_precip_less_1SD, bs='cr') + period + X...origin + destination"
climate_shares = c("share_temp_greater_1SD", "share_precip_less_1SD")
# estimate GAMs for low-income and middle-income sample
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
rm(i, k, model_name, plot, shares_formula_gam, climate_shares)



#### 3.5: interaction between climatic anomalies and share of agricultural productivity ####
# create indicator of quartile in the distribution of share of agricultural GDP
agri_share_quantiles = quantile(df_lowinc$agri_share_gdp, na.rm = T)
df_total$agri_share_gdp_quantile = ifelse(df_total$agri_share_gdp <= agri_share_quantiles["25%"], "1",
                                   ifelse(df_total$agri_share_gdp > agri_share_quantiles["25%"] &
                                   df_total$agri_share_gdp <= agri_share_quantiles["50%"], "2",
                                   ifelse(df_total$agri_share_gdp > agri_share_quantiles["50%"] &
                                   df_total$agri_share_gdp <= agri_share_quantiles["75%"], "3", "4")))
df_total$agri_share_gdp_quantile = as.factor(df_total$agri_share_gdp_quantile)

# estimate models separately for the different quartiles
for(i in 1:(length(agri_share_quantiles) - 1)) {
  # create data frame
  df = df_total %>% filter(agri_share_gdp_quantile == i)
  sample_name = paste0("agri", i)
  assign(paste("df", sample_name, sep = "_"), df)
  
  # model name
  model_name = paste("gam", sample_name, sep = "_")
  
  # estimate GAM
  assign(
    x = model_name,
    value = estimate_GAM(
      sample = sample_name,
      formulae = baseline_formula_gam,
      directory = "agri"
    )
  )
  
  # plot smooths
  for(k in 1:length(climate_vars)) {
    plot = show_gam(
      sample = sample_name,
      climate_var = climate_vars[k],
      model = get(model_name),
      directory = "agri"
    )
  }
}
rm(i, k, n, df, df_name, agri_share_quantiles)

# GAM with interaction between climatic anomalies and factor variable indicating 
# quartile of agricultural GDP
agri_formula = "mig_rate_new ~ s(temp_anom, by = agri_share_gdp_quantile, bs = 'cr') + s(precip_anom, by = agri_share_gdp_quantile, bs = 'cr') + period + X...origin + destination"
gc()
gam_total_agri = mgcv::gam(
  formula = as.formula(agri_formula),
  family = Gamma(link="log"),
  data = df_total,
  method = "REML"
)
saveRDS(gam_total_agri, "models/agri/gam_total_agri.rds")



#### 3.6: exclude observations with extreme temperature and precipitation anomalies ####
for(i in 2:length(samples)) {
    # create low- and middle-income samples without extreme temperature and precipitation anomalies
    sample_name = paste(samples[i], "no_2sd", sep = "_")
    df_name = paste("df", samples[i], sep = "_")
    assign(
      x = paste("df", sample_name, sep = "_"),
      value = get(df_name) %>% filter(
        !(
          temp_anom > (mean(temp_anom) + 2 * sd(temp_anom)) |
          precip_anom < (mean(precip_anom) - 2 * sd(precip_anom))
        )
      )
    )
    
    # model name
    model_name = paste("gam", sample_name, sep = "_")
    
    # estimate GAM
    assign(
      x = model_name,
      value = estimate_GAM(
        sample = sample_name,
        formulae = baseline_formula_gam,
        directory = "no_2sd"
      )
    )
    
    # plot smooths
    for(k in 1:length(climate_vars)) {
      plot = show_gam(
        sample = sample_name,
        climate_var = climate_vars[k],
        model = get(model_name),
        directory = "no_2sd"
      )
    }
    rm(list = paste("df", sample_name, sep = "_"))
}
rm(i, k, sample_name, model_name, plot)