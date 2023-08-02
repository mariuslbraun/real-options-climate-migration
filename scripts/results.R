# This R code produces the main results for the paper "A Real-Options 
# Analysis of Climate Change and International Migration".
# The paper is available via SSRN: https://papers.ssrn.com/abstract=3951942
# Data is also available via Mendeley Data: https://doi.org/10.17632/7f5mmwxcpm.2

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
library(stringr)

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



#### 2.1 Main results ####

# formulas for regression models
baseline_formula_gam = "mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period + X...origin + destination"
baseline_formula_glm = "mig_rate_new ~ temp_anom + precip_anom + period + X...origin + destination"

model_types = c("gam", "glm")
climate_vars = c("temp_anom", "precip_anom")
# estimate GAMs for total, low-income and middle-income sample
# (and GLMs for comparison)
for(i in 1:length(samples)) {
  for(j in 1:length(model_types)) {
    # model name
    model_name = paste(
      model_types[j],
      samples[i],
      sep = "_"
    )
    # estimate GAM
    assign(
      x = model_name,
      value = estimate_gam(
        sample = samples[i],
        formulae = get(
          paste(
            "baseline_formula",
            model_types[j],
            sep = "_"
          )
        ),
        model_type = model_types[j]
      )
    )
    
    # plot smooths
    for(k in 1:length(climate_vars)) {
      if(model_types[j] == "gam") {
        plot = show_gam(
          sample = samples[i],
          climate_var = climate_vars[k],
          model = get(model_name)
        )
      }
    }
  }
  
  # Chi-squared test comparing GAM and GLM
  compare_gam_glm(samples[i])
}
rm(i, j, k, model_name, plot)



#### 2.2 Additional results 1: interaction terms for contiguity and OECD destinations ####

# formula for regression models
contiguity_formula_gam = "mig_rate_new ~ contiguity + s(temp_anom, bs='cr')  + s(temp_anom,by = contiguity, bs='cr') + s(precip_anom, bs='cr')+ s(precip_anom, by = contiguity, bs='cr') + period + X...origin + destination"
OECD_dest_formula_gam = "mig_rate_new ~ OECD_dest + s(temp_anom, bs='cr') + s(temp_anom, by = OECD_dest, bs='cr') + s(precip_anom, bs='cr') + s(precip_anom, by = OECD_dest, bs='cr') + period + X...origin + destination"

# vector of interaction names
interactions = c("contiguity", "OECD_dest")
for(i in 2:length(samples)) {
  for(j in 1:length(interactions)) {
    # model name
    model_name = paste(
      "gam",
      samples[i],
      interactions[j],
      sep = "_"
    )
    
    # estimate GAM
    assign(
      x = model_name,
      value = estimate_gam(
        sample = samples[i],
        formulae = get(
          paste(interactions[j], "formula_gam", sep = "_")
          ),
        by = interactions[j]
      )
    )
    
    # plot smooths
    for(k in 1:length(climate_vars)) {
      plot = show_gam(
        sample = samples[i],
        climate_var = climate_vars[k],
        model = get(model_name),
        by = interactions[j]
      )
    }
  }
}
rm(i, j, k, model_name, plot)