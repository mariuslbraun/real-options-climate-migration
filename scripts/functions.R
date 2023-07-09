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
library(tidymv)

# clear workspace
rm(list = ls())

# estimate and save GAM
estimate_GAM = function(sample, formulae, model_type) {
  # create data frame name
  df_name = paste(
    "df",
    sample,
    sep = "_"
  )
  
  # get regression formula
  formulae = as.formula(formulae)
  
  # create model name
  model_name = paste(
    model_type,
    sample,
    sep = "_"
  )
  
  # estimate model
  gc()
  model = mgcv::gam(
    formula = formulae,
    family = Gamma(link="log"),
    data = get(df_name),
    method = "REML"
  )
  
  # save model as RDS file
  saveRDS(
    model,
    paste0(
      "models/main_results/",
      model_name,
      ".rds"
    )
  )
  
  # return model
  return(model)
}

# plot smooth and save as PDF
show_gam = function(sample, model_type, climate_var, model) {
  plot_name = paste(
    model_type,
    climate_var,
    sample,
    sep = "_"
  )
  
  pdf(
    paste0(
      "figures/main_results/",
      plot_name,
      ".pdf"
    )
  )
  plot_smooth(
    x = model,
    view = climate_var,
    xlab = ifelse(
      climate_var == "temp_anom",
      "Temperature anomalies",
      "Precipitation anomalies"
    ),
    ylab = "log of bilateral migration rates",
    h0 = 0,
    v0 = 0,
    rm.ranef = T
  )
  dev.off()
}

compare_GAM_GLM = function(sample) {
  compareML(
    get( # GAM
      paste(
        "gam",
        sample,
        sep = "_"
      )
    ),
    get( # GLM
      paste(
        "glm",
        sample,
        sep = "_"
      )
    )
  )
}