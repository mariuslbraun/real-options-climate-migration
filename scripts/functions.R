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

#### load data frames ####
get_df =  function(sample){
  df = readRDS(
      paste0(
        "prepared/",
        paste(
          "df",
          sample,
          sep = "_"
        ),
        ".rds"
      )
    )
  return(df)
}

#### estimate and save GAM ####
estimate_gam = function(sample, formulae, model_type, by, directory, smoothing_method) {
  # create data frame name
  df_name = paste(
    "df",
    sample,
    sep = "_"
  )
  
  # handle smoothing method argument
  if(missing(smoothing_method)) {
    smooth_method = "REML"
  } else {
    smooth_method = paste0(smoothing_method, "")
  }
  
  # check whether directory is given and whether it already exists
  if(missing(directory)) {
    subdir = file.path("models", "main_results")
  } else {
    subdir = file.path("models", directory)
  }
  dir.create(subdir, showWarnings = F)
  
  # get regression formula
  formulae = as.formula(formulae)
  
  # create model name
  if(missing(by)) { # no interaction term provided
    # create model name
    model_name = paste(
      ifelse(
        missing(model_type),
        "gam",
        model_type
      ),
      sample,
      sep = "_"
    )
    if(
      missing(directory) |
      str_detect(directory, "lowinc") |
      str_detect(directory, "no_2sd") |
      str_detect(directory, "agri")
    ) {
      model_name = paste0(model_name, "")
    } else {
      model_name = paste(model_name, directory, sep = "_")
    }
  } else { # interaction term provided
    
    # create model name
    model_name = paste(
      ifelse(
        missing(model_type),
        "gam",
        model_type
      ),
      sample,
      by,
      sep = "_"
    )
  }
  
  
  # estimate model
  gc()
  model = mgcv::gam(
    formula = formulae,
    family = Gamma(link="log"),
    data = get(df_name),
    method = smooth_method
  )
  
  # save model as RDS file
  if(missing(by)){ # no interaction term provided
    saveRDS(
      model,
      file.path(subdir, paste0(model_name, ".rds"))
    )
    
    # diagnostic checks for GAM
    gam.check(model)
    
    # return model
    return(model)
  } else { # interaction term provided
    subdir = file.path("models", by)
    dir.create(subdir, showWarnings = F)
    
    saveRDS(
      model,
      file.path(subdir, paste0(model_name, ".rds"))
    )
    
    # diagnostic checks for GAM
    gam.check(model)
    
    # return model
    return(model)
  }
}

#### plot smooth and save as PDF ####
show_gam = function(sample, climate_var, model, by, directory) {
  # check whether directory is given and whether it already exists
  if(missing(directory)) {
    subdir = file.path("figures", "main_results")
  } else {
    subdir = file.path("figures", directory)
  }
  dir.create(subdir, showWarnings = F)
  
  # no interaction term provided
  if(missing(by)) {
    plot_name = paste(
      "gam",
      climate_var,
      sample,
      sep = "_"
    )
    
    if(
      missing(directory) |
      str_detect(directory, "lowinc") |
      str_detect(directory, "no_2sd") |
      str_detect(directory, "agri")
    ) {
      plot_name = paste0(plot_name, "")
    } else {
      plot_name = paste(plot_name, directory, sep = "_")
    }
    
    pdf(
      file.path(subdir, paste0(plot_name, ".pdf"))
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
  } else { # interaction term provided
    plot_name = paste(
      "gam",
      climate_var,
      sample,
      by,
      sep = "_"
    )
    
    subdir = file.path("figures", by)
    dir.create(subdir, showWarnings = F)
    
    pdf(
      file.path(subdir, paste0(plot_name, ".pdf"))
    )
    
    plot_smooth(
      x = model,
      view = climate_var,
      plot_all = by,
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
}

#### Chi-squared test comparing GAM and GLM ####
compare_gam_glm = function(sample) {
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