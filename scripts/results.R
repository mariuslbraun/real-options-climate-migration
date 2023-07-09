# This R code produces the main results for the paper "A Real-Options 
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
library(tidymv)

# clear workspace
rm(list = ls())

# load functions
source("scripts/functions.R")

# load data frames
samples = c("total", "lowinc", "midinc")
for(i in 1:length(samples)) {
  df_name = paste(
    "df",
    samples[i],
    sep = "_"
  )
  assign(
    x = df_name,
    value = readRDS(
      paste0(
        "prepared/",
        df_name,
        ".rds"
      )
    )
  )
}
rm(i, df_name)



#### 2.1 Main results ####

# formulas for regression models
baseline_formula_gam = "mig_rate_new ~ s(temp_anom, bs='cr') + s(precip_anom, bs='cr') + period + X...origin + destination"
baseline_formula_glm = "mig_rate_new ~ temp_anom + precip_anom + period + X...origin + destination"

model_types = c("gam", "glm")
climate_vars = c("temp_anom", "precip_anom")
# estimate GAMs for total, low-income and middle-income sample
# (and GLMs for comparison)
tic()
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
      value = estimate_GAM(
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
        show_gam(
          sample = samples[i],
          model_type = model_types[j],
          climate_var = climate_vars[k],
          model = get(model_name)
        )
      }
    }
  }
  
  # Chi-squared test comparing GAM and GLM
  compare_GAM_GLM(samples[i])
}
toc()
rm(i, j, model_name)



#### 2.2 Additional results 1: interaction terms for contiguity ####

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



#### 2.3 Additional results 2: interaction terms for OECD destination ####

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
