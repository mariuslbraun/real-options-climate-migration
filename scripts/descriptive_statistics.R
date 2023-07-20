# This R code produces descriptive statistics as well as some density plots 
# for the paper "A Real-Options Analysis of Climate Change and International Migration".
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

# load dataset (non-OECD countries only)
df_total = read.csv("prepared\\Dataset_final.csv")
df_total$mig_rate_new = df_total$mig_rate * 100
saveRDS(df_total, "prepared/df_total.rds")

# create separate datasets for low- and middle-income countries
inc_types = c("lowinc", "midinc")
for(i in 1:length(inc_types)) {
  df_name = paste(
    "df",
    inc_types[i],
    sep = "_"
  )
  df = df_total %>%
    filter(
      low_income == as.numeric(inc_types[i] == "lowinc")
    )
  assign(
    x = df_name,
    value = df
  )
  saveRDS(df,
          paste0(
            "prepared/",
            df_name,
            ".rds"
          )
  )
}
rm(i, df, df_name)



#### 1.1 Density plot of migration rates ####
# migration rates scaled at and distribution cut off at 10 
# for illustrative purposes
df_total$mig_rate_scaled = df_total$mig_rate * 1000000
new_df = df_total %>%
  filter(
    mig_rate_scaled < 10
    )
d = density(new_df$mig_rate_scaled)
pdf("figures/density_plots/density_mig_rate.pdf")
par(mar = c(5, 4, 2, 2) + 0.1)
plot(d, main = "")
abline(v = mean(new_df$mig_rate_scaled), lty = 2)
dev.off()
df_total = df_total %>% 
  dplyr::select(
    -mig_rate_scaled
    )
rm(d, new_df)



#### 1.2 Descriptive statistics of main variables ####
# vector of variable names
vars = c(
  "mig_rate_new",
  "temp_anom",
  "precip_anom",
  "share_temp_greater_1SD",
  "share_precip_less_1SD",
  "gdp_pc_origin"
  )

# iterate over variables and print mean, standard deviation
# and number of observations
for(i in 1:length(inc_types)) {
  for(j in 1:length(vars)) {
    df_name = paste(
      "df",
      inc_types[i],
      sep = "_"
    )
    cat(
      vars[j],
      mean(
        get(df_name)[vars[j]][, ],
        na.rm = T
      ),
      sd(
        get(df_name)[vars[j]][, ],
        na.rm = T
      ),
      sum(
        !is.na(
          get(df_name)[vars[j]][, ]
        )
      ),
      "\n",
      sep = " "
    )
  }
}
rm(i, j, df_name)

# migration rates to neighboring and OECD countries
contig_types = c("contig", "noncontig")
OECD_dest_types = c("OECD_dest", "nonOECD_dest")
for(i in 1:length(inc_types)) {
  df_name = paste(
    "df",
    inc_types[i],
    sep = "_"
  )
  # contiguity
  for(j in 1:length(contig_types)) {
    var_name = paste(
      "mig_rate",
      inc_types[i],
      contig_types[j],
      sep = "_"
    )
    var_value = get(df_name) %>%
      filter(contiguity == as.numeric(
        contig_types[j] == "contig"
        )
      ) %>%
      dplyr::select(mig_rate_new)
    
    assign(
      x = var_name,
      value = var_value
    )
    cat(
      var_name,
      mean(var_value[, 1], na.rm = T),
      sd(var_value[, 1], na.rm = T),
      "\n",
      sep = " "
    )
  }
  
  # OECD destination countries
  for(k in 1:length(OECD_dest_types)) {
    var_name = paste(
      "mig_rate",
      inc_types[i],
      OECD_dest_types[k],
      sep = "_"
    )
    var_value = get(df_name) %>%
      filter(OECD_dest == as.numeric(
        OECD_dest_types[k] == "OECD_dest"
        )
      ) %>%
      dplyr::select(mig_rate_new)

    assign(
      x = var_name,
      value = var_value
    )
    cat(
      var_name,
      mean(var_value[, 1], na.rm = T),
      sd(var_value[, 1], na.rm = T),
      "\n",
      sep = " "
    )
  }
}
rm(i, j, k, df_name, var_name, var_value)

# t-test difference between means of low- and middle-income countries
for(i in 1:length(vars)) {
  assign(
    x = paste(
      "ttest",
      vars[i],
      sep = "."
    ),
    value = t.test(
      df_lowinc[vars[i]][, ],
      df_midinc[vars[i]][, ]
    )
  )
}
rm(i)



#### 1.3 Density plots of temperature and precipitation anomalies ####
climate_vars = c("temp_anom", "precip_anom")
for(i in 1:length(inc_types)) {
  for(j in 1:length(climate_vars)) {
    # compute density
    density_name = paste(
      "density",
      climate_vars[j],
      inc_types[i],
      sep = "_"
    )
    climate_var = get(
      paste(
        "df",
        inc_types[i],
        sep = "_"
      )
    )[climate_vars[j]][, ]
    
    assign(
      x = density_name,
      value = density(
        climate_var
      )
    )
    # create plot and save as PDF file
    pdf(
      paste0(
        "figures/density_plots/",
        density_name,
        ".pdf"
      )
    )
    par(mar = c(5, 4, 2, 2) + 0.1)
    plot(get(density_name), main = "")
    abline(v = mean(climate_var), lty = 2)
    dev.off()
    
    # skewness and kurtosis of climatic variables
    cat(
      "Skewness and kurtosis of ",
      inc_types[i],
      climate_vars[j],
      "\n",
      skewness(climate_var),
      kurtosis(climate_var),
      "\n"
    )
  }
}
rm(i, j, density_name, climate_var)

# countries that experienced extreme temperature and precipitation anomalies
# (+2 sd and -2 sd respectively)
for(i in 1:length(inc_types)) {
  for(j in 1:length(climate_vars)) {
    df_name = paste(
      "df",
      inc_types[i],
      sep = "_"
    )
    
    sign = ifelse(
      climate_vars[j] == "temp_anom",
      "plus",
      "minus"
    )
    
    extreme_name = paste(
      "countries",
      inc_types[i],
      climate_vars[j],
      sign,
      "2sd",
      sep = "_"
    )
    
    # get countries that experienced extreme temperature and precipitation anomalies
    assign(
      x = extreme_name,
      value = ifelse(
        climate_vars[j] == "temp_anom",
        # temperature extremes
        (get(df_name) %>%
           filter(
             temp_anom > (mean(temp_anom) + 2 * sd(temp_anom))
             )
         ),
        # precipitation extremes
        (get(df_name) %>%
           filter(
             precip_anom < (mean(precip_anom) - 2 * sd(precip_anom))
             )
         )
      )
    )
    extreme_share = (
      nrow(as.data.frame(get(extreme_name))) /
        length(get(df_name)[climate_vars[j]][, ])
    ) * 100
    cat(
      extreme_name,
      extreme_share,
      "\n",
      sep = " "
    )
  }
}
rm(i, j, df_name, extreme_name, extreme_share)