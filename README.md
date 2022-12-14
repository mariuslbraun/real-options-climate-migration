This repo contains R code as well as the dataset used for the empirical analysis of the research paper "A Real-Options Analysis of Climate Change and International Migration".
In the paper, semiparametric regression models are used to investigate nonlinear effects of climate change on international migration.
You can find the latest version of the paper on SSRN: https://papers.ssrn.com/abstract=3951942

Follow these steps to replicate the results of the paper:

## 1. Setting up the R environment

First, open the R project file "Real_Options_Climate_Migration.Rproj". It is important to work in the R project, as it is associated with the project folder and will set the working directory accordingly.
After opening the R project, open "setup.R". Running the file will set up an R environment using the `renv` package, which manages project-local R dependencies to ensure
that existing data analysis workflows work as they did before; for more information see https://rstudio.github.io/renv/articles/renv.html

Proceed as follows:

1. Call `options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/2022-08-01+Y3JhbiwyOjQ1MjYyMTU7RTY0MEEyRTM"))`
to load the CRAN snapshot from RStudio Package Manager. This will freeze to the R packages as they were available at the time that the analysis was conducted.

2. Install `renv` by calling `install.packages("renv")`.

3. Call `renv::init()` to initialize a new project-local environment with a private R library.
`bare = TRUE`: instead of installing dependencies automatically, we install packages manually.

4. Install the required packages.

5. Call `renv::snapshot()` to save the state of the project library.

## 2. Descriptive statistics

"Real_Options_Climate_Migration_Descriptive_Statistics.R" produces descriptive statistics and density plots of the main variables for low- and middle-income countries.
In addition, t-tests comparing low- and middle-income sample means for the main variables are provided.

## 3. Main Results

"Real_Options_Climate_Migration_Results.R" produces the main results of the paper. Separate data frames are created for low- and middle-income countries, and all
models are estimated separately for low- and middle-income countries.
The general workflow is the same for all of the regression models:

1.  The regression formula is stored as string `baseline_formula_gam` to be pasted into the model call.

2. `gc()` cleans up the R memory.

3. `gam_total = mgcv::gam(baseline_formula_gam, ...)` estimates a generalized additive model (GAM) using the `mgcv` package (Wood 2001). The estimation of the GAMs is quite computationally expensive,
and depending on the model, this may take up to a few hours to execute (hence the `tic()` and `toc()` commands surrounding the model call).

4. Next, plots of the smooth nonparametric functions of temperature and precipitation anomalies estimated by the models are produced:
```
# plot smooth nonparametric functions of temperature and precipitation anomalies
plot(gam_total, shade = TRUE, xlab = "Temperature anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
# label y-axis
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
plot(gam_total, shade = TRUE, xlab = "Precipitation anomalies", ylab = "", cex.lab = 1.4, cex.axis = 1.3)
# label y-axis
title(ylab = "log of bilateral migration rates", mgp=c(2.5,1,0), cex.lab = 1.4)
# add axes to plot
abline(h=0, v=0, lty=2)
```

5. `gam.check(gam_total)` produces diagnostic information about the fitting procedure and results.

Proceed analogously for all other models.

## 4. Robustness Checks

"Real_Options_Climate_Migration_Robustness_Checks.R" produces a host of robustness checks of the main results.
The general workflow for the models is identical to the one outlined for the main results.

## Sources

Wood, S.N. (2001), mgcv: GAMs and Generalized Ridge Regression for R, *R News* 1, 20-25.

