###########################################################################################################
### Project: ST-GPR
### Purpose: Launch custom first stage prior testing and assemble for upload to ST-GPR
### Overview: This script launches the function launch_prior() that in turn runs two functions: test_prior() and assemble_prior(). 
###           Together they allow the creation of an ensemble linear prior for STGPR.
### test_prior()
###   This function runs all combinations of provided covariates on a provided dataset
###   It ranks each sub-model by out-of-sample predictive validity and marks models containing insignificant betas and/or betas that represent the 'wrong' relationship with your outcome
###   Each added covariate adds a significant number of models to test. Only include covariates that may have a reasonable relationship with your outcome
###   The output is a list that includes a dataframe with a row for each model, the fit betas/SE, and whether or not that model violated significance or prior signs
###
### assemble_prior()
###   This function creates out-of-sample predictive validity-weighted predictions of your outcome using the results from the test_prior() function
###   This can be included as a 'cv_custom_prior' in an STGPR data upload
###   This function can also plot a time-series of predictions for submodels, top weighted model, and the ensemble model
###########################################################################################################

######################################
############### SET-UP ###############
######################################
rm(list=ls())

v <- commandArgs()[5]
print(v)

os <- .Platform$OS.type
if (os=="windows") {
  j <- "J:/"
  h <- "H:/"
  k <- "K:/"
} else {
  lib_path <- "[DIRECTORY]"
  username <- Sys.info()[["user"]]
  j <- "/home/j/"
  h <- paste0("/homes/",username)
  k <- "[DIRECTORY]"
}
date<-gsub("-", "_", Sys.Date())

# Load Packages and Functions
pacman::p_load(data.table, ini)
source("[DIRECTORY]/launch.R")
source('[DIRECTORY]/launch_prior_nick.R')
######################################
############## SET ARGS ##############
######################################

## Required
#me <- "prop_inj_animal_venom_snake"               # ST-GPR me name
decomp_step <- "iterative"          # Decomp step (for pulling covariates)
#crosswalk_version_id <- NA    # Crosswalk version id (for input pulling data)
#path_to_data <- '[DIRECTORY]/inj_animal_venom_snake_prop_model.csv'  # Path to data (custom data csv if not using a crosswalk version)

## Functions
test_mods <- TRUE               # Whether to run the test_prior() function.  Not necessary if you've saved a previous run and just want to run assemble_prior()
average <- TRUE                 # Whether to run the assemble_prior() function. Not necessary if you just want to run test_prior()
plot_aves <- FALSE               # Whether to plot diagnostics for model averages
age_trend <- FALSE              # Whether to plot diagnostics for model averages by age instead of year
plot_betas <- TRUE              # Whether to plot diagnostics for ensemble betas

## Model Covariates
covs <- c('Healthcare access and quality index', 
          'Rainfall Population-Weighted (mm/yr)', 
          'Urbanicity', 
          'Proportion of population involved in agricultural activities', 
          'Population-weighted mean temperature', 
          'Socio-demographic Index',
          'Absolute value of average latitude', 
          'Education (years per capita)', 
          'Elevation Over 1500m (proportion)',
          'Elevation Under 100m (proportion)', 
          'Log-transformed age-standardized SEV scalar: Venom',
          'Population Density (over 1000 ppl/sqkm, proportion)', 
          'Population Density (under 150 ppl/sqkm, proportion)',
          'LDI (I$ per capita)',
          'Proportion of population vulnerable to venomous snakebites',
          'Mean number of venomous snake species')
cov_ids <- c(1099, 127, 854, 1087, 71, 881, 3, 33, 
             109, 121, 
             829, 118, 119, 
             57, 
             2318, 2321)   # List of covariate IDs to test for predictive validity in ensemble
prior_sign <- c(-1, 0, -1, 1, 0, -1, 0, -1,
                0, 0, 
                1, -1, 1,
                -1,
                1, 1)     # Prior directions for covariates, in order of list cov_ids() (-1 is negative correlation, 1 is positive correlation, 0 is no prior)

# Check if covariates look right
cov_ref <- data.table(covariate_name = covs,
                      ids = cov_ids,
                      signs = prior_sign)
print(cov_ref)

# Drop snake covariates for non-snake models
if(v != 'snake'){
  covs <- covs[1:(length(covs)-2)]
  cov_ids <- cov_ids[1:(length(covs)-2)]
  prior_sign <- prior_sign[1:(length(covs)-2)]
}

custom_covs <- NULL # List of character vectors containing 1) the custom covariate_name_short and 2) the filepath where the custom covariate is found
#     E.g. custom_covs=list(c("hemog_mean", "[DIRECTORY]/hemog_mean.csv"), c("sbr_mean", "[DIRECTORY]/sbr_mean.csv"))
polynoms <- NULL
#polynoms <- "log(LDI_pc)"                # String of polynomial transformations for covariates using covariate_name_short (e.g. c("sdi^2"))
ban_pairs <- NULL               # List of covariate IDs you wish to ban from occurring together in a sub-model (list of covariate IDs)
fixed_covs <- NULL              # Any covariates you want included in every model (i.e. age fixed effects)
random_effects <- c("(1|region_name/location_id)")  # Any random effects you want included in every model

## Model Settings
modtype <- "lmer"               # Linear model type (lmer if using random effects, lm otherwise)
rank_method <- "oos.rmse"       # OOS error method by which to rank sub-models. Options are Out-of-Sample RMSE ("oos.rmse") or Akaike Information Criterion ("aic")
n_mods <- 200                   # Number of top models to average over. More models is more computationally intensive, and there is a diminishing return
forms_per_job <-50             # Number of model forms to test for each parallelized cluster job
by_sex <- TRUE                  # Whether your model is sex-specific
data_transform <- "log"       # Transform function for your data (usually "logit" for proportions, "log" for continuous variables)
kos <- 5                       # Number of knock-outs for each sub-model
ko_prop <- 0.15                 # Proportion of data you want held out for each ko

## Cluster
proj <- "proj_injuries"         # Cluster project for launching jobs
m_mem_free <- 5                # Gigabytes of memory per job launched (depends on forms_per_job)

######################################
############### LAUNCH ###############
######################################
all_data <- fread('[DIRECTORY]/all_data.csv') # Raw data


path_to_data <- paste0('[DIRECTORY]/', v, '_NR_rate_data.csv')
me <- paste0('noise_reduced_', v)
crosswalk_version_id <- paste0(v, '2')

print(v)
print(crosswalk_version_id)

launch_prior(me=me, decomp_step=decomp_step, crosswalk_version_id=crosswalk_version_id, test_mods=TRUE,
             average=average, n_mods=n_mods, plot_aves=plot_aves, plot_betas=plot_betas, age_trend=age_trend,
             cov_ids=cov_ids, prior_sign=prior_sign, ban_pairs=ban_pairs, polynoms=polynoms, modtype=modtype,
             rank_method=rank_method, forms_per_job=forms_per_job, drop_nids=FALSE, fixed_covs=fixed_covs,
             custom_covs=custom_covs, random_effects=random_effects, username=username, by_sex=by_sex, path_to_data = path_to_data,
             pred_ages = c(2:5, 388:389, 238, 34, 6:20, 30:32, 235))
