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
library(mortcore, lib = "[DIRECTORY]")
library(data.table)

######################################
############### LAUNCH each venom in parallel###############
######################################
all_data <- fread('[DIRECTORY]/all_data.csv') # Raw data

## Run this in an interactive session
lapply(c('other', 'bees', 'spider', 'scorpion', 'snake'), function(v){
  print(v)
  path_to_data <- paste0('[DIRECTORY]/', v, '_NR_rate_data.csv')
  me <- paste0('noise_reduced_', v)
  
  qsub(jobname = paste0('ensemble_', v),
       code = '[DIRECTORY]/launch_prior_worker.R',
       pass = list(v),
       cores = 2,
       mem = 40,
       wallclock = "24:00:00",
       submit = T,
       log = T,
       archive_node = T,
       queue = "long",
       proj = "proj_injuries",
       shell = paste0('[DIRECTORY]/r_shell2.sh'))
  
  # launch_prior(me=me, decomp_step=decomp_step, crosswalk_version_id=crosswalk_version_id, test_mods=test_mods,
  #              average=average, n_mods=n_mods, plot_aves=plot_aves, plot_betas=plot_betas, age_trend=age_trend,
  #              cov_ids=cov_ids, prior_sign=prior_sign, ban_pairs=ban_pairs, polynoms=polynoms, modtype=modtype,
  #              rank_method=rank_method, forms_per_job=forms_per_job, drop_nids=FALSE, fixed_covs=fixed_covs,
  #              custom_covs=custom_covs, random_effects=random_effects, username=username, by_sex=by_sex, path_to_data = path_to_data,
  #              pred_ages = unique(all_data$age_group_id))
})

# Make custom log(LDI) covariate
ldi <- get_covariate_estimates(57, gbd_round_id = 6, decomp_step = 'step4')
ldi[, mean_value := log(mean_value)]
write_csv(ldi, '[DIRECTORY]/log_ldi_pc.csv')


