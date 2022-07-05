### Review priors and launch ST-GPR
## mean_snake_species was significnat!!!
source('[DIRECTORY]/get_age_metadata.R')

locs <- get_location_metadata(22, gbd_round_id = 6)
md <- locs[most_detailed == 1]$location_id
ages <- get_age_metadata(12, 6)
ages_7 <- get_age_metadata(12, 7)

# Check age groups
prior1 <- fread('[DIRECTORY]/noise_reduced_snake_prior_test_yes_prior_sign2020_02_10.csv')
prior2 <- fread('[DIRECTORY]/noise_reduced_snake_prior_test_yes_prior_sign2020_02_12.csv')
data <- fread('[DIRECTORY]/snake_NR_rate_data.csv')



library(data.table)
central_root <- '[DIRECTORY]'
setwd(central_root)

source('r_functions/registration/register.R')
source('r_functions/registration/sendoff.R')
source('r_functions/utilities/utility.r')


# Arguments
# Take in an me_name to run an ST-GPR model easily
# venom_type takes 'other', 'snake', 'scorpion', 'bees', 'spider'
path_to_config <- '[DIRECTORY]/noise_reduced_config.csv'
model_id_ref <- fread(path_to_config)



# Given a venom_type, registers an ST-GPR model
prep_stgpr <- function(venom_type, run = TRUE){
  ME <- paste0('prop_inj_animal_venom_', venom_type)
  my_model_id <- model_id_ref[me_name == ME]$model_index_id
  me_name <- paste0('prop_inj_animal_venom_', venom_type)
  decomp_step <- 'iterative'
  #crosswalk_version_id <- 908612340
  holdouts <- 0 # Keep at 0 unless you want to run cross-validation
  draws <- 0
  project <- 'proj_injuries'
  
  # Registration
  run_id <- register_stgpr_model(
    #me_name = me_name,
    path_to_config = path_to_config,
    model_index_id = my_model_id
    #decomp_step = decomp_step,
    #holdouts = holdouts,
    #draws = draws
    #crosswalk_version_id = crosswalk_version_id
  )
  if(run == TRUE){
    stgpr_sendoff(run_id, project)
  }
  
  # Sendoff
  return(run_id)
}

run_snake <- prep_stgpr('snake', run = T)
run_scorpion <- prep_stgpr('scorpion', run = T)
run_other <- prep_stgpr('other', run = T)
run_bees <- prep_stgpr('bees', run = T)
run_spider <- prep_stgpr('spider', run = T)
