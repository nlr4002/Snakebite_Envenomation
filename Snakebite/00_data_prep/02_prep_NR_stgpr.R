## Prep ST-GPR with noise_reduction files

library(data.table)
library(stringr)
library(ggplot2)
library(magrittr)

source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_envelope.R')
source('[DIRECTORY]/get_population.R')
locs <- get_location_metadata(35, gbd_round_id = 6)

files <- Sys.glob('[DIRECTORY]/noise_reduced_data/*.csv')
VR_files <- Sys.glob('[DIRECTORY]/noise_reduced_data/VR-*.csv')
files1 <- VR_files[1:680]
files2 <- VR_files[730:length(VR_files)]
VR_files <- c(files1, files2)

# Calculate floor by age-sex-year-venom
VR_floor <- rbindlist(lapply(VR_files, fread), fill = TRUE)
VR_floor[, actual_model_group := NULL]

# Merge on reference model_groups to get rid of duplicates
ref <- fread('[DIRECTORY]/model_group_reference.csv')
ref[, model_group := NULL]
VR_floor <- merge(VR_floor, ref, by = c('ihme_loc_id', 'source_name'), all.x = TRUE)


# Format the data for ST-GPR:
all_data <- rbindlist(lapply(files, fread), fill = TRUE)


all_data <- all_data[, c('year_id', 'location_id', 'iso3', 'age_group_id', 'sex_id', 'nid', 'source_name', 'code_system', 
                         'model_group', 'venom', 'ihme_loc_id', 
                         'sample_size', 'deaths', 'pred_deaths', 'std_err_deaths', 'cf', 'std_error', 'mean', 'variance')]


# Merge on reference model_groups to get rid of duplicates
ref <- fread('/share/injuries/envenomation/noise_reduction/model_group_reference.csv')
ref[, model_group := NULL]
all_data <- merge(all_data, ref, by = c('ihme_loc_id', 'source_name'), all.x = TRUE)

# Subset
all_data <- all_data[actual_model_group == model_group]




square_data_ref <- fread('/share/injuries/envenomation/data/modeling_data/square_garbage_code_data.csv')
model_group_ref <- fread('/share/injuries/envenomation/data/modeling_data/venom_model_groups.csv')

stopifnot(nrow(model_group_ref) == nrow(all_data))


# Transform to rate space as we do before
# Need the mortality envelope
mort <- get_envelope(location_id = unique(all_data$location_id), sex_id = c(1,2),
                       age_group_id = unique(all_data$age_group_id), year_id = unique(all_data$year_id),
                       decomp_step = 'step1', gbd_round_id = 7)
setnames(mort, c('mean', 'upper', 'lower'), c('mean_env', 'upper_env', 'lower_env'))

setnames(all_data, 'mean', 'data')

all_data <- all_data[, c('location_id', 'year_id', 'age_group_id', 'sex_id', 'nid', 'model_group', 'venom', 'sample_size',
                         'deaths', 'pred_deaths', 'std_err_deaths', 'cf', 'std_error', 'data', 'variance')]

pop <- get_population(location_id = unique(all_data$location_id), sex_id = c(1,2),
                      age_group_id = unique(all_data$age_group_id), year_id = unique(all_data$year_id),
                      decomp_step = 'step1', gbd_round_id = 7)

all_data <- merge(all_data, pop, by = c('location_id', 'age_group_id', 'sex_id', 'year_id'), all.x = TRUE)
all_data <- merge(all_data, mort, by = c('location_id', 'age_group_id', 'sex_id', 'year_id'), all.x = TRUE)

# Calculate rates - data is noise reduced cause fraction.
all_data[data > 1, data := 0]
all_data[, rate := (data*mean_env)/population] # Rate = cause_fraction (data)*mortality envelope over population
all_data[, rate_per_100k := rate*100000]
all_data[rate_per_100k == 0 | rate_per_100k < 0.01, rate_per_100k := 0.01]
all_data[, val := rate_per_100k]

# Variance on binomial calculation between cause fraction and sample_size
all_data[, variance := sample_size*data*(1-data)] # Variance around the cause fraction
all_data[, variance := ((variance*mean_env)/population)*100000]
all_data[variance == 0, variance := 0.001]

all_data[, measure := 'continuous']
all_data[, is_outlier := 0]


all_data <- all_data[, c('location_id', 'age_group_id', 'sex_id', 'year_id', 'nid', 'model_group',
                         'venom', 'sample_size', 'variance', 'val', 'measure', 'is_outlier')]
all_data[, data := val] # For the linear prior
all_data[variance < 0.0001, variance := 0.0001]

all_data[variance > val, variance := 0.5*val]


for(v in unique(all_data$venom)){
  dt <- all_data[venom == v]
  dt <- unique(dt)
  write_csv(dt, paste0('/ihme/injuries/envenomation/noise_reduction/noise_reduced_stgpr/', v, '_NR_rate_data.csv'))
}

write_csv(all_data, '/share/injuries/envenomation/noise_reduction/noise_reduced_stgpr/all_data.csv')






