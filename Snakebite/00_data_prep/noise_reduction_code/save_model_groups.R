library(data.table)
library(magrittr)

source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_population.R')

locs <- get_location_metadata(35, gbd_round_id = 6)
iso3s <- locs[level == 3]$ihme_loc_id %>% unique()
regions <- locs[level == 2]$ihme_loc_id %>% unique()
super_region_ids <- locs[level == 1]$location_id %>% unique() %>% as.character()
super_region_to_region_ids <- unique(locs[level == 2, c('region_id', 'super_region_id')])
regions_to_ids <- locs[level == 2, c('region_id', 'ihme_loc_id')]
level_three_location_ids <- locs[level == 3]$location_id

# location ID to region and super region ihme_loc_id
super_region_locs <- unique(locs[level == 1, c('super_region_name', 'ihme_loc_id')]) %>% setnames('ihme_loc_id', 'super_region_id')
region_locs <- unique(locs[level == 2, c('region_name', 'ihme_loc_id')]) %>% setnames('ihme_loc_id', 'region_id')

loc_df <- merge(locs[, c('ihme_loc_id', 'location_id', 'region_name', 'super_region_name')],
                region_locs, by = 'region_name')
loc_df <- merge(loc_df, super_region_locs, by = 'super_region_name')

# Special VA sources
IND_SRS_SOURCES = c("India_SRS_states_report", "India_SRS_Maternal_states")
IDN_SRS_SOURCES = c("Indonesia_SRS_2014", "Indonesia_SRS_province")
MATLAB_SOURCES = c('Matlab_1963_1981', 'Matlab_1982_1986', 'Matlab_1987_2002',
                  'Matlab_2003_2006', 'Matlab_2007_2012', 'Matlab_2011_2014')


# This main function basically just pulls data that's similar if the model_group is region or needs something else

df <- fread('[DIRECTORY]/venom_model_groups.csv')

# Create model group filters, for aggregating data based on the model group
# LIke if it's a regional thing, get the rest of the data in the region


# Get the data you want to model

ref_file <- data.table()
for(model_group in unique(df$model_group)){
  # Get locations if the model_group isn't at national level
  loc_code <- model_group
  loc_code <- gsub('VR-', '', loc_code)
  loc_code <- gsub('VA-', '', loc_code)
  
  # VR completely siloed from VA
  if(model_group %like% 'VR-'){
    if(loc_code %in% iso3s){
      model_df <- df[iso3 == loc_code & model_group %like% 'VR']
    } else if(loc_code %in% unique(loc_df$region_id)){
      model_df <- df[region_id == loc_code & model_group %like% 'VR']
    } 
  }
  
  # VA sources are kept completely separate from VR sources
  if(model_group %like% 'VA-'){
    if(model_group == 'VA-SRS-IND'){
      model_df <- df[source_name %in% IND_SRS_SOURCES]
    } else if(model_group == 'VA-SRS-IDN'){
      model_df <- df[source_name %in% IDN_SRS_SOURCES]
    } else if(model_group == 'VA-Matlab'){
      model_df <- df[source_name %in% MATLAB_SOURCES]
    } else if(model_group == 'VA-IND'){
      model_df <- df[iso3 == 'IND' & model_group %like% 'VA']
    } else if(loc_code %in% unique(loc_df$super_region_id)){
      model_df <- df[super_region_id == loc_code & model_group %like% 'VA']
    }
  }
  
  print(model_group)
  print(nrow(model_df))
  
  model_group2 <- model_group
  model_df[, actual_model_group := model_group2]
  
  ref <- unique(model_df[, c('ihme_loc_id', 'source_name', 'model_group', 'actual_model_group')])
  ref_file <- rbind(ref_file, ref)
  
  model_df[, model_group := model_group2]
  write_csv(model_df, paste0('[DIRECTORY]', 
                             model_group, '.csv'))
}

# To remove duplicates later
ref_file <- ref_file[model_group == actual_model_group]
write_csv(ref_file, '[DIRECTORY]/model_group_reference.csv')
