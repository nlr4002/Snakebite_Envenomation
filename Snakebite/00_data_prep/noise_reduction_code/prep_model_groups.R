### Model grouping in noise reduction
### See if I can do it here
### After squaring
library(data.table)
library(readr)

source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_population.R')
locs <- get_location_metadata(35, gbd_round_id = 7)
locs[, iso3 := substr(ihme_loc_id, 1,3)]
locs[level == 3, iso3_id := location_id]
iso3_locs <- locs[!is.na(iso3_id)]
super_region_locs <- unique(locs[level == 1, c('super_region_name', 'ihme_loc_id')]) %>% setnames('ihme_loc_id', 'super_region_id')
region_locs <- unique(locs[level == 2, c('region_name', 'ihme_loc_id')]) %>% setnames('ihme_loc_id', 'region_id')

df <- fread('[DIRECTORY]/square_garbage_code_data.csv')
df <- merge(df, locs[, c('location_id', 'ihme_loc_id', 'region_name', 'super_region_name')], by = 'location_id', all.x = TRUE)


# Aggregate NOR subnationals to just the national level
df_nor <- df[ihme_loc_id %like% 'NOR']
df_nor[, ihme_loc_id := 'NOR']
df_nor[, sample_size := sum(sample_size), 
       by = c('venom', 'nid', 'source_name', 'code_system', 
              'year_id', 'age_group_id', 'sex_id', 'ihme_loc_id')]
df_nor[, deaths := sum(deaths), 
       by = c('venom', 'nid', 'source_name', 'code_system', 
              'year_id', 'age_group_id', 'sex_id', 'ihme_loc_id')]
df_nor[, location_id := 90]
df_nor <- unique(df_nor)

df <- df[!(ihme_loc_id %like% 'NOR')]
df <- rbind(df, df_nor)



df[, iso3 := substr(ihme_loc_id, 1,3)]
df <- merge(df, iso3_locs[, c('iso3', 'iso3_id')], by = 'iso3')

df <- merge(df, super_region_locs, by = 'super_region_name')
df <- merge(df, region_locs, by = 'region_name')



max_pop <- get_population(location_id = locs[level == 3]$location_id, gbd_round_id = 6, 
                          age_group_id = 22, sex_id = 3, year_id = c(1980:2019), decomp_step = 'step4')
max_pop[, age_group_id := NULL]
max_pop[, sex_id := NULL]
setnames(max_pop, 'location_id', 'iso3_id')

# Merge max_pop
df <- merge(df, max_pop, by = c('iso3_id', 'year_id'), all.x = TRUE)
df[, max_pop := max(population), by = 'iso3_id']


# Assign model groups
# VR
df[data_type_id %in% c(9,10), model_group := paste0('VR-', iso3)]

# small countries with pop < 1000000
df[data_type_id %in% c(9,10) & max_pop < 1000000, model_group := paste0('VR-', region_id)]

df[ihme_loc_id == 'GRL', model_group := 'VR-GRL-AK']

# Now VA with super region
df[data_type_id == 8, model_group := paste0('VA-', super_region_id)]

# Inda VA
df[data_type_id == 8 & iso3 == 'IND', model_group := 'VA-IND']

# SRS in silo
df[source_name %like% 'India_SRS', model_group := 'VA-SRS-IND']

# Matlab
df[source_name %like% 'Matlab', model_group := 'VA-Matlab'] # Only 7 rows

# Indonesia SRS
df[source_name %like% 'Indonesia_SRS', model_group := 'VA-SRS-IDN']

# Nepal
df[source_name == 'Nepal_Burden_VA', model_group := 'VA-Nepal-Burden'] # Empty in current data

# Done!
write_csv(df, '/share/injuries/envenomation/data/modeling_data/venom_model_groups.csv')

