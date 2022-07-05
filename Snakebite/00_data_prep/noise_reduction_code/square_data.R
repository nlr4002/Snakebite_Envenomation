## Square new COD data. Use same algorithm as what you did in first ST-GPR models
# Use garbage-coded data

library(data.table)
library(readr)
library(magrittr)

source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_population.R')

# Start out with merging on full COD sources, this has full coverage and prevents duplications

ages <- fread('[DIRECTORY]/age_group_ids_2018-03-07_02-13-PM.csv')
locs <- get_location_metadata(35)

dt <- fread('[DIRECTORY]/garbage_code_redistributed_snake.csv')
meta_cod <- fread('[DIRECTORY]/sample_size_with_data_type.csv')
meta_cod[, has_meta := 1]
meta_cod[, index := 1]
venom_ref <- data.table(venom = unique(dt$venom), index = 1)

meta_cod <- merge(meta_cod, venom_ref, by = 'index', allow.cartesian = TRUE)

square <- merge(dt, meta_cod, by = c('nid', 'source_name', 'code_system', 'location_id', 'year_id', 'age_group_id', 
                                   'sex_id', 'sample_size', 'venom'), all.x = TRUE, all.y = TRUE)


square <- square[location_id %in% unique(dt$location_id) & source_name %in% unique(dt$source_name)]


loc_venom_ref <- unique(dt[, c('location_id', 'venom')])
loc_venom_ref[, loc_has_venom := 1]
square <- merge(square, loc_venom_ref, by = c('location_id', 'venom'), all.x = TRUE)

square[is.na(loc_has_venom), deaths := 0]
square <- square[!(loc_has_venom == 1 & is.na(deaths))] 

write_csv(square, '[DIRECTORY]/square_garbage_code_data.csv')

