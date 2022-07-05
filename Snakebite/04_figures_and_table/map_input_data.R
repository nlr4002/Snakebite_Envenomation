### Map location-years used in the ST-GPR model for snakes

run_snake <- 136802

central_root <- '[DIRECTORY]'
setwd(central_root)
source('[DIRECTORY]/get_location_metadata.R')
source('r_functions/registration/register.R')
source('r_functions/registration/sendoff.R')
source('r_functions/utilities/utility.r')
source("[DIRECTORY]/2019GBD_MAP-copy.R")
locs <- get_location_metadata(22, gbd_round_id = 6)

dt <- model_load(run_snake, 'data')
dt <- dt[data != 0]
dt <- merge(dt, locs[, c('location_id', 'ihme_loc_id', 'region_name', 'super_region_name', 'parent_id')], by = 'location_id')
loc_year <- unique(dt[, c('year_id', 'ihme_loc_id', 'location_id')])
loc_year[, ly := .N, by = 'location_id']

setnames(loc_year, 'ly', 'mapvar')
mapdf <- unique(loc_year[, c('ihme_loc_id', 'mapvar', 'location_id')])


gbd_map(data=mapdf,
        limits=c(0, 1, 5, 10 , 20, 30, 40), # change to whatever bins make sense for your data
        label=c('1', '2-5', '6-10', '11-20', '21-30', '31-40'), # label bins in the legendsdfds
        col='Blues', # choose palette
        na.color = 'Gray',
        legend.title = 'Number of years of data in a location',
        #col=c('chartreuse4', 'chartreuse'), # choose palette
        col.reverse=F, #reverse palette if you want
        title="Years of snakebite death data per location (#)", # map title
        fname="[DIRECTORY]/snakebite_map.pdf",
        legend.cex = .7) # save as .tif .eps or .pdf

### Map VR or VA
df <- fread(paste0('[DIRECTORY]/', v, '_NR_rate_data.csv'))
df[model_group %like% '^VR', type := 'VR'][model_group %like% '^VA', type := 'VA']
df <- unique(df[, c('location_id', 'ihme_loc_id', 'type')])
df <- dcast(df, location_id + ihme_loc_id ~ type)
df[!is.na(VA) & !is.na(VR), label := 'VR and VA']
df[is.na(VA) & !is.na(VR), label := 'VR']
df[is.na(VR) & !is.na(VA), label := 'VA']

df[label == 'VR', mapvar := 1][label == 'VA', mapvar := 2][label == 'VR and VA', mapvar := 3]


gbd_map(data=df,
        limits=c(1, 2, 3, 4), # change to whatever bins make sense for your data
        label=c('VR only', 'VA only', 'Both VR and VA'), # label bins in the legendsdfds
        col='RdYlBu', # choose palette
        na.color = 'Gray',
        legend.title = '',
        #col=c('chartreuse4', 'chartreuse'), # choose palette
        col.reverse=F, #reverse palette if you want
        title="Data type sources used", # map title
        fname="/[DIRECTORY]/data_type_snakebite4.pdf",
        legend.cex = .7) # save as .tif .eps or .pdf

source('[DIRECTORY]/get_cod_data.R')
cod_data <- get_cod_data(cause_id = 710, gbd_round_id = 6, decomp_step = 'step4')
cod_loc_years <- unique(cod_data[, c('year', 'location_id')])
cod_loc_years <- merge(cod_loc_years, locs[, c('level', 'location_id', 'ihme_loc_id', 'parent_id')], by = 'location_id')
cod_loc_years[, num_loc := .N, by = 'location_id']
cod_loc_map <- unique(cod_loc_years[, c('ihme_loc_id', 'location_id', 'num_loc')])
cod_loc_map[, mapvar := num_loc]

cod_loc_ind <- cod_loc_map[ihme_loc_id %like% 'IND']
cod_loc_ind <- merge(cod_loc_ind, locs[, c('location_id', 'level', 'parent_id')], by = 'location_id')
cod_loc_ind[, parent_max := max(mapvar), by = 'parent_id']
cod_loc_ind <- unique(cod_loc_ind[location_id != 163, c('parent_id', 'parent_max')]) %>%
  setnames(c('parent_id', 'parent_max'), c('location_id', 'mapvar'))

cod_loc_map <- cod_loc_map[!(ihme_loc_id %like% 'IND')]
cod_loc_map <- rbind(cod_loc_map, cod_loc_ind, fill = TRUE)



cod_loc_ind <- cod_loc_map[ihme_loc_id %like% 'IND']
cod_loc_ind <- merge(cod_loc_ind, locs[, c('location_id', 'level', 'parent_id')], by = 'location_id')
cod_loc_ind[, parent_max := max(mapvar), by = 'parent_id']
cod_loc_ind <- unique(cod_loc_ind[location_id != 163, c('parent_id', 'parent_max')]) %>%
  setnames(c('parent_id', 'parent_max'), c('location_id', 'mapvar'))

eng_locs <- data.table(location_name = c('South West England', 'South East England',
              'East of England', 'East Midlands', 'West Midlands', 
              'North West England', 'Yorkshire and the Humber', 
              'North East England', 'North West England')) %>% 
  merge(locs[, c('location_id', 'location_name')], by = 'location_name')
eng_locs[, mapvar := 38]
cod_loc_map <- rbind(cod_loc_map, eng_locs, fill = TRUE)

gbd_map(data=cod_loc_map,
        limits=c(0, 10, 20, 30, 40), # change to whatever bins make sense for your data
        label=c('0-10', '>10-20', '>20-30', '>30-40'), # label bins in the legendsdfds
        col='Blues', # choose palette
        na.color = 'Gray',
        legend.title = 'Number of years of data in a location',
        #col=c('chartreuse4', 'chartreuse'), # choose palette
        col.reverse=F, #reverse palette if you want
        title="Location-years of data in CODEm model", # map title
        fname="[DIRECTORY]/codem_data_map_ind_eng2.pdf",
        legend.cex = .7) # save as .tif .eps or .pdf


