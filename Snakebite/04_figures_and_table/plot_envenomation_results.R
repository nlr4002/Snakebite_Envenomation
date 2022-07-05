## Make a clean file to make the maps
# One map of 2019 ASDR
# One map of proportion of venomous animal deaths due to snakes

source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_population.R')
library(data.table)


ages <- get_age_metadata(12, gbd_round_id = 6)
locs <- get_location_metadata(35, gbd_round_id = 6)
ind_locs <- locs[ihme_loc_id %like% 'IND']
eng_locs <- locs[ihme_loc_id %like% 'GBR']

df <- fread('[DIRECTORY]/mapdf_plotdf.csv')
ind_df <- df[location_id %in% ind_locs$location_id]
eng_df <- df[location_id %in% eng_locs$location_id]

pop <- get_population(location_id = unique(df$location_id),
                      year_id = unique(df$year_id),
                      age_group_id = unique(df$age_group_id),
                      sex_id = c(1,2),
                      gbd_round_id = 6, decomp_step = 'step4')

pop_all_age <- get_population(location_id = unique(df$location_id),
                      year_id = unique(df$year_id),
                      age_group_id = 22,
                      sex_id = c(1,2),
                      gbd_round_id = 6, decomp_step = 'step4')
setnames(pop_all_age, 'population', 'all_age_pop')
pop_all_age[, age_group_id := NULL]

df <- merge(df, pop, by = c('age_group_id', 'location_id', 'year_id', 'sex_id'))
df[, rate := venom_deaths/population]
df[, rate_both_sex := weighted.mean(rate, w = population), by = c('age_group_id', 'location_id', 'year_id', 'venom')]

df <- merge(df, ages[, c('age_group_id', 'age_group_weight_value')], by = c('age_group_id'))
df[, as_rate := weighted.mean(rate_both_sex, w = age_group_weight_value),
   by = c('location_id', 'year_id', 'venom')]
df[, both_sex_as_rate := as_rate*100000]

df[, loc_total_both_sex := sum(venom_deaths), by = c('location_id', 'year_id', 'venom')]

intermed_df <- unique(df[venom == 'snake', c('age_group_id', 'location_id', 'year_id', 'sex_id', 'location_name', 'sex', 'sex_id', 'venom_deaths', 'age_group_name',
                                             'loc_total_both_sex')])
write_csv(intermed_df, '[DIRECTORY]/death_results.csv')


mapdf <- unique(df[, c('location_id', 'year_id', 'location_name', 'venom', 'both_sex_as_rate', 'loc_total_both_sex')])
mapdf[, total := sum(both_sex_as_rate), by = c('location_id', 'year_id')]
mapdf[, prop := both_sex_as_rate/total]

mapdf <- merge(mapdf, locs[, c('location_id', 'ihme_loc_id', 'parent_id', 'level')], by = 'location_id')
mapdf[, iso3 := substr(ihme_loc_id, 1, 3)]

# Get the national countries you want for the map
metric <- 'csmr'
nat <- fread(paste0('[DIRECTORY]/', metric, '_country_results_count_asr.csv'))
nat[, both_sex_as_rate := mean_as]
nat[, total := sum(mean_count), by = c('location_id', 'year_id')]
nat[, prop := mean_count/total]
nat <- merge(nat, locs[, c('location_id', 'ihme_loc_id')], by = 'location_id')
nat[, loc_total_both_sex := mean_count]

# Format stuff to make the map. From the hub page to make it easy
national <- data.table(location_name = c('Ethiopia', 'Italy', 'Iran', 'New Zealand', 'Nigeria', 
                                         'Norway', 'Pakistan', 'Philippines', 'Poland', 'Russia', 'South Africa', 'Ukraine'),
                       ihme_loc_id = c('ETH', 'ITA', 'IRN', 'NZL', 'NGA', 'NOR', 'PAK', 'PHL', 'POL', 'RUS', 'ZAF', 'UKR'))
mapdf <- mapdf[!(iso3 %in% national$ihme_loc_id)]

nat <- nat[ihme_loc_id %in% national$ihme_loc_id]


# ones to format in a custom way
ind_df <- merge(ind_df, locs[, c('location_id', 'parent_id', 'ihme_loc_id', 'level')], by = 'location_id')
ind_df[, parent_deaths_both_sex := sum(venom_deaths), by = c('parent_id', 'age_group_id', 'year_id', 'venom')]
ind_df[, parent_deaths_both_sex_all_age := sum(venom_deaths), by = c('parent_id', 'year_id', 'venom')]
ind1 <- unique(ind_df[, c('parent_id', 'age_group_id', 'year_id', 'venom', 'parent_deaths_both_sex', 
                          'parent_deaths_both_sex_all_age')])
ind1 <- merge(ind1, ages[, c('age_group_id', 'age_group_weight_value')], by = 'age_group_id')
setnames(ind1, 'parent_id', 'location_id')
ind_pop <- get_population(location_id = unique(ind1$location_id), year_id = unique(ind1$year_id), 
                          sex_id = c(3), age_group_id = unique(ind1$age_group_id),
                          gbd_round_id = 6, decomp_step = 'step4')

ind1 <- merge(ind1, ind_pop, by = c('age_group_id', 'location_id', 'year_id'))
ind1[, rate := parent_deaths_both_sex/population]
ind1[, as_rate := weighted.mean(rate, w = age_group_weight_value), by = c('location_id', 'year_id', 'venom')]
ind1 <- unique(ind1[, c('location_id', 'year_id', 'venom', 'as_rate', 'parent_deaths_both_sex_all_age')])
ind1[, as_rate := as_rate*100000]
ind1 <- merge(ind1, locs[, c('location_id', 'ihme_loc_id', 'location_name')], by = 'location_id')
ind1[, total := sum(as_rate), by = c('location_id', 'year_id')]
ind1[, prop := as_rate/total]

# GBR
eng_df <- merge(eng_df, locs[, c('location_id', 'parent_id', 'ihme_loc_id', 'level')], by = 'location_id')
eng_df_ok <- eng_df[level == 4]

eng_df <- eng_df[level > 4]
eng_df[, parent_deaths_both_sex := sum(venom_deaths), by = c('parent_id', 'age_group_id', 'year_id', 'venom')]
eng_df[, parent_deaths_both_sex_all_age := sum(venom_deaths), by = c('parent_id', 'year_id', 'venom')]
eng1 <- unique(eng_df[, c('parent_id', 'age_group_id', 'year_id', 'venom', 'parent_deaths_both_sex',
                          'parent_deaths_both_sex_all_age')])
eng1 <- merge(eng1, ages[, c('age_group_id', 'age_group_weight_value')], by = 'age_group_id')
setnames(eng1, 'parent_id', 'location_id')
eng_pop <- get_population(location_id = unique(eng1$location_id), year_id = unique(eng1$year_id), 
                          sex_id = c(3), age_group_id = unique(eng1$age_group_id),
                          gbd_round_id = 6, decomp_step = 'step4')

eng1 <- merge(eng1, eng_pop, by = c('age_group_id', 'location_id', 'year_id'))
eng1[, rate := parent_deaths_both_sex/population]
eng1[, as_rate := weighted.mean(rate, w = age_group_weight_value), by = c('location_id', 'year_id', 'venom')]
eng1 <- unique(eng1[, c('location_id', 'year_id', 'venom', 'as_rate', 'parent_deaths_both_sex_all_age')])
eng1[, as_rate := as_rate*100000]
eng1 <- merge(eng1, locs[, c('location_id', 'ihme_loc_id', 'location_name')], by = 'location_id')
eng1[, total := sum(as_rate), by = c('location_id', 'year_id')]
eng1[, prop := as_rate/total]

# put them together and make a map
mapdf <- mapdf[!(location_id %in% ind_df$location_id) & !(location_id %in% eng_df$location_id)]
fulldf <- rbind(mapdf, nat, ind1, eng1, fill = TRUE)
fulldf[is.na(both_sex_as_rate), both_sex_as_rate := as_rate]
fulldf[is.na(loc_total_both_sex), loc_total_both_sex := parent_deaths_both_sex_all_age]

# Go through and make maps
fulldf_snake <- fulldf[venom == 'snake' & year_id == 2019]
fulldf_bees <- fulldf[venom == 'bees' & year_id == 2019]

write_csv(fulldf_bees, paste0('[DIRECTORY]/age_standardized_mapping_df_', metric, '_bees.csv'))

setnames(fulldf_bees, 'both_sex_as_rate', 'mapvar')

source("[DIRECTORY]/2019GBD_MAP-copy.R")
gbd_map(data = fulldf,  
        limits = c(0, 0.01, 0.1, 1, 3, 7),
        label = c('0-0.01', '>0.01-0.1', '>0.1-1', '>1-3', '>3-6.6'),
        col = 'RdYlBu',
        col.reverse = TRUE,
        na.color = 'Gray',
        legend.title = 'Age-standardized mortality rate (per 100,000)',
        title = "Age-standardized snakebite mortality rate",
        fname = "[DIRECTORY]/asdr_map.pdf",
        legend.cex = .6)

# For YLLs
gbd_map(data = fulldf,  
        limits = c(0, 1, 5,10, 100, 200, 300),
        label = c('0-1', '>1-5', '>5-10', '>10-100', '>100-200', '>200-255'),
        col = 'RdYlBu',
        col.reverse = TRUE,
        na.color = 'Gray',
        legend.title = 'Age-standardised YLLs per 100 000',
        title = "Age-standardised YLLs due to snakebites, 2019, both sexes",
        fname = "[DIRECTORY]/yll_map_2019_3.pdf",
        legend.cex = .8)


# For snake count
gbd_map(data = fulldf_snake,  
        limits = c(0, 1, 5, 50, 100, 500, 2000, 12000),
        label = c('0-1', '>1-5', '>5-50', '>50-100', '>100-500', '>500-2000', '>2000-12000'),
        col = 'RdYlBu',
        col.reverse = TRUE,
        na.color = 'Gray',
        legend.title = 'Snakebite deaths (Abs #)',
        title = "Number of snakebite deaths, both sexes, 2019",
        fname = "[DIRECTORY]/csmr_counts_snake5.pdf",
        legend.cex = .8)

fulldf_snake[, mapvar := prop]
gbd_map(data = fulldf_snake,  
        limits = c(0, 0.2, 0.4, 0.6, 0.8, 1),
        label = c('0-20%', '>20%-40%', '>40%-60%', '>60%-80%', '>80%-100%'),
        col = 'RdYlBu',
        col.reverse = TRUE,
        na.color = 'Gray',
        legend.title = '',
        title = "Proportion of deaths due to snakebite, both sexes, 2019",
        fname = "[DIRECTORY]/proportion_snake2.pdf",
        legend.cex = .8)


# For bee CSMR
gbd_map(data = fulldf_bees,  
        limits = c(0, 0.01, 0.1, 0.3, 0.5, 1.2),
        label = c('0-0.01', '>0.01-0.1', '>0.1-0.3', '>0.3-0.5', '>0.5-1.2'),
        col = 'RdYlBu',
        col.reverse = TRUE,
        na.color = 'Gray',
        legend.title = 'Age-standardized mortality rate (per 100,000)',
        title = "Age-standardized bees mortality rate",
        fname = "[DIRECTORY]/asdr_map_bee2.pdf",
        legend.cex = .6)

gbd_map(data = fulldf_bees,  
        limits = c(0, 1, 5, 20, 50, 500, 1000),
        label = c('0-1', '>1-5', '>5-20', '>20-50', '>50-500', '>500-1000'),
        col = 'RdYlBu',
        col.reverse = TRUE,
        na.color = 'Gray',
        legend.title = 'Deaths from bees (Abs #)',
        title = "Number of deaths from bees, both sexes, 2019",
        fname = "[DIRECTORY]/csmr_counts_bees3.pdf",
        legend.cex = .8)

fulldf_bees[, mapvar := prop]
gbd_map(data = fulldf_bees,  
        limits = c(0, 0.2, 0.4, 0.6, 0.8, 1),
        label = c('0-20%', '20%-40%', '>40%-60%', '>60%-80%', '>80%-100%'),
        col = 'RdYlBu',
        col.reverse = TRUE,
        na.color = 'Gray',
        legend.title = '',
        title = "Proportion of deaths due to bees, both sexes, 2019",
        fname = "[DIRECTORY]/proportion_bees.pdf",
        legend.cex = .8)



