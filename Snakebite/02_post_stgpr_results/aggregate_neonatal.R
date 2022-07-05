### Make age group 4 and 5 from the new neonatal ones. Need to do it in draw space
# GBD 2019 results do not include the neonatal ages

library(data.table)
library(plyr)
library(magrittr)
library(readr)
source('[DIRECTORY]/r/get_location_metadata.R')
source('[DIRECTORY]/get_population.R')

loc <- commandArgs()[5]
year <- commandArgs()[6]
venom <- commandArgs()[7]
v <- venom

#all_years <- Sys.glob(paste0('[DIRECTORY]/', venom, '/*/', loc, '.csv'))

df <- fread(paste0('[DIRECTORY]/', venom, '/', year, '/', loc, '.csv'))



# pop <- get_population(age_group_id = unique(df$age_group_id),
#                       year_id = unique(df$year_id),
#                       location_id = unique(df$location_id),
#                       sex_id = c(1,2),
#                       gbd_round_id = 6, decomp_step = 'step4')

pop_young <- get_population(age_group_id = c(389, 388, 238, 34), 
                      location_id = unique(df$location_id),
                      year_id = unique(df$year_id),
                      sex_id = c(1,2), gbd_round_id = 7, decomp_step = 'iterative')


df_old <- df[!age_group_id %in% c(389, 388, 238, 34)]
df_young <- df[age_group_id %in% c(389, 388, 238, 34)]

df_young <- merge(df_young, pop_young, by = c('age_group_id', 'location_id', 'year_id', 'sex_id'))

df_young[age_group_id == 389 | age_group_id== 388, age_group_id := 4]
df_young[age_group_id == 238 | age_group_id == 34, age_group_id := 5]

# Draw is in rate per 100,000 right now

df_young[, draw_value := weighted.mean(draw_value, w = population), by = c('age_group_id', 'location_id', 'sex_id', 'year_id',
                                                                         'draw')]

df_young <- unique(df_young[, c('age_group_id', 'location_id', 'sex_id', 'year_id', 'draw', 'draw_value')])
df_old <- df_old[, c('age_group_id', 'location_id', 'sex_id', 'year_id', 'draw', 'draw_value')]

df_final <- rbind(df_young, df_old)
df_final[, venom := v]

write_csv(df_final, paste0('[DIRECTORY]/', venom, '/', year, '/', loc, '.csv'))
