### Nick Roberts ###

## Calculate venom proportions at level 3 location level and maintain draws
## Then scale up to region

library(data.table)
library(plyr)
library(magrittr)
library(readr)
source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_population.R')
source('[DIRECTORY]/get_draws.R')


loc <- commandArgs()[5]
year <- commandArgs()[6]

venoms <- c('snake', 'bees', 'scorpion', 'spider', 'other')


all_df <- rbindlist(lapply(venoms, function(v){
  df <- fread(paste0('[DIRECTORY]/', v, '/', year, '/', loc, '.csv'))
  df[, venom := v]
}))

all_df[, total := sum(draw_value), by = c('age_group_id', 'year_id', 'sex_id', 'draw', 'location_id')]
all_df[, cause_prop := draw_value/total]

dir <- paste0('[DIRECTORY]/', year, '/')
dir.create(dir, recursive = TRUE)

write_csv(all_df, paste0(dir, loc, '.csv'))
