### Get GBD draws

library(data.table)
library(magrittr)
library(ggplot2)
library(readr)

source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_population.R')
source('[DIRECTORY]/get_draws.R')

ages <- get_age_metadata(12)

loc <- commandArgs()[5]
metric <- commandArgs()[6]
#year <- commandArgs()[6]

country_draw <- get_draws('cause_id', measure_id = metric, source = 'codcorrect', metric_id = 1,
                           gbd_id = 710,
                           year_id = c(1990:2019), gbd_round_id = 6, status = 'latest',
                           location_id = loc, age_group_id = unique(ages$age_group_id),
                           sex_id = c(1,2), decomp_step = 'step4')
country_draw <- melt(country_draw, id.vars = c('age_group_id', 'year_id', 'sex_id', 'location_id'),
                  measure.vars = patterns('draw_'))

if(metric == 1){
  dir <- paste0('[DIRECTORY]')
} else if(metric == 4){
  dir <- paste0('[DIRECTORY]')
  
}
dir.create(dir)
write_csv(country_draw, paste0(dir, loc, '.csv'))

print('Wrote draws')
print(nrow(country_draw))
