

library(data.table)
library(magrittr)
library(ggplot2)
library(readr)
library(parallel)
library(mortcore, lib = "[DIRECTORY]")


source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_population.R')
source('[DIRECTORY]/get_cod_data.R')

locs <- get_location_metadata(35, gbd_round_id = 6)
locs_nor <- get_location_metadata(22, gbd_round_id = 7)
lvl3_plus <- locs[level >= 3]$location_id
ages <- get_age_metadata(12)

locs <- locs[!(ihme_loc_id %like% 'NOR')]
locs_nor <- locs_nor[ihme_loc_id %like% 'NOR']
locs <- rbind(locs, locs_nor, fill = TRUE)


# ST-GPR libraries
central_root <- '[DIRECTORY]'
setwd(central_root)

source('r_functions/registration/register.R')
source('r_functions/registration/sendoff.R')
source('r_functions/utilities/utility.r')
#source('[DIRECTORY]')



# Get country level results for all 5 
md <- locs[most_detailed == 1]$location_id
md_nor <- locs[most_detailed == 1 & ihme_loc_id %like% 'NOR']$location_id

snake <- model_load(run_snake, 'raked') %>% .[, venom := 'snake']
bees <- model_load(run_bees, 'raked') %>% .[, venom := 'bees']
scorpion <- model_load(run_scorpion, 'raked') %>% .[, venom := 'scorpion']
spider <- model_load(run_spider, 'raked') %>% .[, venom := 'spider']
other <- model_load(run_other, 'raked') %>% .[, venom := 'other']

results <- rbind(snake, bees, scorpion, spider, other)

results <- merge(results, locs[, c('location_id', 'location_name', 'parent_id', 'ihme_loc_id', 'level')],
                 by = 'location_id', all.x = TRUE)


## Submit jobs to make draws
mclapply(lvl3_plus, function(loc){
  print(loc)
  dt <- results[location_id == loc]
  fp <- paste0('[DIRECTORY]/', loc, '.csv')
  write_csv(dt, fp)
  
  qsub(jobname = paste0('draws_', loc),
       code = '[DIRECTORY]/make_draws_worker.R',
       pass = list(loc, fp),
       cores = 2,
       mem = 4,
       wallclock = "00:30:00",
       submit = T,
       log = T, 
       queue = 'long',
       archive_node = T,
       proj = 'proj_injuries',
       shell = paste0('[DIRECTORY]/r_shell2.sh'))  
}, mc.cores = 40)

# NOR:
for(loc in md_nor){
  print(loc)
  dt <- results[location_id == loc]
  fp <- paste0('[DIRECTORY]/', loc, '.csv')
  write_csv(dt, fp)
  
  qsub(jobname = paste0('draws_', loc),
       code = '[DIRECTORY]/make_draws_worker.R',
       pass = list(loc, fp),
       cores = 2,
       mem = 4,
       wallclock = "00:30:00",
       submit = T,
       log = T, 
       queue = 'long',
       archive_node = T,
       proj = 'proj_injuries',
       shell = paste0('[DIRECTORY]/r_shell2.sh'))
}



# Submit jobs to read in draws file, and aggregate the neonatal ages, and overwrites the same folder
mclapply(locs[level > 2]$location_id, function(loc){
  #for(year in c(1990:2019)){
  year <- 'all_years'
    for(venom in c('snake', 'spider', 'scorpion', 'bees', 'other')){
      print(paste0('agg_neonatal_', loc, '_', venom, '_', year))
      qsub(jobname = paste0('agg_neo_', loc, '_', venom, '_', year),
           code = '[DIRECTORY]/aggregate_neonatal.R',
           pass = list(loc, year, venom),
           cores = 2,
           mem = 3,
           wallclock = "00:30:00",
           submit = T,
           log = T, 
           queue = 'long',
           archive_node = T,
           proj = 'proj_injuries',
           shell = paste0('[DIRECTORY]/r_shell2.sh'))  
    }
  #}
}, mc.cores = 40, mc.silent = TRUE)

for(loc in ind_locs){
  year <- 'all_years'
    for(venom in c('snake', 'spider', 'scorpion', 'bees', 'other')){
      print(paste0('agg_neonatal_', loc, '_', venom, '_', year))
      qsub(jobname = paste0('agg_neo_', loc, '_', venom, '_', year),
           code = '[DIRECTORY]/aggregate_neonatal.R',
           pass = list(loc, year, venom),
           cores = 2,
           mem = 3,
           wallclock = "00:20:00",
           submit = T,
           log = T,
           queue = 'long',
           archive_node = T,
           proj = 'proj_injuries',
           shell = paste0('[DIRECTORY]/r_shell2.sh'))
    }
  #}
}



## Submit jobs to calculate proportion. At most detailed and level 3 level
lvl3 <- locs[level == 3]$location_id
md <- locs[most_detailed == 1]$location_id
mclapply(lvl3_plus, function(loc){
  #for(year in c(1990, 2019)){
  year <- 'all_years'
    print(paste0('prop_', loc, '_', year))
    qsub(jobname = paste0('prop_', loc, '_', year),
         code = '[DIRECTORY]/calculate_proportion_draws.R',
         pass = list(loc, year),
         cores = 2, 
         mem = 5,
         wallclock = "00:20:00",
         submit = T,
         log = T, 
         queue = 'long',
         archive_node = T,
         proj = 'proj_injuries',
         shell = paste0('[DIRECTORY]/r_shell2.sh'))  
  #}
}, mc.cores = 40, mc.silent = TRUE)

for(loc in locs_nor$location_id){
  #for(year in c(1990, 2019)){
  year <- 'all_years'
  print(paste0('prop_', loc, '_', year))
  qsub(jobname = paste0('prop_', loc, '_', year),
       code = '[DIRECTORY]/calculate_proportion_draws.R',
       pass = list(loc, year),
       cores = 2, 
       mem = 5,
       wallclock = "00:20:00",
       submit = T,
       log = T, 
       queue = 'long',
       archive_node = T,
       proj = 'proj_injuries',
       shell = paste0('[DIRECTORY]/r_shell2.sh'))  
  #}
}

## Submit jobs to get GBD 2019 results
# 


locs_redo <- locs[location_id %in% locs2]
locs <- get_location_metadata(35, gbd_round_id = 6)
#for(loc in locs$location_id){
metric <- 4
#for(loc in locs$location_id){
lapply(locs$location_id, function(loc){
  print(paste0('gbd_draws_', loc))
  qsub(jobname = paste0('gbd_draws_', loc),
       code = '[DIRECTORY]/get_gbd_draws.R',
       pass = list(loc, metric),
       cores = 2,
       mem = 5,
       wallclock = "00:30:00",
       submit = T,
       log = T,
       queue = 'long',
       archive_node = T,
       proj = 'proj_injuries',
       shell = paste0('[DIRECTORY]/r_shell2.sh'))
})



