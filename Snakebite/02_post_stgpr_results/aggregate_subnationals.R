#### Aggregate draws ###
## Goal is to read a national location where we do submational, and aggregate to it


library(data.table)
library(plyr)
library(magrittr)
library(readr)
source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_population.R')

print(commandArgs())
loc <- commandArgs()[5]
year <- commandArgs()[6]
venom <- commandArgs()[7]


if(loc != 90){
  locs <- get_location_metadata(35, gbd_round_id = 6)
} else{
  locs <- get_location_metadata(35, gbd_round_id = 7)
}

#loc = 102
#venom <- 'snake'
#year <- 2017

ihme_loc <- locs[location_id == loc]$ihme_loc_id
print(ihme_loc)
md_locs <- locs[most_detailed == 1 & ihme_loc_id %like% ihme_loc]$location_id

df <- rbindlist(lapply(md_locs, function(x){
  print(x)
  fp <- paste0('[DIRECTORY]/', venom, '/', year, '/', x, '.csv')
  tmp <- fread(fp)                
}))
  
pop <- get_population(age_group_id = unique(df$age_group_id),
                      year_id = unique(df$year_id),
                      location_id = unique(df$location_id),
                      sex_id = c(1,2),
                      gbd_round_id = 6, decomp_step = 'step4')

df <- merge(df, pop, by = c('age_group_id', 'year_id', 'location_id', 'sex_id'))
# Now need to aggregate up in steps
# If level 4 only. It's easy and just this:
if(length(unique(df$level)) == 1 & 4 %in% unique(df$level)){
  print('level 4 only')
  df[, draw_value := as.numeric(draw_value)]
  df[, draw_value := (draw_value*population)/100000] # Get in count space
  df[, subnat := sum(draw_value), by = c('age_group_id', 'year_id', 'sex_id', 
                                         'draw', 'parent_id')]
  result <- unique(df[, c('age_group_id', 'year_id', 'sex_id', 
                          'draw', 'parent_id', 'subnat')])
  setnames(result, c('parent_id', 'subnat'), c('location_id', 'draw_value'))
  print(nrow(result))
  dir <- paste0('[DIRECTORY]/', venom, '/', year, '/', loc, '.csv')
  write_csv(result, dir)
} 

# India
if(loc == 163){
  df[, draw_value := as.numeric(draw_value)]
  df[, draw_value := (draw_value*population)/100000] # Get in count space
  
  df[, lvl4 := sum(draw_value), by = c('age_group_id', 'year_id', 'sex_id', 
                                         'draw', 'parent_id')]
  result <- unique(df[, c('age_group_id', 'year_id', 'sex_id', 
                          'draw', 'parent_id', 'lvl4')])
  setnames(result, c('parent_id', 'lvl4'), c('location_id', 'draw_value'))
  print(nrow(result))
  write_csv(result, paste0('[DIRECTORY]', venom, '_india_level_4_agg_draws_raked.csv'))
  
  result <- merge(result, locs[, c('location_id', 'parent_id')], by = 'location_id')
  result[, lvl3 := sum(draw_value), by = c('age_group_id', 'year_id', 'sex_id', 
                                       'draw', 'parent_id')]
  
  result1 <- unique(result[, c('age_group_id', 'year_id', 'sex_id', 
                               'draw', 'parent_id', 'lvl3')])
  
  setnames(result1, c('parent_id', 'lvl3'), c('location_id', 'draw_value'))
  
  dir <- paste0('[DIRECTORY]/', venom, '/', year, '/', loc, '.csv')
  write_csv(result1, dir)
} 

# GBR
if(loc == 95){
  df[, draw_value := as.numeric(draw_value)]
  df[, draw_value := (draw_value*population)/100000] # Get in count space
  
  # Split into different GBR levels
  df6 <- df[level == 6]
  df4 <- df[level == 4]  # To be added back later
  
  df6[, lvl5 := sum(draw_value), by = c('age_group_id', 'year_id', 'sex_id', 
                                       'draw', 'parent_id')]
  result <- unique(df6[, c('age_group_id', 'year_id', 'sex_id', 
                          'draw', 'parent_id', 'lvl5')])
  setnames(result, c('parent_id', 'lvl5'), c('location_id', 'draw_value'))
  print(nrow(result))
  
  result <- merge(result, locs[, c('location_id', 'parent_id')], by = 'location_id')
  result[, lvl4 := sum(draw_value), by = c('age_group_id', 'year_id', 'sex_id', 
                                           'draw', 'parent_id')]
  result_child <- unique(result[, c('age_group_id', 'year_id', 'sex_id', 
                               'draw', 'parent_id', 'lvl4')])
  setnames(result_child, c('parent_id', 'lvl4'), c('location_id', 'draw_value'))
  result_child <- merge(result_child, locs[, c('location_id', 'parent_id')], by = 'location_id')
  # put level 4 together and aggregate to level 3
  lvl4s <- rbind(result_child, df4, fill = TRUE)
  
  lvl4s[, lvl3 := sum(draw_value), by = c('age_group_id', 'year_id', 'sex_id', 
                                        'draw', 'parent_id')]
  result <- unique(lvl4s[, c('age_group_id', 'year_id', 'sex_id', 
                           'draw', 'parent_id', 'lvl3')])
  setnames(result, c('parent_id', 'lvl3'), c('location_id', 'draw_value'))
  print(nrow(result))
  
  dir <- paste0('[DIRECTORY]/', venom, '/', year, '/', loc, '.csv')
  write_csv(result, dir)
}
  
  