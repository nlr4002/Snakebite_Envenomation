## Parallelize making draws by location


library(data.table)
library(plyr)
library(magrittr)
library(readr)

print(commandArgs())
loc <- commandArgs()[5]
fp <- commandArgs()[6]
df <- fread(fp)

make_stgpr_draws <- function(df, i){
  pct <- i/nrow(df)
  if(round_any(pct, 0.1) == pct){ 
    print(pct)
  }
  
  tmp <- df[i] 
  tmp[, log_mean := log(gpr_mean)][, log_upper := log(gpr_upper)][, log_lower := log(gpr_lower)]
  tmp[, upper_diff := log_upper-log_mean][, lower_diff := log_mean-log_lower]
  tmp[, sd := ((upper_diff + lower_diff)/2)/1.96]
  
  draws <- rnorm(1000, mean = tmp$log_mean, sd = tmp$sd) %>% exp()
  
  print(names(tmp))
  row_draws <- data.table(location_id = tmp$location_id, 
                          sex_id = tmp$sex_id,
                          year_id = tmp$year_id,
                          age_group_id = tmp$age_group_id,
                          venom = tmp$venom,
                          parent_id = tmp$parent_id,
                          level = tmp$level,
                          location_name = tmp$location_name,
                          ihme_loc_id = tmp$ihme_loc_id,
                          #population = tmp$population,
                          draw = c(1:1000),
                          draw_value = draws)
  
  return(row_draws)
}

all_draws <- rbindlist(lapply(1:nrow(df), function(x){
  draws <- make_stgpr_draws(df, x)
  return(draws)
}))


for(v in unique(all_draws$venom)){
  dir <- paste0('[DIRECTORY]/', v, '/all_years/')
  dir.create(dir, recursive = TRUE)
  
  write_csv(all_draws[venom == v], 
            paste0(dir, loc, '.csv'))
}
  




