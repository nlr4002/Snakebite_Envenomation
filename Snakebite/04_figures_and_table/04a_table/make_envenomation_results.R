library(data.table)
library(parallel)

source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_population.R')
source('[DIRECTORY]/get_age_metadata.R')

ages <- get_age_metadata(12, gbd_round_id = 6)
setnames(ages, c('age_group_years_start', 'age_group_years_end'), c('age_start', 'age_end'))


locs <- get_location_metadata(35, gbd_round_id = 6)
ind_locs <- locs[ihme_loc_id %like% 'IND' & level == 4]

region <- locs[level == 2]

## Want region level age-standardized rates of mortality ##
year <- 'all_years'
dir <- paste0('[DIRECTORY]/', year, '/')

pop <- get_population(location_id = locs$location_id,
                      sex_id = c(1,2),
                      year_id = c(1990, 2019), 
                      age_group_id = ages$age_group_id,
                      gbd_round_id = 6, decomp_step = 'step4')

all_age_pop <- get_population(location_id = locs$location_id,
                              sex_id = c(1,2),
                              year_id = c(1990, 2019), 
                              age_group_id = 22,
                              gbd_round_id = 6, decomp_step = 'step4')


all_loc_results <- rbindlist(mclapply(locs[level == 3]$location_id, function(loc){
  print(loc)
  df <- fread(paste0('[DIRECTORY]/', year, '/', loc, '.csv'))
  df <- df[year_id == 2019| year_id == 1990]
  df <- df[venom == 'snake']
  df <- df[, c('age_group_id', 'location_id', 'sex_id', 'year_id', 'draw', 'cause_prop')]
  df[, draw := draw-1]
  df[, draw := paste0('draw_', draw)]
  
  gbd_dir <- '[DIRECTORY]'
  gbd <- fread(paste0(gbd_dir, loc, '.csv'))
  setnames(gbd, 'variable', 'draw')
  print('Read GBD draws')
  
  gbd_dir_yll <- '[DIRECTORY]'
  gbd_yll <- fread(paste0(gbd_dir_yll, loc, '.csv'))
  setnames(gbd_yll, 'variable', 'draw')
  setnames(gbd_yll, 'value', 'gbd_ylls')
  print('Read GBD draws')
  
  df <- merge(df, gbd, by = c('age_group_id', 'location_id', 'sex_id', 'year_id', 'draw'))
  df[, deaths := cause_prop*value]
  
  df <- merge(df, gbd_yll, by = c('age_group_id', 'location_id', 'sex_id', 'year_id', 'draw'))
  df[, ylls := cause_prop*gbd_ylls]
  
  df <- merge(df, pop, by = c('age_group_id', 'location_id', 'sex_id', 'year_id'))
  df[, rate := deaths/population]
  df[, rate_yll := ylls/population]
  df <- merge(df, ages[, c('age_group_id', 'age_group_weight_value')], by = 'age_group_id')
  df[, as_rate := weighted.mean(rate, w = age_group_weight_value), by = c('location_id', 'sex_id', 'year_id', 'draw')]
  df[, as_rate_yll := weighted.mean(rate_yll, w = age_group_weight_value), by = c('location_id', 'sex_id', 'year_id', 'draw')]
  df[, sum_deaths := sum(deaths), by = c('location_id', 'sex_id', 'year_id', 'draw')]
  df[, sum_ylls := sum(ylls), by = c('location_id', 'sex_id', 'year_id', 'draw')]
  
  draw_results <- unique(df[, c('location_id', 'sex_id', 'year_id', 'draw', 'as_rate', 'sum_deaths', 'as_rate_yll', 'sum_ylls')])
  draw_results <- merge(draw_results, all_age_pop, by = c('location_id', 'sex_id', 'year_id'))
  draw_results[, as_rate := weighted.mean(as_rate, w = population), by = c('location_id', 'year_id', 'draw')]
  draw_results[, sum_deaths := sum(sum_deaths), by = c('location_id', 'year_id', 'draw')]
  draw_results[, as_rate_yll := weighted.mean(as_rate_yll, w = population), by = c('location_id', 'year_id', 'draw')]
  draw_results[, sum_ylls := sum(sum_ylls), by = c('location_id', 'year_id', 'draw')]
  draw_results <- unique(draw_results[, c('location_id', 'year_id', 'draw', 'as_rate', 'sum_deaths', 'as_rate_yll', 'sum_ylls')])
  draw_results[, mean_as := mean(as_rate), by = 'year_id']
  draw_results[, lo_as := quantile(as_rate, 0.025), by = 'year_id']
  draw_results[, hi_as := quantile(as_rate, 0.975), by = 'year_id']
  draw_results[, mean_deaths := mean(sum_deaths), by = 'year_id']
  draw_results[, lo_deaths := quantile(sum_deaths, 0.025), by = 'year_id']
  draw_results[, hi_deaths := quantile(sum_deaths, 0.975), by = 'year_id']
  draw_results[, mean_as_yll := mean(as_rate_yll), by = 'year_id']
  draw_results[, lo_as_yll := quantile(as_rate_yll, 0.025), by = 'year_id']
  draw_results[, hi_as_yll := quantile(as_rate_yll, 0.975), by = 'year_id']
  draw_results[, mean_deaths_yll := mean(sum_ylls), by = 'year_id']
  draw_results[, lo_deaths_yll := quantile(sum_ylls, 0.025), by = 'year_id']
  draw_results[, hi_deaths_yll := quantile(sum_ylls, 0.975), by = 'year_id']
  
  return(draw_results)
}, mc.cores = 30))


table_results_csmr <- unique(all_loc_results[year_id == 2019, c('location_id', 'mean_as', 'lo_as', 'hi_as', 
                                                                'mean_deaths', 'lo_deaths', 'hi_deaths',
                                                                'mean_as_yll', 'lo_as_yll', 'hi_as_yll',
                                                                'mean_deaths_yll', 'lo_deaths_yll', 'hi_deaths_yll')])

# Get diff
diff_results <- copy(all_loc_results)
diff_results <- diff_results[, c('location_id', 'year_id', 'draw', 'as_rate', 'as_rate_yll')]
diff_results <- dcast(diff_results, location_id + draw ~ year_id, value.var = c('as_rate', 'as_rate_yll'))
diff_results[, diff_csmr := as_rate_2019/as_rate_1990]
diff_results[, diff_yll := as_rate_yll_2019/as_rate_yll_1990]
diff_results[, mean_diff_csmr := mean(diff_csmr), by = 'location_id']
diff_results[, lo_diff_csmr := quantile(diff_csmr, 0.025), by = 'location_id']
diff_results[, hi_diff_csmr := quantile(diff_csmr, 0.975), by = 'location_id']
diff_results[, mean_diff_yll := mean(diff_yll), by = 'location_id']
diff_results[, lo_diff_yll := quantile(diff_yll, 0.025), by = 'location_id']
diff_results[, hi_diff_yll := quantile(diff_yll, 0.975), by = 'location_id']
diff_results <- unique(diff_results[, c('location_id', 'mean_diff_csmr', 'lo_diff_csmr', 'hi_diff_csmr',
                                        'mean_diff_yll', 'lo_diff_yll', 'hi_diff_yll')])

country_results <- merge(diff_results, table_results_csmr, by = 'location_id')
country_results <- merge(country_results, locs[, c('location_id', 'location_name')])

country_results[, `Deaths - Count, 2019` := paste0(prettyNum(round(mean_deaths, 0), big.mark = ','), " (", 
                                                   prettyNum(round(lo_deaths, 0), big.mark = ','), " to ", 
                                                   prettyNum(round(hi_deaths, 0), big.mark = ','), ")")]
country_results[, `Deaths - Age-standardised rate, 2019` := paste0(sprintf("%.2f", round(mean_as*100000, 2)), " (",
                                                                   sprintf("%.2f", round(lo_as*100000, 2)), " to ",
                                                                   sprintf("%.2f", round(hi_as*100000, 2)), ")")]
country_results[, `YLLs - Count, 2019` := paste0(prettyNum(round(mean_deaths_yll, 0), big.mark = ','), " (", 
                                                   prettyNum(round(lo_deaths_yll, 0), big.mark = ','), " to ", 
                                                   prettyNum(round(hi_deaths_yll, 0), big.mark = ','), ")")]
country_results[, `YLLs - Age-standardised rate, 2019` := paste0(sprintf("%.2f", round(mean_as_yll*100000, 2)), " (",
                                                                   sprintf("%.2f", round(lo_as_yll*100000, 2)), " to ",
                                                                   sprintf("%.2f", round(hi_as_yll*100000, 2)), ")")]
diff_results[, `Deaths - Percent change from 1990 to 2019` := paste0(round((mean_diff_csmr-1)*100, 0), "% (",
                                                                     round((lo_diff_csmr-1)*100, 0), "% to ",
                                                                     round((hi_diff_csmr-1)*100, 0), "%)")]

diff_results[, `YLLs - Percent change from 1990 to 2019` := paste0(round((mean_diff_yll-1)*100, 0), "% (",
                                                                     round((lo_diff_yll-1)*100, 0), "% to ",
                                                                     round((hi_diff_yll-1)*100, 0), "%)")]

country_table <- merge(country_results, diff_results, by = 'location_id')
country_table <- country_table[, c('location_name', 'Deaths - Count, 2019', 'Deaths - Age-standardised rate, 2019', 
                                   'Deaths - Percent change from 1990 to 2019', 'YLLs - Count, 2019',
                                   'YLLs - Age-standardised rate, 2019','YLLs - Percent change from 1990 to 2019')]

### Get the region results
# Read in file that plots from forecasting data
# The "Real" columns are what you want
# deaths
region_global <- fread('[DIRECTORY]/global_region_results.csv')
region_global <- region_global[year_id %in% c(2019)]
region_global[, `Deaths - Count, 2019` := paste0(prettyNum(round(mean_cases_real, 0), big.mark = ','), " (", 
                                                   prettyNum(round(lo_cases_real, 0), big.mark = ','), " to ", 
                                                   prettyNum(round(hi_cases_real, 0), big.mark = ','), ")")]
region_global[, `Deaths - Age-standardised rate, 2019` := paste0(sprintf("%.2f", round(mean_agest_real*100000, 2)), " (",
                                                                   sprintf("%.2f", round(lo_agest_real*100000, 2)), " to ",
                                                                   sprintf("%.2f", round(hi_agest_real*100000, 2)), ")")]


region_global_diffs <- fread('[DIRECTORY]/global_region_diffs.csv')
region_global_diffs[, `Deaths - Percent change from 1990 to 2019` := paste0(round((mean_prop_2019-1)*100, 0), "% (",
                                                                     round((lo_prop_2019-1)*100, 0), "% to ",
                                                                     round((hi_prop_2019-1)*100, 0), "%)")]

# YLLs
region_global_yll <- fread('[DIRECTORY]/global_region_results_yll.csv')
region_global_yll <- region_global_yll[year_id %in% c(2019)]
region_global_yll[, `YLLs - Count, 2019` := paste0(prettyNum(round(mean_cases_real, 0), big.mark = ','), " (", 
                                                 prettyNum(round(lo_cases_real, 0), big.mark = ','), " to ", 
                                                 prettyNum(round(hi_cases_real, 0), big.mark = ','), ")")]
region_global_yll[, `YLLs - Age-standardised rate, 2019` := paste0(sprintf("%.2f", round(mean_agest_real*100000, 2)), " (",
                                                                 sprintf("%.2f", round(lo_agest_real*100000, 2)), " to ",
                                                                 sprintf("%.2f", round(hi_agest_real*100000, 2)), ")")]


region_global_diffs_yll <- fread('[DIRECTORY]/global_region_diffs_ylls.csv')
region_global_diffs_yll[, `YLLs - Percent change from 1990 to 2019` := paste0(round((mean_prop_2019-1)*100, 0), "% (",
                                                                            round((lo_prop_2019-1)*100, 0), "% to ",
                                                                            round((hi_prop_2019-1)*100, 0), "%)")]

region_global_table <- merge(region_global[, c('region_name', 'Deaths - Count, 2019', 
                                               'Deaths - Age-standardised rate, 2019')],
                             region_global_diffs[, c('region_name', 'Deaths - Percent change from 1990 to 2019')],
                             by = 'region_name') 
region_global_table <- merge(region_global_table, region_global_yll[, c('region_name', 'YLLs - Count, 2019', 
                                                                        'YLLs - Age-standardised rate, 2019')],
                             by = 'region_name')  
region_global_table <- merge(region_global_table, region_global_diffs_yll[, c('region_name', 'YLLs - Percent change from 1990 to 2019')],
                             by = 'region_name')  

setnames(region_global_table, 'region_name', 'location_name')

all_table <- rbind(country_table, region_global_table)

all_table <- merge(all_table, locs[, c('location_name', 'sort_order', 'ihme_loc_id')], by = 'location_name')
all_table[, dupes := .N, by = 'location_name']
all_table <- all_table[!(ihme_loc_id %in% c('USA_533', 'NGA_25344', 'S4', 'S5'))]
setorder(all_table, sort_order)
all_table[, sort_order := NULL]
all_table[, ihme_loc_id := NULL][, dupes := NULL]
setnames(all_table, 'location_name', 'Location')
write.xlsx(all_table, '[DIRECTORY]/mortality_results_table_1.xlsx')
