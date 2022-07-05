### Calculate snakebite results by draw
### Want count space and rate (and ASR space)
### Want location, iso3, region, super regoin, and global all here
library(data.table)
library(ggplot2)
library(magrittr)
library(readr)


source('[DIRECTORY]/get_location_metadata.R')
source('/[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_population.R')


ages <- get_age_metadata(12, gbd_round_id = 6)
yll_scalar <- fread('[DIRECTORY]/life_expect_max.csv')

locs <- get_location_metadata(35, gbd_round_id = 6)
lvl3 <- locs[level == 3]$location_id
lvl3_plus <- locs[level >= 3]$location_id
md <- locs[most_detailed == 1]$location_id


# Do just for GBD 2019
all_draws <- rbindlist(mclapply(lvl3, function(loc){
  print(loc)
  df <- data.table()
  year <- 'all_years'

    df1 <- fread(paste0('[DIRECTORY]/',year, '/', loc, '.csv'))
    df1 <- df1[year_id %in% c(1990, 2019)]
    df <- rbind(df, df1, fill = TRUE)
  #}
  return(df)
}, mc.cores = 35))
write_csv(all_draws, '[DIRECTORY]/level_3_draws_raked.csv')

all_draws <- fread('[DIRECTORY]/level_3_draws_raked.csv')



all_draws[, draw := draw-1]
all_draws[, draw := paste0('draw_', draw)]

metric <- 'csmr'
gbd_draws <- rbindlist(lapply(lvl3, function(loc){
  df1 <- fread(paste0('[DIRECTORY]/', metric, '/', loc, '.csv'))
  df1 <- df1[year_id == 2019 | year_id == 1990]
  return(df1)
}), fill = TRUE)
setnames(gbd_draws, 'variable', 'draw')

all_draws <- merge(all_draws, gbd_draws, by = c('age_group_id', 'year_id', 'sex_id', 'location_id', 'draw'), all.x = TRUE)
setnames(all_draws, 'value', 'gbd_deaths')
all_draws[, venom_deaths := cause_prop*gbd_deaths]

# now get population and make rates
pop <- get_population(age_group_id = unique(all_draws$age_group_id),
                      sex_id = c(1,2),
                      location_id = unique(all_draws$location_id),
                      year_id = unique(all_draws$year_id),
                      gbd_round_id = 6, decomp_step = 'step4')

pop_all_age <- get_population(age_group_id = 22,
                      sex_id = c(1,2),
                      location_id = unique(all_draws$location_id),
                      year_id = unique(all_draws$year_id),
                      gbd_round_id = 6, decomp_step = 'step4')
setnames(pop_all_age, 'population', 'pop_all_age')
pop_all_age[, age_group_id := NULL]

all_draws <- merge(all_draws, pop, by = c('age_group_id', 'sex_id', 'location_id', 'year_id'), all.x = TRUE)
all_draws <- merge(all_draws, pop_all_age, by = c('sex_id', 'location_id', 'year_id'), all.x = TRUE)
all_draws[, venom_rate := venom_deaths/population]

# draw-level age-standardised rate
all_draws <- merge(all_draws, ages[, c('age_group_id', 'age_group_weight_value')], by = 'age_group_id', all.x = TRUE)
all_draws[, as_venom_rate := weighted.mean(venom_rate, w = age_group_weight_value), 
          by = c('sex_id', 'location_id', 'year_id', 'draw', 'venom')]

all_draws[, venom_count := sum(venom_deaths), by = c('sex_id', 'location_id', 'year_id', 'draw', 'venom')]
all_draws[, total_pop := sum(population), by = c('sex_id', 'year_id', 'draw', 'venom', 'location_id')]

# mean and UI of subnationals
subnat_df <- unique(all_draws[, c('location_id', 'sex_id', 'year_id', 'pop_all_age', 'draw', 'as_venom_rate')])
#all_draws[, subnat_mean_count := mean(venom_count), by = c('venom', '')]


country <- unique(all_draws[, c('sex_id', 'location_id', 'year_id', 'draw', 'venom', 'venom_count', 'as_venom_rate', 'total_pop')])
country[, both_sex_count := sum(venom_count), by = c('location_id', 'year_id', 'draw', 'venom')]
country[, as_both_sex_rate := weighted.mean(as_venom_rate, w = total_pop), by = c('location_id', 'year_id', 'draw', 'venom')]

country <- unique(country[, c('location_id', 'year_id', 'draw', 'venom', 'both_sex_count', 'as_both_sex_rate')])
country[, mean_count := mean(both_sex_count), by = c('venom', 'location_id', 'year_id')]
country[, lo_count := quantile(both_sex_count, probs = 0.025), by = c('venom', 'location_id', 'year_id')]
country[, hi_count := quantile(both_sex_count, probs = 0.975), by = c('venom', 'location_id', 'year_id')]
country[, mean_as := mean(as_both_sex_rate), by = c('venom', 'location_id', 'year_id')]
country[, lo_as := quantile(as_both_sex_rate, probs = 0.025), by = c('venom', 'location_id', 'year_id')]
country[, hi_as := quantile(as_both_sex_rate, probs = 0.975), by = c('venom', 'location_id', 'year_id')]

setorder(country, -mean_as)

country_wide <- dcast(country, location_id + draw + venom ~ year_id, value.var = c('both_sex_count', 'as_both_sex_rate'))

country_wide[, diff_as := as_both_sex_rate_2019/as_both_sex_rate_1990]
country_wide[, mean_diff_as := mean(diff_as), by = c('location_id', 'venom')]
country_wide[, lo_diff_as := quantile(diff_as, 0.025), by = c('location_id', 'venom')]
country_wide[, hi_diff_as := quantile(diff_as, 0.975), by = c('location_id', 'venom')]

country_wide[, diff_as_abs := as_both_sex_rate_2019-as_both_sex_rate_1990]
country_wide[, mean_diff_as_abs := mean(diff_as_abs), by = c('location_id', 'venom')]
country_wide[, lo_diff_as_abs := quantile(diff_as_abs, 0.025), by = c('location_id', 'venom')]
country_wide[, hi_diff_as_abs := quantile(diff_as_abs, 0.975), by = c('location_id', 'venom')]

country_wide[, diff_count := both_sex_count_2019/both_sex_count_1990]
country_wide[, mean_diff_count := mean(diff_count), by = c('location_id', 'venom')]
country_wide[, lo_diff_count := quantile(diff_count, 0.025), by = c('location_id', 'venom')]
country_wide[, hi_diff_count := quantile(diff_count, 0.975), by = c('location_id', 'venom')]

country_wide[, diff_count_abs := both_sex_count_2019-both_sex_count_1990]
country_wide[, mean_diff_count_abs := mean(diff_count_abs), by = c('location_id', 'venom')]
country_wide[, lo_diff_count_abs := quantile(diff_count_abs, 0.025), by = c('location_id', 'venom')]
country_wide[, hi_diff_count_abs := quantile(diff_count_abs, 0.975), by = c('location_id', 'venom')]

country_wide <- merge(country_wide, locs[, c('location_id', 'location_name')], by = 'location_id')
setorder(country_wide, mean_diff_as)
country_wide_snake <- country_wide[venom == 'snake']
country_wide_snake <- unique(country_wide_snake[, c('location_name', 'venom', 'mean_diff_as', 'lo_diff_as', 'hi_diff_as', 
                                                    'mean_diff_count', 'lo_diff_count', 'hi_diff_count',
                                                    'mean_diff_as_abs', 'lo_diff_as_abs', 'hi_diff_as_abs',
                                                    'mean_diff_count_abs', 'lo_diff_count_abs', 'hi_diff_count_abs')])
setorder(country_wide_snake, mean_diff_as_abs)
country_wide_snake[, mean_diff_as_abs := mean_diff_as_abs*100000]
country_wide_snake[, lo_diff_as_abs := lo_diff_as_abs*100000]
country_wide_snake[, hi_diff_as_abs := hi_diff_as_abs*100000]
write_csv(country_wide_snake, paste0('[DIRECTORY]/', metric, '_country_wide_snake.csv'))

country_results <- unique(country[, c('year_id', 'venom', 'location_id', 'mean_count', 'lo_count', 'hi_count', 'mean_as', 'lo_as', 'hi_as')])
country_results <- merge(country_results, locs[, c('location_id', 'location_name')], by = 'location_id')
setorder(country_results, -mean_as)
country_results[, mean_as := mean_as*100000]
country_results[, lo_as := lo_as*100000]
country_results[, hi_as := hi_as*100000]
  
setorder(country_results, -mean_count)
write_csv(country_results, paste0('[DIRECTORY]/', metric, '_country_results_count_asdr.csv'))
write_csv(country_results, paste0('[DIRECTORY]/', metric, '_country_results_count_asr.csv'))

# Global
all_draws[, global_count := sum(venom_deaths), by = c('age_group_id', 'sex_id', 'year_id', 'draw', 'venom')]
all_draws[, global_pop := sum(population), by = c('age_group_id', 'sex_id', 'year_id', 'draw', 'venom')]
all_draws[, global_rate := global_count/global_pop]

## Country:
all_draws[, country_rate := venom_deaths/population]



## GLOBAL
global_draws <- unique(all_draws[, c('age_group_id', 'sex_id', 'year_id', 'venom', 'draw', 'age_group_weight_value', 
                                     'global_count', 'global_pop', 'global_rate')])
global_draws[, as_venom_rate := weighted.mean(global_rate, w = age_group_weight_value), by = c('venom', 'draw', 'sex_id', 'year_id')]
global_draws[, all_ages_pop := sum(global_pop), by = c('sex_id', 'year_id', 'venom', 'draw')]
global_draws[, all_ages_count := sum(global_count), by = c('sex_id', 'year_id', 'venom', 'draw')]

# By age and sex
global_draws[, mean_rate := mean(global_rate), by = c('venom', 'sex_id', 'year_id', 'age_group_id')]
global_draws[, low_rate := quantile(global_rate, probs = 0.025), by = c('venom', 'sex_id', 'year_id', 'age_group_id')]
global_draws[, hi_rate := quantile(global_rate, probs = 0.975), by = c('venom', 'sex_id', 'year_id', 'age_group_id')]

global_draws[, mean_count_age_sex := mean(global_count), by = c('venom', 'sex_id', 'year_id', 'age_group_id')]
global_draws[, lo_count_age_sex := quantile(global_count, probs = 0.025), by = c('venom', 'sex_id', 'year_id', 'age_group_id')]
global_draws[, hi_count_age_sex := quantile(global_count, probs = 0.975), by = c('venom', 'sex_id', 'year_id', 'age_group_id')]

glo_result <- unique(global_draws[, c('venom', 'sex_id', 'year_id', 'age_group_id', 'mean_rate', 'low_rate', 'hi_rate', 
                                      'mean_count_age_sex', 'lo_count_age_sex', 'hi_count_age_sex')])
age_names <- fread('[DIRECTORY]/age_group_ids_2019-11-21_02-42-PM.csv')
glo_result <- merge(glo_result, age_names[, c('age_group_id', 'age_group_name')], by = 'age_group_id')
glo_result[sex_id == 1, sex := 'Male'][sex_id == 2, sex := 'Female']
setorder(glo_result, age_group_id)
glo_result[, age_group_name := factor(age_group_name, levels = unique(glo_result$age_group_name))]



as_global <- unique(global_draws[, c('draw', 'year_id', 'sex_id', 'venom', 'all_ages_count', 'all_ages_pop', 'as_venom_rate')])
as_global[, as_both_sex := weighted.mean(as_venom_rate, w = all_ages_pop), by = c('venom', 'draw', 'year_id')]
as_global[, count_both_sex := sum(all_ages_count), by = c('venom', 'draw', 'year_id')]

as_global[, mean_sex_as := mean(as_venom_rate)*100000, by = c('venom', 'year_id', 'sex_id')]
as_global[, lower_sex_as := quantile(as_venom_rate, probs = 0.025)*100000, by = c('venom', 'year_id', 'sex_id')]
as_global[, upper_sex_as := quantile(as_venom_rate, probs = 0.975)*100000, by = c('venom', 'year_id', 'sex_id')]

as_global[, mean_count := mean(count_both_sex), by = c('venom', 'year_id')]
as_global[, lower_count := quantile(count_both_sex, probs = 0.025), by = c('venom', 'year_id')]
as_global[, upper_count := quantile(count_both_sex, probs = 0.975), by = c('venom', 'year_id')]

as_global[, mean_asdr := mean(as_both_sex), by = c('venom', 'year_id')]
as_global[, lower_asdr := quantile(as_both_sex, probs = 0.025), by = c('venom', 'year_id')]
as_global[, upper_asdr := quantile(as_both_sex, probs = 0.975), by = c('venom', 'year_id')]

as_global[, mean_asdr := mean_asdr*100000]
as_global[, lower_asdr := lower_asdr*100000]
as_global[, upper_asdr := upper_asdr*100000]

year_dif <- unique(as_global[, c('draw', 'year_id', 'venom', 'count_both_sex', 'as_both_sex')])
year_dif <- dcast(year_dif, draw + venom ~ year_id, value.var = c('count_both_sex', 'as_both_sex'))
year_dif[, change := as_both_sex_2019/as_both_sex_1990]
year_dif[, mean_change := mean(change), by= 'venom']
year_dif[, low_change := quantile(change, probs = 0.025), by = 'venom']
year_dif[, hi_change := quantile(change, probs = 0.975), by = 'venom']
write_csv(as_global, paste0('[DIRECTORY]/', metric, '_as_global_raked.csv'))
write_csv(year_dif, paste0('[DIRECTORY]/', metric, '_year_dif_global_raked.csv'))

## Region
all_draws <- merge(all_draws, locs[, c('location_id', 'region_name')], by = 'location_id', all.x = TRUE)

all_draws[, region_count := sum(venom_deaths), by = c('age_group_id', 'sex_id', 'year_id', 'draw', 'venom', 'region_name')]
all_draws[, region_pop := sum(population), by = c('age_group_id', 'sex_id', 'year_id', 'draw', 'venom', 'region_name')]
all_draws[, region_rate := region_count/region_pop]

region_draws <- unique(all_draws[, c('age_group_id', 'sex_id', 'year_id', 'venom', 'draw', 'age_group_weight_value', 
                                     'region_count', 'region_pop', 'region_rate', 'region_name')])
region_draws[, as_venom_rate := weighted.mean(region_rate, w = age_group_weight_value), 
             by = c('venom', 'draw', 'sex_id', 'region_name', 'year_id')]
region_draws[, all_ages_pop := sum(region_pop), by = c('sex_id', 'year_id', 'venom', 'draw', 'region_name')]
region_draws[, all_ages_count := sum(region_count), by = c('sex_id', 'year_id', 'venom', 'draw', 'region_name')]
region_draws[, all_ages_rate := all_ages_count/all_ages_pop]
region_draws[, global_count := sum(region_count), by = c('sex_id', 'year_id', 'venom', 'draw')]


as_region <- unique(region_draws[, c('draw', 'sex_id', 'venom', 'year_id', 'region_name', 'all_ages_count', 'all_ages_pop', 'as_venom_rate', 'all_ages_rate',
                                     'global_count')])
as_region[, as_both_sex := weighted.mean(as_venom_rate, w = all_ages_pop), by = c('venom', 'draw', 'region_name', 'year_id')]
as_region[, count_both_sex := sum(all_ages_count), by = c('venom', 'draw', 'region_name', 'year_id')]
as_region[, proportion := all_ages_count/global_count]

as_region[, mean_count := mean(count_both_sex), by = c('venom', 'region_name', 'year_id')]
as_region[, lower_count := quantile(count_both_sex, probs = 0.025), by = c('venom', 'region_name', 'year_id')]
as_region[, upper_count := quantile(count_both_sex, probs = 0.975), by = c('venom', 'region_name', 'year_id')]

as_region[, mean_asdr := mean(as_both_sex), by = c('venom', 'region_name', 'year_id')]
as_region[, lower_asdr := quantile(as_both_sex, probs = 0.025), by = c('venom', 'region_name', 'year_id')]
as_region[, upper_asdr := quantile(as_both_sex, probs = 0.975), by = c('venom', 'region_name', 'year_id')]

as_region[, mean_rate := mean(all_ages_rate)*100000, by = c('venom', 'region_name', 'year_id', 'sex_id')]
as_region[, lower_rate := quantile(all_ages_rate, probs = 0.025)*100000, by = c('venom', 'region_name', 'year_id', 'sex_id')]
as_region[, upper_rate := quantile(all_ages_rate, probs = 0.975)*100000, by = c('venom', 'region_name', 'year_id', 'sex_id')]

as_region[, mean_as_sex := mean(as_venom_rate)*100000, by = c('venom', 'region_name', 'year_id', 'sex_id')]
as_region[, lower_as_sex := quantile(as_venom_rate, probs = 0.025)*100000, by = c('venom', 'region_name', 'year_id', 'sex_id')]
as_region[, upper_as_sex := quantile(as_venom_rate, probs = 0.975)*100000, by = c('venom', 'region_name', 'year_id', 'sex_id')]

as_region[, mean_asdr := mean_asdr*100000]
as_region[, lower_asdr := lower_asdr*100000]
as_region[, upper_asdr := upper_asdr*100000]

as_region[, mean_prop := mean(proportion), by = c('venom', 'region_name', 'year_id')]
as_region[, lower_prop := quantile(proportion, probs = 0.025), by = c('venom', 'region_name', 'year_id')]
as_region[, upper_prop := quantile(proportion, probs = 0.975), by = c('venom', 'region_name', 'year_id')]

year_dif <- unique(as_region[, c('draw', 'region_name', 'year_id', 'venom', 'count_both_sex', 'as_both_sex')])
year_dif <- dcast(year_dif, draw + venom + region_name ~ year_id, value.var = c('count_both_sex', 'as_both_sex'))
year_dif[, change := as_both_sex_2019/as_both_sex_1990]
year_dif[, mean_change := mean(change), by= c('venom', 'region_name')]
year_dif[, low_change := quantile(change, probs = 0.025), by = c('venom', 'region_name')]
year_dif[, hi_change := quantile(change, probs = 0.975), by = c('venom', 'region_name')]
year_dif <- year_dif[venom == 'snake']
year_dif <- unique(year_dif[, c('venom', 'region_name', 'mean_change', 'low_change', 'hi_change')])
setorder(year_dif, -mean_change)


region_results <- unique(as_region[venom == 'snake', c('region_name', 'year_id', 'mean_count', 'lower_count', 'upper_count', 
                                       'mean_asdr', 'lower_asdr', 'upper_asdr', 'mean_prop', 'lower_prop', 'upper_prop')])
setorder(region_results, -mean_count)
write_csv(region_results, paste0('[DIRECTORY]/', metric, '_region_results.csv'))
write_csv(year_dif, paste0('[DIRECTORY]/', metric, '_region_results_year_dif.csv'))

## Age-trends by region
region_ref <- unique(locs[, c('region_name', 'super_region_name')])
region_draws <- merge(region_draws, region_ref, by = 'region_name')
region_draws[, super_region_age := sum(region_count), by = c('age_group_id', 'sex_id', 'year_id', 'venom', 'super_region_name', 'draw')]
region_draws[, super_region_pop := sum(region_count), by = c('age_group_id', 'sex_id', 'year_id', 'venom', 'super_region_name', 'draw')]

region_pop_df <- unique(region_draws[, c('age_group_id', 'sex_id', 'year_id', 'region_name', 'super_region_name', 'region_pop')])
region_pop_df[, super_region_pop := sum(region_pop), by = c('age_group_id', 'sex_id', 'year_id', 'super_region_name')]
super_region_pop_df <- unique(region_pop_df[, c('age_group_id', 'sex_id', 'year_id', 'super_region_name', 'super_region_pop')])

region_draws[, mean_region_age := mean(region_count), by = c('age_group_id', 'sex_id', 'year_id', 'venom', 'region_name')]
region_draws[, lo_count_age := quantile(region_count, 0.025), by = c('age_group_id', 'sex_id', 'year_id', 'venom', 'region_name')]
region_draws[, hi_count_age := quantile(region_count, 0.975), by = c('age_group_id', 'sex_id', 'year_id', 'venom', 'region_name')]

super_region_draws <- unique(region_draws[, c('age_group_id', 'draw', 'sex_id', 'year_id', 'venom', 'super_region_name', 'super_region_age')])
super_region_draws[, mean_age := mean(super_region_age), by = c('age_group_id', 'sex_id', 'year_id', 'venom', 'super_region_name')]
super_region_draws[, lo_age := quantile(super_region_age, 0.025), by = c('age_group_id', 'sex_id', 'year_id', 'venom', 'super_region_name')]
super_region_draws[, hi_age := quantile(super_region_age, 0.975), by = c('age_group_id', 'sex_id', 'year_id', 'venom', 'super_region_name')]

super_region_plot <- unique(super_region_draws[, c('super_region_name', 'age_group_id', 'sex_id', 'year_id', 'venom', 'mean_age', 'lo_age', 'hi_age')])
super_region_plot <- merge(super_region_plot, super_region_pop_df, by = c('age_group_id', 'sex_id', 'year_id', 'super_region_name'))
super_region_plot[, rate := mean_age/super_region_pop]


region_age <- unique(region_draws[, c('age_group_id', 'sex_id', 'year_id', 'venom', 'region_name', 
                                      'mean_region_age', 'lo_count_age', 'hi_count_age', 'region_pop')])

region_age[, mean_rate_age := (mean_region_age/region_pop)*100000]
region_age[, lo_rate_age := (lo_count_age/region_pop)*100000]
region_age[, hi_rate_age := (hi_count_age/region_pop)*100000]

region_age[, all_age_rate := weighted.mean(mean_rate_age, w = region_pop), by = c('sex_id', 'year_id', 'venom', 'region_name')]

sex_comp <- region_age[venom == 'snake' & year_id == 2019, c('region_name', 'sex_id', 'all_age_rate')] %>% unique()
sex_comp <- dcast(sex_comp, region_name ~ sex_id, value.var = 'all_age_rate')
sex_comp[, male_over_female := `1`/`2`]

region_sex_comp <- region_age[venom == 'snake' & year_id == 2019]
region_sex_comp <- dcast(region_sex_comp, age_group_id + year_id + venom +region_name~ sex_id, value.var = c('mean_rate_age', 'lo_rate_age', 'hi_rate_age', 'mean_region_age'))
region_sex_comp[, sex_diff := mean_rate_age_1/mean_rate_age_2]

age_names <- fread('[DIRECTORY]/age_group_ids_2019-11-21_02-42-PM.csv')
region_age <- merge(region_age, age_names[, c('age_group_id', 'age_group_name')], by = 'age_group_id')

setorder(region_age, age_group_id)
region_age[, age_group_name := factor(age_group_name, levels = unique(region_age$age_group_name))]

region_age[, region_name := factor(region_name, levels = rev(unique(region_results$region_name)))]

region_age[!region_name %in% c('South Asia', 'Western Sub-Saharan Africa', 
                               'Eastern Sub-Saharan Africa', 'Central Sub-Saharan Africa', 'Oceania', 
                               'Southeast Asia', 'Other'),
           region_label := 'Other']
region_age[region_name %in% c('South Asia', 'Western Sub-Saharan Africa', 
                              'Eastern Sub-Saharan Africa', 'Central Sub-Saharan Africa', 'Oceania', 
                              'Southeast Asia', 'Other'),
           region_label := region_name]
region_age[, region_label := factor(region_label, levels = rev(c('South Asia', 'Western Sub-Saharan Africa', 
                                                                              'Eastern Sub-Saharan Africa', 'Central Sub-Saharan Africa', 'Oceania', 
                                                                              'Southeast Asia', 'Other')))]
colors <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69')

region_age[sex_id == 1, sex := 'Male'][sex_id == 2, sex := 'Female']

write_csv(region_age, paste0('[DIRECTORY]/region_age_', metric, '.csv'))
# if reading in
region_age <- fread(paste0('[DIRECTORY]/region_age_', metric, '.csv'))
setorder(region_age, age_group_id)
region_age[, age_group_name := factor(age_group_name, levels = unique(region_age$age_group_name))]

region_age[, region_label := factor(region_label, levels = rev(c('South Asia', 'Western Sub-Saharan Africa', 
                                                                 'Eastern Sub-Saharan Africa', 'Central Sub-Saharan Africa', 'Oceania', 
                                                                 'Southeast Asia', 'Other')))]
m_rate <- ggplot(region_age[year_id == 2019 & venom == 'snake' & sex_id == 1], 
       aes(x = factor(age_group_name), y = mean_rate_age)) +
  geom_bar(stat = 'identity', aes(fill = region_label)) + theme_bw() +
  #facet_wrap(~sex_id) +
  xlab('Age group') + ylab('Snakebite YLL rate per 100,000') +
  #ggtitle('Male snakebite death rate')+ 
  scale_fill_manual(name = 'GBD region', values = colors) +
  coord_flip() +
  scale_x_discrete(position = 'top') +
  scale_y_reverse(breaks = c(120, 90, 60, 30, 0), labels = c('120', '90', '60', '30', '0'), limits = c(120, 0)) +
  #scale_y_reverse(breaks = c(5000, 4000, 3000, 2000, 1000, 0), labels = c('5000', '4000', '3000', '2000', '1000', '0'), limits = c(5500, 0)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_blank(),
        axis.line = element_line(colour="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position = "none") +
  ggtitle("Males") +
  xlab('Death rate per 100 000')

f_rate <- ggplot(region_age[year_id == 2019 & venom == 'snake' & sex_id == 2], 
                 aes(x = factor(age_group_name), y = mean_rate_age)) +
  geom_bar(stat = 'identity', aes(fill = region_label)) + theme_bw() +
  #facet_wrap(~sex_id) +
  xlab('Age group') + ylab('Snakebite YLL rate per 100,000') +
  #ggtitle('Male snakebite death rate')+ 
  scale_fill_manual(name = 'GBD region', values = colors) +
  coord_flip() +
  scale_y_continuous(breaks = c(0,30,60,90,120), labels = c('0', '30','60', '90', '120'), limits = c(0, 120)) +
  #scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000), labels = c('0', '1000', '2000', '3000', '4000', '5000'), limits = c(0, 5500)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_blank(),
        axis.line = element_line(colour="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.key.height = unit(.8, "cm"),
        legend.key.width = unit(.8, "cm"),
        legend.title = element_text(size = 12),
        legend.position = c(0.7,0.5)) +
  guides(fill = guide_legend(ncol = 1, reverse=T)) +
  ggtitle("Females") +
  xlab('Death rate per 100 000')

axis <- ggplot(unique(region_age[year_id == 2019 & venom == 'snake' & sex_id == 1, c('age_group_name')]), aes(x=factor(age_group_name), y=1)) +
  geom_text(aes(label=factor(age_group_name)), position="identity", size = 4)+
  coord_flip() +
  scale_y_continuous()+
  ggtitle("")+
  ylab(NULL)+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA))


pdf(paste0('[DIRECTORY]/', metric, '_age_sex_region_bar_new.pdf'), width=15, height=8.5)
gridExtra::grid.arrange(m_rate,axis,f_rate,ncol=3,widths=c(10,2,10))
dev.off()

write_csv(region_age, paste0('[DIRECTORY]/region_age_', metric, '.csv'))


male <- ggplot(dt_male, aes(x=factor(lancet_label, levels=unique(dt_male[,lancet_label])),
                            y=val)) +
  geom_bar(data=dt_male,
           stat='identity',
           width=.7,
           aes(fill=factor(age_group_id, levels=c('70 plus', '50 to 69', '15 to 49', '5 to 14', 'Under 5')))) +
  coord_flip() +
  scale_x_discrete(position = "top") +
  scale_y_reverse(limits = c(60000,0), labels=c('0','20,000','40,000','60,000')) +
  scale_fill_manual(values=colors) +
  guides(fill = guide_legend(ncol = 1, reverse=T)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_blank(),
        axis.line = element_line(colour="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position = "none") +
  guides(fill = guide_legend(ncol = 1, reverse=T)) +
  labs(title = "Males") 



m_num <- ggplot(region_age[venom == 'snake' & year_id == 2019 & sex_id == 1 &
                    region_name %in% c('Central Sub-Saharan Africa', 'Eastern Sub-Saharan Africa', 
                                       'South Asia', 'Western Sub-Saharan Africa')], 
       aes(x = factor(age_group_name), y = mean_region_age)) +
  facet_wrap(~region_name, scales = 'free', ncol = 4) + 
  geom_bar(stat = 'identity', fill = 'dark green') + theme_bw() +
  xlab('Age group') + ylab('Snakebite deaths') +
  ggtitle('Male snakebite deaths')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

f_num <- ggplot(region_age[venom == 'snake' & year_id == 2019 & sex_id == 2 &
                    region_name %in% c('Central Sub-Saharan Africa', 'Eastern Sub-Saharan Africa', 
                                       'South Asia', 'Western Sub-Saharan Africa')], 
       aes(x = factor(age_group_name), y = mean_region_age)) +
  facet_wrap(~region_name, scales = 'free', ncol = 4) + 
  geom_bar(stat = 'identity', fill = 'dark green') + theme_bw() +
  xlab('Age group') + ylab('Snakebite deaths') +
  ggtitle('Female snakebite deaths')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(gridExtra)
grid.arrange(m_rate, f_rate, m_num, f_num, ncol = 1)

sdi <- get_covariate_estimates(1099, gbd_round_id = 6, decomp_step = 'step4')


ggplot(super_region_plot[venom == 'snake' & year_id == 2019], aes(x = factor(age_group_id), y = mean_age, fill = super_region_name)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~sex_id) +
  theme_bw()

ggplot(super_region_plot[venom == 'snake' & year_id == 2019], aes(x = factor(age_group_id), y = rate, fill = super_region_name)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~sex_id) +
  theme_bw()


