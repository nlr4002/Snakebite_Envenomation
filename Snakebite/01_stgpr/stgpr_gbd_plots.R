### Plot results for snakebite

library(data.table)
library(magrittr)
library(ggplot2)
library(readr)


source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_population.R')
source('[DIRECTORY]/get_outputs.R')

locs <- get_location_metadata(35, gbd_round_id = 6)
ages <- get_age_metadata(12)
ages <- fread('[DIRECTORY]/age_group_ids_2019-11-21_02-42-PM.csv')

# ST-GPR libraries
central_root <- '[DIRECTORY]'
setwd(central_root)

source('r_functions/registration/register.R')
source('r_functions/registration/sendoff.R')
source('r_functions/utilities/utility.r')


# Get data
data_snake <- model_load(run_snake, 'data') %>% .[, venom := 'snake']
data_bees <- model_load(run_bees, 'data') %>% .[, venom := 'bees']
data_scorpion <- model_load(run_scorpion, 'data') %>% .[, venom := 'scorpion']
data_spider <- model_load(run_spider, 'data') %>% .[, venom := 'spider']
data_other <- model_load(run_other, 'data') %>% .[, venom := 'other']

datadf <- rbind(data_snake, data_bees, data_scorpion, data_spider, data_other)

# Get results. You want unraked and most_detailed locations used in ST-GPR. May need to aggregate/Rake
snake <- model_load(run_snake, 'raked') %>% .[, venom := 'snake']
bees <- model_load(run_bees, 'raked') %>% .[, venom := 'bees']
scorpion <- model_load(run_scorpion, 'raked') %>% .[, venom := 'scorpion']
spider <- model_load(run_spider, 'raked') %>% .[, venom := 'spider']
other <- model_load(run_other, 'raked') %>% .[, venom := 'other']

results <- rbind(snake, bees, scorpion, spider, other)

# Get just most_detailed because that's where we have data
# Also make a variable for if it's a location with data or not
results <- results[location_id %in% locs[most_detailed == 1]$location_id]
results[location_id %in% unique(datadf$location_id), has_data := 1]
results[!(location_id %in% unique(datadf$location_id)), has_data := 0]
results <- merge(results, ages, by = 'age_group_id', all.x = TRUE)


# Set up GBD plot
# Calculate proportion of cause by certain venom and apply to venom. Not doing by uncertainty for this result.
results[, total := sum(gpr_mean), by = c('age_group_id', 'location_id', 'year_id', 'sex_id')]
results[, cause_prop := gpr_mean/total]


gbd_2019 <- fread('[DIRECTORY]/venom_animal_contact_result_gbd2019.csv')
#write_csv(gbd_2019, '[DIRECTORY]/venom_animal_contact_result_gbd2019_yll.csv')
gbd_2019_yll <- fread('[DIRECTORY]/venom_animal_contact_result_gbd2019_yll.csv')

metric <- 1
gbd_2019_young <- get_outputs('cause', cause_id = 710, measure_id = metric,
                        location_id = unique(locs$location_id), age_group_id = c(4,5),
                        year_id = c(1990:2019), sex_id = c(1,2), decomp_step = 'step4',
                        metric_id = 1, gbd_round_id = 6, version = 'latest')

gbd_2019_young <- fread('[DIRECTORY]/venom_animal_contact_result_gbd2019_age_4_5.csv')
gbd_2019_young_yll <- fread('[DIRECTORY]/venom_animal_contact_result_gbd2019_age_4_5_yll.csv')



gbd_2019 <- rbind(gbd_2019, gbd_2019_young)
gbd_2019_yll <- rbind(gbd_2019_yll, gbd_2019_young_yll)

# Just aggregate right now at the non-draw level for plotting purposes
results_young <- results[age_group_id %in% c(389, 388, 238, 34)]


pop <- get_population(age_group_id = unique(results_young$age_group_id), 
                      location_id = unique(results_young$location_id),
                      year_id = unique(results_young$year_id),
                      sex_id = c(1,2), gbd_round_id = 7, decomp_step = 'step2')



# taking weighted mean for now. This is a hack and need to do at draw level!!! Just for presentation purposes
results_young <- merge(results_young, pop, by = c('age_group_id', 'location_id', 'year_id', 'sex_id'), all.x = TRUE)

results_young[age_group_id == 389 | age_group_id== 388, age_group_id := 4]
results_young[age_group_id == 238 | age_group_id == 34, age_group_id := 5]

results_young[, gpr_mean := weighted.mean(gpr_mean, w = population), by = c('age_group_id', 'location_id', 'year_id', 'sex_id', 'venom')]
results_young[, gpr_lower := weighted.mean(gpr_lower, w = population), by = c('age_group_id', 'location_id', 'year_id', 'sex_id', 'venom')]
results_young[, gpr_upper := weighted.mean(gpr_upper, w = population), by = c('age_group_id', 'location_id', 'year_id', 'sex_id', 'venom')]

results_young[, total := sum(gpr_mean), by = c('age_group_id', 'location_id', 'year_id', 'sex_id')]
results_young[, cause_prop := gpr_mean/total]

results_young <- results_young[, c('age_group_id', 'location_id', 'year_id', 'sex_id', 
                                   'gpr_mean', 'gpr_lower', 'gpr_upper', 'venom', 'total', 'cause_prop')] %>% 
  unique()

results <- results[!(age_group_id %in% c(389, 388, 238, 34))]
results <- rbind(results, results_young, fill = TRUE)

results <- results[, c('age_group_id', 'location_id', 'year_id', 'sex_id', 
                       'gpr_mean', 'gpr_lower', 'gpr_upper', 'venom', 'total', 'cause_prop')]

cod_venom <- copy(gbd_2019)
cod_venom <- cod_venom[, c('age_group_id', 'location_id', 'sex_id', 'year_id', 'location_name', 'sex',
                           'val', 'upper', 'lower')]
gbd17 <- merge(cod_venom, results, by = c('age_group_id', 'location_id', 'year_id', 'sex_id')) 
gbd17[, venom_deaths := cause_prop*val]

setorder(gbd17, age_group_id)
gbd17 <- merge(gbd17, ages[, c('age_group_id', 'age_group_name')], by = 'age_group_id')
gbd17[, age_group_name := factor(age_group_name, levels = unique(gbd17$age_group_name))]

age_groups <- unique(gbd17[, c('age_group_id', 'age_group_name')])
results <- merge(results, age_groups, by = 'age_group_id')
datadf <- merge(datadf, age_groups, by = 'age_group_id')

results <- merge(results, locs[, c('location_id', 'region_name', 'location_name', 'sort_order')], by = 'location_id')
setorder(results, sort_order)

results[sex_id == 1, sex := 'Male'][sex_id == 2, sex := 'Female']
datadf[sex_id == 1, sex := 'Male'][sex_id == 2, sex := 'Female']
gbd17[sex_id == 1, sex := 'Male'][sex_id == 2, sex := 'Female']

write_csv(gbd17, '[DIRECTORY]/deaths_for_thesis.csv')


# Go region by region, then location then sex. Making pdf at region specific level to easily scan.
# This is most detailed

date <- Sys.Date()
dir <- paste0('[DIRECTORY]/stgpr_results/', date, '/')
dir.create(dir)
mclapply(unique(results$region_name), function(region){
  print(region)
  
  tmp <- results[region_name == region]
  pdf(paste0(dir, region, '_results_200_submodel_raked.pdf'), 
      onefile = TRUE, width = 10, height = 10)
  for(loc in unique(tmp$location_id)){
    print(loc)
    tmp1 <- tmp[location_id == loc]
    for(sex_var in unique(tmp1$sex)){
      tmp2 <- tmp1[sex == sex_var]
      loc_name <- locs[location_id == loc]$location_name
      p_gpr <- ggplot(tmp2, aes(x = year_id, y = gpr_mean, color = venom)) +
        geom_line() +
        geom_ribbon(aes(ymin = gpr_lower, ymax = gpr_upper, fill = venom), alpha = 0.3) +
        geom_point(data = datadf[location_id == loc & sex == sex_var & is_outlier == 0], aes(x = year_id, y = data), size = 2) +
        facet_wrap(~age_group_name, scales = 'free_y') +
        theme_bw() +
        labs(x = 'Year', y = 'Death rate per 100,000',
             title = paste0(loc_name, ' - ', sex_var, ' - ST-GPR results and data')) +
        scale_fill_manual(name = '', values = c('#9e379f', '#eb8c00', '#B7D968', '#7CCCE5', '#555E7B')) +
        scale_color_manual(name = '', values = c('#9e379f', '#eb8c00', '#B7D968', '#7CCCE5', '#555E7B'))
      
      p_gbd <- ggplot(gbd17[sex == sex_var & location_id == loc], aes(x = year_id, y = venom_deaths)) +
        geom_bar(stat = 'identity', aes(fill = venom), alpha = 0.7) +
        theme_bw() +
        labs(x = 'Year', y = 'Deaths (#)',
             title = paste0(loc_name, ' - ', sex_var, ' - GBD estimate of deaths from venomous species')) +
        facet_wrap(~age_group_name, scales = 'free_y') +
        scale_fill_manual(name = '', values = c('#9e379f', '#eb8c00', '#B7D968', '#7CCCE5', '#555E7B'))
      
      # p_gbd_2017 <- ggplot(gbd17[location_id == loc & year_id ==2017], aes(x = factor(age_group_name), y = venom_deaths)) +
      #   geom_bar(stat = 'identity', aes(fill = venom), alpha = 0.7) +
      #   theme_bw() +
      #   labs(x = 'Age group', y = 'Deaths (#)', 
      #        title = paste0(loc_name, ' - GBD estimate of deaths from venomous species for year 2017')) +
      #   facet_wrap(~sex, scales = 'free_y') +
      #   scale_fill_manual(name = '', values = c('#9e379f', '#eb8c00', '#B7D968', '#7CCCE5', '#555E7B')) +
      #  theme(axis.text.x = element_text(angle = 90, hjust = 1))
      #grid.arrange(p_gpr, p_gbd, nrow = 1)
      print(p_gpr)
      print(p_gbd)
      #print(p_gbd_2017)
    }
  }
  dev.off()
}, mc.cores = 10)


## Make 2017 age x-axis results. Just gbd
bar <- gbd17[year_id == 2019]
subnat <- locs[level == 3 & most_detailed != 1]$location_id

# Level 3 countries:
# Need to calculate proportions and apply

cod_venom <- fread('[DIRECTORY]/venom_animal_contact_result.csv')

cod_venom <- cod_venom[, c('age_group_id', 'location_id', 'sex_id', 'year_id', 'age_group_name', 'location_name', 'sex',
                           'val', 'upper', 'lower')]

# Run this after aggregating draws
pdf('[DIRECTORY]/.pdf', onefile = T, width = 9, height = 9)
plot_locs <- get_location_metadata(35, gbd_round_id = 5) 
for(loc in plot_locs[level == 3]$location_id){
  loc_name <- locs[location_id == loc]$location_name
  print(loc_name)
  
  df <- fread(paste0('[DIRECTORY]', loc, '.csv'))
  df[, mean_prop := mean(cause_prop), by = c('age_group_id', 'year_id', 'sex_id', 'location_id', 'venom')]

  df <- unique(df[, c('age_group_id', 'year_id', 'sex_id', 'location_id', 'venom', 'mean_prop')])
  
  df <- merge(df, cod_venom, by = c('age_group_id', 'location_id', 'sex_id', 'year_id'))
  df[, deaths := mean_prop*val]
  setorder(df, age_group_id)
  df[, age_group_name := factor(age_group_name, levels = unique(df$age_group_name))]
 
  p <- ggplot(df, aes(x = age_group_name, y = deaths, fill = venom)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~sex) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0(loc_name, ' - Deaths from different venomous species'),
         x = 'Age group',
         y = 'Deaths (#)') +
    scale_fill_manual(name = '', values = c('#9e379f', '#eb8c00', '#B7D968', '#7CCCE5', '#555E7B'))
  
  print(p)
   
}

dev.off()

# Make map
mapdf <-copy(gbd17)
write_csv(mapdf, '[DIRECTORY]/mapdf_plotdf_yll.csv')

ages <- get_age_metadata(12, gbd_round_id = 6)
pop <- get_population(age_group_id = unique(mapdf$age_group_id), year_id = c(1990:2019), sex_id = c(1,2), location_id = unique(mapdf$location_id),
                      gbd_round_id = 6, decomp_step = 'step4')

mapdf <- merge(mapdf, ages[, c('age_group_id', 'age_group_weight_value')], by = 'age_group_id', all.x = TRUE)
mapdf <- merge(mapdf, pop, by = c('age_group_id', 'sex_id', 'location_id', 'year_id'), all.x = TRUE)

# This is by location
mapdf[, all_age := sum(venom_deaths), by = c('location_id', 'sex_id', 'year_id', 'venom')]
mapdf[, all_age_both_sex := sum(venom_deaths), by = c('location_id', 'year_id', 'venom')]

mapdf[, rate := venom_deaths/population]
mapdf[, as_rate := weighted.mean(rate, w = age_group_weight_value), by = c('venom', 'location_id', 'sex_id', 'year_id')]
mapdf[, as_rate_both_sex := weighted.mean(as_rate, w = population), by = c('venom', 'location_id', 'year_id')]

# Go down the list, calculating global, super region, region, country! (use ISO3)
# Calculate sum, rate, AS rate

mapdf[, global_count := sum(venom_deaths), by = c('venom', 'year_id', 'sex_id')]
mapdf[, global_by_age := sum(venom_deaths), by = c('venom', 'year_id', 'sex_id', 'age_group_id')]
mapdf[, global_rate_by_age := weighted.mean(rate, w = population), by = c('venom', 'year_id', 'sex_id', 'age_group_id')]

global <- unique(mapdf[, c('venom', 'year_id', 'sex_id', 'age_group_id', 'age_group_weight_value',
                           'global_by_age', 'global_rate_by_age', 'global_count')])
global[, as_rate_by_sex := weighted.mean(global_rate_by_age, w = age_group_weight_value), by = c('venom', 'year_id', 'sex_id')]
global[, as_rate_both_sex := weighted.mean(global_rate_by_age, w = age_group_weight_value), by = c('venom', 'year_id')]
global[, as_rate_by_sex := as_rate_by_sex*100000]
global[, as_rate_both_sex := as_rate_both_sex*100000]

# By ISO3
mapdf <- merge(mapdf, locs[, c('location_id', 'ihme_loc_id', 'region_name', 'super_region_name')], by = c('location_id'), all.x = TRUE)
mapdf[, iso3 := substr(ihme_loc_id, 1,3)]
mapdf[, iso3_count := sum(venom_deaths), by = c('venom', 'year_id', 'sex_id', 'iso3')]
mapdf[, iso3_count_by_age := sum(venom_deaths), by = c('venom', 'year_id', 'sex_id', 'age_group_id', 'iso3')]
mapdf[, iso3_rate_by_age := weighted.mean(rate, w = population), by = c('venom', 'year_id', 'sex_id', 'age_group_id', 'iso3')]

country <- unique(mapdf[, c('venom', 'year_id', 'sex_id', 'age_group_id', 'age_group_weight_value', 'iso3',
                           'iso3_count_by_age', 'iso3_rate_by_age', 'iso3_count')])
country[, as_rate_by_sex := weighted.mean(iso3_rate_by_age, w = age_group_weight_value), by = c('venom', 'year_id', 'sex_id', 'iso3')]
country[, as_rate_both_sex := weighted.mean(iso3_rate_by_age, w = age_group_weight_value), by = c('venom', 'year_id', 'iso3')]
country[, as_rate_by_sex := as_rate_by_sex*100000]
country[, as_rate_both_sex := as_rate_both_sex*100000]

map_pop <- get_population(age_group_id = 22, sex_id = 3, location_id = unique(mapdf$location_id), 
                          year_id = c(1990:2019), gbd_round_id = 6, decomp_step = 'step4')
setnames(map_pop, 'population', 'all_age_population')

mapdf <- merge(mapdf, map_pop[, c('location_id', 'year_id', 'all_age_population')], by = c('location_id', 'year_id'))
mapdf[, all_age_rate := (all_age_both_sex/all_age_population)*100000]

# Calculate global
mapdf[, global := sum(all_age_both_sex), by = c('year_id', 'venom')]
mapdf[, global_all := sum(all_age_both_sex), by = c('year_id')]
mapdf[, global_pop := sum(population), by = c('year_id', 'venom')]
mapdf[, global_rate := global/global_pop*100000]


mapdf <- merge(mapdf, locs[, c('location_id', 'ihme_loc_id', 'super_region_name')], by = 'location_id')
mapdf[, super_region := sum(all_age_both_sex), by = c('year_id', 'venom', 'super_region_name')]
mapdf[, super_region_all := sum(all_age_both_sex), by = c('year_id', 'super_region_name')]
mapdf[, super_region_pop := sum(population), by = c('year_id', 'venom', 'super_region_name')]
mapdf[, super_region_rate := super_region/super_region_pop*100000]
mapdf[, super_region_rate_all := super_region_all/super_region_pop*100000]
sr <- unique(mapdf[year_id == 2017, c('venom', 'super_region_name', 'super_region', 'super_region_pop', 'super_region_rate', 'super_region_rate_all')])
sr[, super_region_prop := super_region_rate/super_region_rate_all]

ggplot(sr, aes(x = venom, y = super_region_name, fill = log(super_region_rate))) +
  geom_tile() +
  theme_bw() +
  scale_fill_gradient(low = 'white', high = 'red', name = 'Rate per 100,000') +
  geom_text(aes(label = round(super_region_rate, 3)), size = 10) +
  ylab('Super region') +
  xlab('Venomous species') +
  scale_x_discrete(position = "top") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(size = 16, color = 'black'),
        axis.text.y = element_text(color="black", size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

mapdf <- merge(mapdf, locs[, c('location_id', 'region_name')],
               by = 'location_id')
mapdf[, region := sum(all_age_both_sex), by = c('year_id', 'venom', 'region_name')]
mapdf[, region_all := sum(all_age_both_sex), by = c('year_id', 'region_name')]
mapdf[, region_pop := sum(population), by = c('year_id', 'venom', 'region_name')]
mapdf[, region_rate := region/region_pop*100000]
mapdf[, region_rate_all := region_all/region_pop*100000]
r <- unique(mapdf[year_id == 2017, c('venom', 'region_name', 'region', 'region_pop', 'region_rate', 'region_rate_all')])
r[, region_prop := region_rate/region_rate_all]

setorder(r, region_rate_all)
r[, label := (factor(region_name, levels = unique(r$region_name)))]
ggplot(r, aes(x = label, y = region_rate, fill = venom)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(name = '', values = c('#9e379f', '#eb8c00', '#B7D968', '#7CCCE5', '#555E7B')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab('Rate per 100k') +
  xlab('') +
  theme(axis.text = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16))



# Aggregate IND and ZAF
agg <- mapdf[ihme_loc_id %like% 'IND']
agg <- merge(agg, locs[, c('location_id', 'parent_id')], by = 'location_id')
agg[, state_rate := weighted.mean(rate, w = population), by = c('year_id', 'venom', 'parent_id')]
agg[, state_count := sum(all_age_both_sex), by = c('year_id', 'venom', 'parent_id')]
agg <- unique(agg[, c('parent_id', 'year_id', 'venom', 'state_rate', 'state_count')])
setnames(agg, 'parent_id', 'location_id')
setnames(agg, 'state_rate', 'rate')
setnames(agg, 'state_count', 'all_age_both_sex')
agg <- merge(agg, locs[, c('location_id', 'ihme_loc_id')], by = 'location_id')

mapdf <- mapdf[!(ihme_loc_id %like% 'IND')]
mapdf <- rbind(mapdf, agg, fill = TRUE)

# ZAF
agg <- mapdf[ihme_loc_id %like% 'ZAF']
agg <- merge(agg, locs[, c('location_id', 'parent_id')], by = 'location_id')
agg[, state_rate := weighted.mean(rate, w = population), by = c('year_id', 'venom', 'parent_id')]
agg[, state_count := sum(all_age_both_sex), by = c('year_id', 'venom', 'parent_id')]
agg <- unique(agg[, c('parent_id', 'year_id', 'venom', 'state_rate', 'state_count')])
setnames(agg, 'parent_id', 'location_id')
setnames(agg, 'state_rate', 'rate')
setnames(agg, 'state_count', 'all_age_both_sex')
agg <- merge(agg, locs[, c('location_id', 'ihme_loc_id')], by = 'location_id')

mapdf <- mapdf[!(ihme_loc_id %like% 'ZAF')]
mapdf <- rbind(mapdf, agg, fill = TRUE)


source("[DIRECTORY]/GBD_WITH_INSETS_MAPPING_FUNCTION.R")

# Make map in loop over venom
for(v in unique(mapdf$venom)){
  print(v)
  tmp <- mapdf[venom == v & year_id == 2017]
  setnames(tmp, 'rate', 'mapvar')
  
  
  if(v == 'snake'){
    lim = c(0, 0.01, 0.03, 0.1, 1 , 3, 5)
    lab <- c('0-0.01','>0.01-0.03', '>0.03-0.1', '>0.1-1', '>1-3', '>3-4.7')
  }
  if(v == 'scorpion'){
    lim = c(0, 0.001, 0.01, 0.1, 0.3, 0.6, 1.4)
    lab <- c('0-0.001','>0.001-0.01', '>0.01-0.1', '>0.1-0.3', '>0.3-0.6', '>0.6-1.4')
  } 
  if(v == 'spider'){
    lim = c(0, 0.001, 0.005, 0.01, 0.05 , 0.1, 0.2)
    lab <- c('0-0.001','>0.001-0.005', '>0.005-0.01', '>0.01-0.05', '>0.05-0.1', '>0.1-0.13')
  }
  if(v == 'other'){
    lim = c(0, 0.005, 0.01, 0.05 , 0.1, 1, 3.2)
    lab <- c('0-0.005', '>0.005-0.01', '>0.01-0.05', '>0.05-0.1', '>0.1-1', '>0.3-3.1')
  }
  if(v == 'bees'){
    lim = c(0, 0.005, 0.01, 0.05 , 0.1, 0.3, 0.8)
    lab <- c('0-0.005', '>0.005-0.01', '>0.01-0.05', '>0.05-0.1', '>0.1-0.3', '>0.3-0.7')
  }
  
  
  
  gbd_map(data=tmp,
          limits=lim, # change to whatever bins make sense for your data
          label=lab, # label bins in the legendsdfds
          col='RdYlBu', # choose palette
          na.color = 'Gray',
          legend.title = 'Death rate per 100,000',
          #col=c('chartreuse4', 'chartreuse'), # choose palette
          col.reverse=T, #reverse palette if you want
          title=paste0(v, ' all ages, both sex, 2017 rate of deaths'), # map title
          fname=paste0('[DIRECTORY]/'),
          legend.cex = 0.8) # save as .tif .eps or .pdf
}

# Count space
for(v in unique(mapdf$venom)){
  print(v)
  tmp <- mapdf[venom == v & year_id == 2017]
  setnames(tmp, 'all_age_both_sex', 'mapvar')
  
  if(v == 'snake'){
    lim = c(0, 1, 5, 10, 500 , 1000, 11000)
    lab <- c('0','1-5', '>5-10', '>10-500', '>500-1000', '>1000-11,000')
  }
  if(v == 'scorpion'){
    lim = c(0, 1, 2, 5, 20 , 100)
    lab <- c('0','>1-2', '>2-5', '>5-20', '>20-95')
  } 
  if(v == 'spider'){
    lim = c(0, 1, 2, 5, 20 , 100, 500)
    lab <- c('0','>1-2', '>2-5', '>5-20', '>20-100', '>100-500')
  }
  if(v == 'other'){
    lim = c(0, 1, 5, 10, 30, 70, 150, 700)
    lab <- c('0', '1-5', '>5-10', '>10-30', '>30-70', '>70-150', '>150-700')
  }
  if(v == 'bees'){
    lim = c(0, 1, 2, 5, 20, 50, 100, 600)
    lab <- c('0', '1-2', '>2-5', '>5-20', '>20-50', '>50-100', '>100-600')
  }
  
  
  
  gbd_map(data=tmp,
          limits=lim, # change to whatever bins make sense for your data
          label=lab, # label bins in the legendsdfds
          col='RdYlBu', # choose palette
          na.color = 'Gray',
          legend.title = 'Number of deaths',
          #col=c('chartreuse4', 'chartreuse'), # choose palette
          col.reverse=T, #reverse palette if you want
          title=paste0(v, ' all ages, both sex, 2017 death count'), # map title
          fname=paste0('[DIRECTORY].pdf'),
          legend.cex = 0.8) # save as .tif .eps or .pdf
}



