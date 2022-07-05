## Covariate analysis for snakebites
library(stringr)
library(ggplot2)
library(lme4)
source('[DIRECTORY]/r/get_covariate_estimates.R')
source('[DIRECTORY]/r/get_ids.R')

venom <- 'snake'
data_df <- fread(paste0('[DIRECTORY]/', venom, 
                        '_NR_rate_data.csv'))


all_sd <- data.table()
for(venom in c('snake', 'bees', 'scorpion', 'spider', 'other')){
  data_df <- fread(paste0('[DIRECTORY]/', venom, 
                          '_NR_rate_data.csv'))
  sd_data <- sd(data_df$data)
  sd_dt <- data.table(venom_type = venom, sd = sd_data)
  all_sd <- rbind(all_sd, sd_dt)
}

cov_ids <- c(1099, 127, 854, 1087, 71, 881, 3, 33, 
             109, 121, 
             719, 118, 119, 
             57, 
             2318, 2321)
all_covs <- data.table()
for(id in cov_ids){
  d1 <- get_covariate_estimates(id, decomp_step = 'step4', gbd_round_id = 6)
  all_covs <- rbind(all_covs, d1)
}
all_covs[covariate_id != 57, sd_cov := sd(mean_value), by = 'covariate_id']
all_covs[covariate_id == 57, sd_cov := sd(log(mean_value))]
cov_sd <- unique(all_covs[, c('covariate_name_short', 'sd_cov')])

## Get model dataframe and go through models
covariates <- get_ids('covariate')

files <- Sys.glob('[DIRECTORY]/')
files <- c("[DIRECTORY]/noise_reduced_snake_prior_test_yes_prior_sign2020_02_13.csv",
           "[DIRECTORY]/noise_reduced_bees_prior_test_yes_prior_sign2020_02_13.csv",
           "[DIRECTORY]/noise_reduced_spider_prior_test_yes_prior_sign2020_02_13.csv",
           "[DIRECTORY]/noise_reduced_scorpion_prior_test_yes_prior_sign2020_02_13.csv",
           "[DIRECTORY]/noise_reduced_other_prior_test_yes_prior_sign2020_02_13.csv")
all_mods <- rbindlist(lapply(files, function(f){
  print(f)
  venom <- str_split(str_split(f, '/')[[1]][6], '_')[[1]][3]
  print(venom)
  mod_df <- fread(f)
  mod_df <- mod_df[drop == 0]
  
  both_sex <- rbindlist(lapply(c('M', 'F'), function(s){
    mod_df_M <- mod_df[sex == s]
    setorder(mod_df_M, out_rmse)
    mod_df_M[, mod := .I]
    
    mod_df_M_long <- melt(mod_df_M, id.vars = c('mod', 'out_rmse', 'covs', 'sex'), value.var = patterns('_fixd'))
    mod_df_M_long <- mod_df_M_long[variable %like% '_fixd$']
    mod_df_M_long <- mod_df_M_long[mod <= 200] # Get 50 best models
    mod_df_M_long <- mod_df_M_long[!is.na(value)]
    mod_df_M_long[, value := as.numeric(value)]
    mod_df_M_long[, out_rmse := as.numeric(out_rmse)]
    mod_df_M_long[, mean := weighted.mean(value, w = out_rmse), by = 'variable']
    betas <- unique(mod_df_M_long[, c('sex', 'variable', 'mean')])
  }))
  both_sex[, venom_type := venom]
  
}))

# standardize betas
all_mods$variable <- gsub('_fixd', '', all_mods$variable)
setnames(all_mods, 'variable', 'covariate_name_short')
all_mods <- merge(all_mods, cov_sd, by = 'covariate_name_short')
all_mods <- merge(all_mods, all_sd, by = 'venom_type')

all_mods[, standard := mean*sd_cov/sd]

all_mods <- merge(all_mods, covariates, by = 'covariate_name_short', all.x = TRUE)
all_mods[covariate_name_short == 'vulnerable_per_pop', covariate_name := 'Snakebite vulnerability']

# Make a heatmap
library(RColorBrewer)
ggplot(all_mods[venom_type == 'snake'], aes(x = covariate_name, y = sex, fill = (standard))) +
  geom_tile() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient2(low = 'navy', mid = 'dark gray', high = 'maroon', midpoint = 0, name = 'Standardised beta') +
  geom_text(aes(label = round(standard, 3)), color = 'white', size = 5.5) +
  xlab('Covariate') +
  ylab('Sex') +
  theme(axis.text = element_text(size = 14))
  # theme(axis.text = element_text(size = 24)) +
  # theme(legend.text = element_text(size = 24)) +
  # theme(legend.title = element_text(size = 24)) +
  # theme(axis.title = element_text(size = 24))

ggplot(all_mods[sex == 'F'], aes(x = covariate_name, 
                                 y = venom_type, fill = (standard))) +
  geom_tile() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient2(low = 'navy', mid = 'light gray', high = 'maroon', midpoint = 0, name = 'Standardised beta') +
  geom_text(aes(label = round(standard, 4)), color = 'white', size = 15) +
  xlab('Covariate') +
  ylab('Venom') +
  theme(axis.text = element_text(size = 24)) +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 24)) +
  theme(axis.title = element_text(size = 24))

mod1 <- all_mods[sex == 'M' & venom_type == 'snake']

ggplot(all_mods[sex == 'M'], aes(x = covariate_name, y = standard)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~venom_type)
  
