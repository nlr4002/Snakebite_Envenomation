#### Snakebite mortality
#### Make the results over time + forecasting

library(data.table)
library(ggplot2)

source('[DIRECTORY]get_age_metadata.R')
ages <- get_age_metadata(12, gbd_round_id = 6)

# Read in 1990 to 2019 results by region
# Make age-standardized rate by year
df <- fread('[DIRECTORY]/region_snake_draws.csv')
df <- melt(df, id.vars = c('age_group_id', 'sex_id', 'year_id', 'location_id'),
           value.vars = patterns('draw_'))
df <- merge(df, ages[, c('age_group_id', 'age_group_weight_value')])
df[, as_rate := weighted.mean(value, w = age_group_weight_value), by = c('variable', 'sex_id', 'year_id', 'location_id')]
df <- unique(df[, c('variable', 'sex_id', 'year_id', 'location_id', 'as_rate')])
df[, mean_draw := mean(as_rate), by = c('sex_id', 'year_id', 'location_id')]
df[, lo_draw := quantile(as_rate, 0.025), by = c('sex_id', 'year_id', 'location_id')]
df[, hi_draw := quantile(as_rate, 0.975), by = c('sex_id', 'year_id', 'location_id')]

plotdf <- unique(df[, c('sex_id', 'year_id', 'location_id', 'mean_draw', 'lo_draw', 'hi_draw')])
plotdf <- merge(plotdf, locs[, c('location_id', 'location_name')])
plotdf[sex_id == 1, sex := 'Male'][sex_id == 2, sex := 'Female']

plot_locs <- c('South Asia', 'Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa', 'Central Sub-Saharan Africa',
               'Oceania', 'Southeast Asia')

ggplot(plotdf[location_name %in% plot_locs], aes(x = year_id, y = mean_draw*100000, color = location_name)) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = lo_draw, ymax = hi_draw, fill = location_name), alpha = 0.05) +
  facet_wrap(~sex) +
  theme_bw() +
  xlab('Year') +
  ylab('Age-standardized mortality rate per 100,000') +
  scale_color_brewer(name = 'Region', palette = 'Dark2')

