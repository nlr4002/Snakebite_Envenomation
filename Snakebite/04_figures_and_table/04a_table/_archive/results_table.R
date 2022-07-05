## Make the main table for the plot, at counry and region level. 

locs <- get_location_metadata(35, gbd_round_id = 6)

csmr_country <- fread('[DIRECTORY]/csmr_country_results_count_asr.csv')
csmr_country <- csmr_country[venom == 'snake' & year_id == 2019]
csmr_country <- merge(csmr_country, locs[, c('location_id', 'sort_order')], by = 'location_id')
setorder(csmr_country, sort_order)

# Do counts
csmr_country[, mean_count_round := round(mean_count, 0)]
csmr_country[, lo_count_round := round(lo_count, 0)]
csmr_country[, hi_count_round := round(hi_count, 0)]
csmr_country[, tab_count := paste0(prettyNum(mean_count_round, big.mark = " "), " (", 
                                   prettyNum(lo_count_round, big.mark = " "), "-",
                                   prettyNum(hi_count_round, big.mark = " "), ")")]
setnames(csmr_country, 'tab_count', "Count, 2019")

csmr_country[, mean_as_round :=sprintf('%.2f',mean_as)]
csmr_country[, lo_as_round := sprintf('%.2f',lo_as)]
csmr_country[, hi_as_round := sprintf('%.2f',hi_as)]
csmr_country[, tab_as := paste0(mean_as_round, " (", 
                                lo_as_round, "-",
                                hi_as_round, ")")]

setnames(csmr_country, 'tab_as', "Age-standardised rate, 2019")

csmr_country_wide <- fread('[DIRECTORY]/csmr_country_wide_snake.csv')
csmr_country_wide[, mean_diff_count := mean_diff_count - 1]
csmr_country_wide[, lo_diff_count := lo_diff_count - 1]
csmr_country_wide[, hi_diff_count := hi_diff_count - 1]

csmr_country_wide[, mean_diff_as := (mean_diff_as - 1)*100]
csmr_country_wide[, lo_diff_as := (lo_diff_as - 1)*100]
csmr_country_wide[, hi_diff_as := (hi_diff_as - 1)*100]

csmr_country_wide[, tab_diff := paste0(sprintf('%.1f',mean_diff_as), "% (",
                                      sprintf('%.1f',lo_diff_as), " - ",
                                      sprintf('%.1f',hi_diff_as), ")")]

deaths <- merge(csmr_country[, c('location_name', 'sort_order', 'Count, 2019', 'Age-standardised rate, 2019')],
                csmr_country_wide[, c('location_name', 'tab_diff')],
                by = 'location_name')
setorder(deaths, sort_order)

# YLL
yll_country <- fread('[DIRECTORY]/yll_country_results_count_asr.csv')
yll_country <- yll_country[venom == 'snake' & year_id == 2019]
yll_country <- merge(yll_country, locs[, c('location_id', 'sort_order')], by = 'location_id')
setorder(yll_country, sort_order)

# Do counts
yll_country[, mean_count_round := round(mean_count, 0)]
yll_country[, lo_count_round := round(lo_count, 0)]
yll_country[, hi_count_round := round(hi_count, 0)]
yll_country[, tab_count := paste0(prettyNum(mean_count_round, big.mark = " "), " (", 
                                   prettyNum(lo_count_round, big.mark = " "), "-",
                                   prettyNum(hi_count_round, big.mark = " "), ")")]
setnames(yll_country, 'tab_count', "Count, 2019")

yll_country[, mean_as_round :=sprintf('%.2f',mean_as)]
yll_country[, lo_as_round := sprintf('%.2f',lo_as)]
yll_country[, hi_as_round := sprintf('%.2f',hi_as)]
yll_country[, tab_as := paste0(mean_as_round, " (", 
                                lo_as_round, "-",
                                hi_as_round, ")")]

setnames(yll_country, 'tab_as', "Age-standardised rate, 2019")

yll_country_wide <- fread('[DIRECTORY]/yll_country_wide_snake.csv')
yll_country_wide[, mean_diff_count := (mean_diff_count - 1)*100]
yll_country_wide[, lo_diff_count := (lo_diff_count - 1)*100]
yll_country_wide[, hi_diff_count := (hi_diff_count - 1)*100]

yll_country_wide[, mean_diff_as := (mean_diff_as - 1)*100]
yll_country_wide[, lo_diff_as := (lo_diff_as - 1)*100]
yll_country_wide[, hi_diff_as := (hi_diff_as - 1)*100]

yll_country_wide[, tab_diff := paste0(sprintf('%.1f',mean_diff_as), "% (",
                                      sprintf('%.1f',lo_diff_as), " - ",
                                      sprintf('%.1f',hi_diff_as), ")")]

ylls <- merge(yll_country[, c('location_name', 'sort_order', 'Count, 2019', 'Age-standardised rate, 2019')],
                yll_country_wide[, c('location_name', 'tab_diff')],
                by = 'location_name')
setorder(ylls, sort_order)


## Regional level
csmr_region <- fread('[DIRECTORY]/csmr_region_results.csv')
csmr_region <- csmr_region[year_id == 2019]
csmr_region[, mean_count_round := round(mean_count, 0)]
csmr_region[, lo_count_round := round(lower_count, 0)]
csmr_region[, hi_count_round := round(upper_count, 0)]
csmr_region[, tab_count := paste0(prettyNum(mean_count_round, big.mark = " "), " (", 
                                   prettyNum(lo_count_round, big.mark = " "), "-",
                                   prettyNum(hi_count_round, big.mark = " "), ")")]
setnames(csmr_region, 'tab_count', "Count, 2019")

csmr_region[, mean_as_round :=sprintf('%.2f',mean_asdr)]
csmr_region[, lo_as_round := sprintf('%.2f',lower_asdr)]
csmr_region[, hi_as_round := sprintf('%.2f',upper_asdr)]
csmr_region[, tab_as := paste0(mean_as_round, " (", 
                                lo_as_round, "-",
                                hi_as_round, ")")]

setnames(csmr_region, 'tab_as', "Age-standardised rate, 2019")

csmr_region_wide <- fread('[DIRECTORY]/csmr_region_results_year_dif.csv')

csmr_region_wide[, mean_diff_as := (mean_change - 1)*100]
csmr_region_wide[, lo_diff_as := (low_change - 1)*100]
csmr_region_wide[, hi_diff_as := (hi_change - 1)*100]

csmr_region_wide[, tab_diff := paste0(sprintf('%.1f',mean_diff_as), "% (",
                                       sprintf('%.1f',lo_diff_as), " - ",
                                       sprintf('%.1f',hi_diff_as), ")")]
region_deaths <- merge(csmr_region[, c('region_name', 'Count, 2019', 'Age-standardised rate, 2019')],
                       csmr_region_wide[, c('region_name', 'tab_diff')], by = 'region_name')
setnames(region_deaths, 'region_name', 'location_name')
region_deaths <- merge(region_deaths, locs[, c('location_name', 'sort_order')], by = 'location_name')

# YLL
yll_region <- fread('[DIRECTORY]/yll_region_results.csv')
yll_region <- yll_region[year_id == 2019]
yll_region[, mean_count_round := round(mean_count, 0)]
yll_region[, lo_count_round := round(lower_count, 0)]
yll_region[, hi_count_round := round(upper_count, 0)]
yll_region[, tab_count := paste0(prettyNum(mean_count_round, big.mark = " "), " (", 
                                  prettyNum(lo_count_round, big.mark = " "), "-",
                                  prettyNum(hi_count_round, big.mark = " "), ")")]
setnames(yll_region, 'tab_count', "Count, 2019")

yll_region[, mean_as_round :=sprintf('%.2f',mean_asdr)]
yll_region[, lo_as_round := sprintf('%.2f',lower_asdr)]
yll_region[, hi_as_round := sprintf('%.2f',upper_asdr)]
yll_region[, tab_as := paste0(mean_as_round, " (", 
                               lo_as_round, "-",
                               hi_as_round, ")")]

setnames(yll_region, 'tab_as', "Age-standardised rate, 2019")

yll_region_wide <- fread('[DIRECTORY]/yll_region_results_year_dif.csv')

yll_region_wide[, mean_diff_as := (mean_change - 1)*100]
yll_region_wide[, lo_diff_as := (low_change - 1)*100]
yll_region_wide[, hi_diff_as := (hi_change - 1)*100]

yll_region_wide[, tab_diff := paste0(sprintf('%.1f',mean_diff_as), "% (",
                                      sprintf('%.1f',lo_diff_as), " - ",
                                      sprintf('%.1f',hi_diff_as), ")")]
region_ylls <- merge(yll_region[, c('region_name', 'Count, 2019', 'Age-standardised rate, 2019')],
                       yll_region_wide[, c('region_name', 'tab_diff')], by = 'region_name')
setnames(region_ylls, 'region_name', 'location_name')
region_ylls <- merge(region_ylls, locs[, c('location_name', 'sort_order')], by = 'location_name')

# put together
deaths_all <- rbind(deaths, region_deaths) %>% setorder(sort_order)
ylls_all <- rbind(ylls, region_ylls) %>% setorder(sort_order)
deaths_all[, sort_order := NULL]
ylls_all[, sort_order := NULL]

setnames(deaths_all, c('Count, 2019', 'Age-standardised rate, 2019', 'tab_diff'),
         c('Deaths - Count, 2019', 'Deaths - Age-standardised rate, 2019', 'Deaths - Percent change from 1990 to 2019'))
setnames(ylls_all, c('Count, 2019', 'Age-standardised rate, 2019', 'tab_diff'),
         c('YLLs - Count, 2019', 'YLLs - Age-standardised rate, 2019', 'Ylls - Percent change from 1990 to 2019'))
ylls_all[, location_name := NULL]
setnames(deaths_all, 'location_name', 'Location')
final_table <- cbind(deaths_all, ylls_all)
write.xlsx(final_table, '[DIRECTORY]/paper_figures/master_table.xlsx')
