## Map GBD venomous animal contact results

source('[DIRECTORY]/get_location_metadata.R')
source('[DIRECTORY]/get_outputs.R')

locs <- get_location_metadata(35, gbd_round_id = 6)

venom_results <- get_outputs('cause', cause_id = 710, measure_id = 1, compare_version_id = 7226,
                             location_id = unique(locs$location_id), age_group_id = 22,
                             year_id = c(2019), sex_id = 3, decomp_step = 'step4',
                             metric_id = 3, gbd_round_id = 6)

ref_plot_df <- fread('[DIRECTORY]/age_standardized_mapping_df_csmr_bees.csv')

venom_results_map <- venom_results[location_id %in% unique(ref_plot_df$location_id)]
venom_results_map[, val := val*100000]
setnames(venom_results_map, 'val', 'mapvar')

source("[DIRECTORY]/2019GBD_MAP-copy.R")

pdf(file = '[DIRECTORY]/gbd_2019_venom_all_ages_both_sex.pdf', height = 5, width = 8, pointsize = 6.5)
gbd_map(data = venom_results_map,  
        limits = c(0, 0.05, 0.1, 1, 3, 7),
        label = c('0-0.05', '>0.05-0.1', '>0.1-1', '>1-3', '>3-6'),
        col = 'RdYlBu',
        col.reverse = TRUE,
        na.color = 'Gray',
        legend.title = 'All ages mortality rate (per 100,000)',
        title = "GBD 2019 venomous animal contact mortality",
        fname = "[DIRECTORY]/all_ages_results_map.pdf",
        legend.cex = .6)
dev.off()
