### Forecast to 2050 ---------------------------------------------------------------------------------------------------------------------------
### Purpose: Forecast non-fatal estimates for snake bites to the year 2050 by predicting rates and then multiplying by forecasted population


## Set up
rm(list=ls())

if (Sys.info()["sysname"] == "Linux") {
  j_root <- "[DIRECTORY]"
  h_root <- "~/"
  l_root <- "[DIRECTORY]"
} else {
  j_root <- "[DIRECTORY]"
  h_root <- "[DIRECTORY]"
  l_root <- "[DIRECTORY]"
}

pacman::p_load(data.table, openxlsx, ggplot2, magrittr, dplyr, stringr, splines, ggthemes)
library(janitor, lib.loc = "[DIRECTORY]")
date <- gsub("-", "_", Sys.Date())
date <- Sys.Date()

my_dir <- paste0(j_root, "[DIRECTORY]")
draw_dir <- "[DIRECTORY]"
source("[DIRECTORY]/get_location_metadata.R")
source("/[DIRECTORY]/get_age_metadata.R")

#Load dfs used in each iteration
loc_df <- get_location_metadata(35, gbd_round_id=6)
ages_2019 = get_age_metadata(age_group_set_id=22, gbd_round_id=6)

## Set variables here (only one severity for hearing loss)
year_forecast_list <- c(2019, 2020, 2030, 2040, 2050)

#Pull in draws
prev_year_ref <- as.data.table(read.csv(paste0(my_dir, "region_snakes_draws_dismod_years.csv")))

dt <- fread('[DIRECTORY]/region_snake_draws.csv')
dt <- dt[year_id %in% unique(prev_year_ref$year_id)]

for (year_forecast in year_forecast_list){

    print("sequence starting")

    dt1 <- as.data.table(dt)

    ## Set sex and severity variable
    dt1[, sex:= ifelse(sex_id == 1, "male", ifelse(sex_id==2, "female", "both"))]

    ## Create prediction matrix with age, forecasted years, forecasted population, sex, and GBD regions
    #Age
    df_matrix <- as.data.table(ages_2019[,c("age_group_id", "most_detailed")])
    df_matrix <- df_matrix[most_detailed ==1, ]
    df_matrix <- df_matrix[age_group_id!=2 & age_group_id!=3 & age_group_id!=4, ]
    #Region
    region_df <- loc_df[level==2,]
    region_name <- region_df$location_name
    region_id <- region_df$location_id
    df_matrix <- expand.grid(location_id = region_id, age_group_id = df_matrix$age_group_id)
    #Add population
    #Pull in forecasted population numbers
    df_pop <- read.csv("[DIRECTORY]/resub_pop_forecast_age_location_specific.csv")
    df_pop <- df_pop[df_pop$year_id==year_forecast & df_pop$sex_id!=3, ]
    setnames(df_pop, "population", "pop_size")
    df_pop[,c("quantile","scenario", "income_group_id", "income_group", "X")] <- NULL
    df_matrix <- merge(df_matrix, df_pop, all.x=T)

    df_matrix <- as.data.table(merge(df_matrix, ages_2019[,c("age_group_id", "age_group_name", "age_group_years_start", "age_group_years_end")],
                                     by = c("age_group_id")))
    df_matrix[age_group_years_start==95, age_group_years_end := 99]
    df_matrix[, midage := (age_group_years_end + age_group_years_start)/2]
    df_matrix[,c("age_group_years_start", "age_group_years_end")] <- NULL
    df_matrix <- merge(df_matrix, loc_df[,c("location_id", "region_name", "super_region_name")], by = "location_id")

    ## Add midage variable and region names to dt
    dt1 <- as.data.table(merge(dt1, ages_2019[,c("age_group_id", "age_group_years_start", "age_group_years_end", "age_group_name")], by = "age_group_id"))
    dt1[age_group_years_start==95, age_group_years_end := 99]
    dt1[, midage:= (age_group_years_end + age_group_years_start)/2]
    dt1[,c("age_group_years_start", "age_group_years_end")] <- NULL
    dt1 <- merge(dt1, loc_df[,c("region_name", "location_id", "super_region_name")], by = "location_id")


    ## Run regression models after logit-transforming data
    cols <- c(paste0("draw_",0:999))

    dt1 <- as.data.frame(dt1)
    dt1[cols] <- gtools::logit(dt1[cols])

    dt_f <- dt1[dt1$sex_id==2, ]
    dt_m <- dt1[dt1$sex_id==1, ]

    fits_f <- lapply(0:999, function(i) {
      #i <- 1 # dev
      df_tmp <- as.data.frame(dt_f)[, c("region_name", "year_id", "midage", paste0("draw_", i))]
      names(df_tmp)[4] <- "draw_i"
      lm(draw_i ~ year_id * region_name + bs(midage,knots = c(25,50,75)), data = df_tmp) #interaction term region and year, cubic spline on age (midage)
    })

    fits_m <- lapply(0:999, function(i) {
      #i <- 1 # dev
      df_tmp <- as.data.frame(dt_m)[, c("region_name", "year_id", "midage", paste0("draw_", i))]
      names(df_tmp)[4] <- "draw_i"
      lm(draw_i ~ year_id * region_name + bs(midage,knots = c(25,50,75)), data = df_tmp) #interaction term region and year, cubic spline on age (midage)
    })


    ## Predict out rates using fits
    df_matrix_f <- df_matrix[df_matrix$sex_id==2,]
    df_matrix_m <- df_matrix[df_matrix$sex_id==1, ]

    preds_f <- lapply(fits_f, function(x) {
      # x <- fits[[1]] # dev
      predict(x, newdata = df_matrix_f)
    })

    preds_m <- lapply(fits_m, function(x) {
      # x <- fits[[1]] # dev
      predict(x, newdata = df_matrix_m)
    })


    ## Convert back to normal space
    dat_pred_f <- do.call("cbind", preds_f)
    dat_pred_f <- gtools::inv.logit(dat_pred_f)
    dat_pred_f <- as.data.table(dat_pred_f)
    dat_pred_m <- do.call("cbind", preds_m)
    dat_pred_m <- gtools::inv.logit(dat_pred_m)
    dat_pred_m <- as.data.table(dat_pred_m)


    #Add region and age variables back to df
    dat_pred_m$region_name <- df_matrix_m$region_name
    dat_pred_m$age_group_name <- df_matrix_m$age_group_name
    dat_pred_m$age_group_id <- df_matrix_m$age_group_id
    dat_pred_m$pop_size_f <- df_matrix_m$pop_size
    for (i in 1:1000) {
      colnames(dat_pred_m)[colnames(dat_pred_m)==paste0("V", i)] <- paste0("male_", i)
    }
    region_agg_m <- as.data.frame(dat_pred_m)
    dat_pred_f$region_name <- df_matrix_f$region_name
    dat_pred_f$age_group_name <- df_matrix_f$age_group_name
    dat_pred_f$age_group_id <- df_matrix_f$age_group_id
    dat_pred_f$pop_size_m <- df_matrix_f$pop_size
    for (i in 1:1000) {
      colnames(dat_pred_f)[colnames(dat_pred_f)==paste0("V", i)] <- paste0("female_", i)
    }
    region_agg_f <- as.data.frame(dat_pred_f)


    region_agg_b <- merge(region_agg_f,region_agg_m, by = c("age_group_name", "region_name", "age_group_id"))

    moveMe <- function(data, tomove, where = "last", ba = NULL) {
      temp <- setdiff(names(data), tomove)
      x <- switch(
        where,
        first = data[c(tomove, temp)],
        last = data[c(temp, tomove)],
        before = {
          if (is.null(ba)) stop("must specify ba column")
          if (length(ba) > 1) stop("ba must be a single character string")
          data[append(temp, values = tomove, after = (match(ba, temp)-1))]
        },
        after = {
          if (is.null(ba)) stop("must specify ba column")
          if (length(ba) > 1) stop("ba must be a single character string")
          data[append(temp, values = tomove, after = (match(ba, temp)))]
        })
      x
    }
    region_agg_b <- moveMe(region_agg_b, "region_name", "before", "female_1")
    region_agg_b <- moveMe(region_agg_b, "age_group_name", "before", "female_1")
    region_agg_b <- moveMe(region_agg_b, "age_group_id", "before", "female_1")



    ## Calculate age-specific forecasted case number
    region_agg <- as.data.table(region_agg_b)
    region_agg[, paste0("male_", 0:999) := lapply(1:1000, function(x) get(paste0("male_", x)) *pop_size_m)]
    region_agg[, paste0("female_", 0:999) := lapply(1:1000, function(x) get(paste0("female_", x)) *pop_size_f)]

    ##Aggregate to get both sex
    ## Add cases together to get draws for both sex
    region_agg[, paste0("both_", 0:999) := lapply(0:999, function(x) get(paste0("male_", x)) + get(paste0("female_", x)))]
    region_agg[, pop_size_b := pop_size_m + pop_size_f]
    region_agg[,c("female_1000", "male_1000")] <- NULL

    ## Revert to rates in order to age-standardize
    snake_rates <- as.data.table(region_agg)
    snake_rates[, paste0("male_", 0:999) := lapply(0:999, function(x) get(paste0("male_", x)) /pop_size_m)]
    snake_rates[, paste0("female_", 0:999) := lapply(0:999, function(x) get(paste0("female_", x)) /pop_size_f)]
    snake_rates[, paste0("both_", 0:999) := lapply(0:999, function(x) get(paste0("both_", x)) /pop_size_b)]

    ## Age-standardized rates by region
    calc_agestd_rates <- function(data_dt){
      dt_tmp <- copy(data_dt)
      age_weights <- get_age_metadata(12, gbd_round_id=6)
      dt_tmp <- merge(dt_tmp, age_weights[, c("age_group_id", "age_group_weight_value")], by = "age_group_id", all.x=T)
      dt_tmp[, paste0("male_", 0:999) := lapply(.SD, function(x) sum(x * age_group_weight_value)), by = "region_name", .SDcols = paste0("male_", 0:999)]
      dt_tmp[, paste0("female_", 0:999) := lapply(.SD, function(x) sum(x * age_group_weight_value)), by = "region_name", .SDcols = paste0("female_", 0:999)]
      dt_tmp[, paste0("both_", 0:999) := lapply(.SD, function(x) sum(x * age_group_weight_value)), by = "region_name", .SDcols = paste0("both_", 0:999)]
      dt_tmp <- unique(dt_tmp, by = c("region_name"))
      dt_tmp[,`:=` (age_group_weight_value = NULL, age_group_id = 27)]
      dt_tmp[, age_group_name := "Age-standardized"]
      dt_tmp[, mean_male := rowMeans(.SD), .SDcols = paste0("male_", 0:999)]
      dt_tmp[, mean_female := rowMeans(.SD), .SDcols = paste0("female_", 0:999)]
      dt_tmp[, mean_both := rowMeans(.SD), .SDcols = paste0("both_", 0:999)]
      dt_tmp$upper_male <- apply(dt_tmp[,c(paste0("male_", 0:999))], 1, quantile, probs = 0.975, na.rm = TRUE)
      dt_tmp$lower_male <- apply(dt_tmp[,c(paste0("male_", 0:999))], 1, quantile, probs = 0.025, na.rm = TRUE)
      dt_tmp$upper_female <- apply(dt_tmp[,c(paste0("female_", 0:999))], 1, quantile, probs = 0.975, na.rm = TRUE)
      dt_tmp$lower_female <- apply(dt_tmp[,c(paste0("female_", 0:999))], 1, quantile, probs = 0.025, na.rm = TRUE)
      dt_tmp$upper_both <- apply(dt_tmp[,c(paste0("both_", 0:999))], 1, quantile, probs = 0.975, na.rm = TRUE)
      dt_tmp$lower_both <- apply(dt_tmp[,c(paste0("both_", 0:999))], 1, quantile, probs = 0.025, na.rm = TRUE)
      dt_tmp[, c(paste0("male_", 0:999), paste0("female_", 0:999), paste0("both_", 0:999))] <- NULL
      return(dt_tmp)
    }

    agestdrate_dt <- calc_agestd_rates(snake_rates)


    ## Age-aggregate cases from each region
    mylist <- split(region_agg, region_agg$region_name)
    newlist <- list()
    for(i in 1:21){
    #  i <- 1 # dev
      df_tmp <- as.data.frame(mylist[[i]])
      name <- as.character(df_tmp[1, 1])
      df_tmp <- as.data.table(df_tmp %>% adorn_totals("row"))
      df_tmp <- df_tmp[region_name=="Total",]
      df_tmp$region_name <- gsub('Total', name, df_tmp$region_name)
      newlist[[i]] <- df_tmp
    }

    region_collapse <- do.call(rbind,newlist)
    region_collapse <- region_collapse[region_collapse$age_group_id!=0,]


    ## Add in row for global total and collapse cases to regional and global mean and UIs
    region_collapse <- as.data.table(region_collapse %>% adorn_totals("row"))
    region_collapse$region_name <- gsub("Total", "Global", region_collapse$region_name)
    region_collapse$age_group_name <- "All age"
    region_collapse$age_group_id <- 22
    region_collapse[, mean_male := rowMeans(.SD), .SDcols = paste0("male_", 0:999)]
    region_collapse$upper_male <- apply(region_collapse[,c(paste0("male_", 0:999))], 1, quantile, probs = 0.975, na.rm = TRUE)
    region_collapse$lower_male <- apply(region_collapse[,c(paste0("male_", 0:999))], 1, quantile, probs = 0.025, na.rm = TRUE)
    region_collapse[, mean_female := rowMeans(.SD), .SDcols = paste0("female_", 0:999)]
    region_collapse$upper_female <- apply(region_collapse[,c(paste0("female_", 0:999))], 1, quantile, probs = 0.975, na.rm = TRUE)
    region_collapse$lower_female <- apply(region_collapse[,c(paste0("female_", 0:999))], 1, quantile, probs = 0.025, na.rm = TRUE)
    region_collapse[, mean_both := rowMeans(.SD), .SDcols = paste0("both_", 0:999)]
    region_collapse$upper_both <- apply(region_collapse[,c(paste0("both_", 0:999))], 1, quantile, probs = 0.975, na.rm = TRUE)
    region_collapse$lower_both <- apply(region_collapse[,c(paste0("both_", 0:999))], 1, quantile, probs = 0.025, na.rm = TRUE)
    region_collapse[, c(paste0("male_", 0:999), paste0("female_", 0:999), paste0("both_", 0:999))] <- NULL


    ## Combine outputs for all age cases, age-standardized rates, and age-specific rates
    #variables: region_collapse, rates_dt_wide, agestdrate_dt
    final_dt <- plyr::rbind.fill(region_collapse, agestdrate_dt)
    final_dt[,c("age_group_id","standard_error")] <- NULL
    #Finalize dataset
    #final_dt$sex <- sex
    final_dt$year_id <- year_forecast


    ## Save output
    output_path_reg <- paste0(my_dir, "output/")
    write.xlsx(final_dt, paste0(output_path_reg, "_", year_forecast, "_snake_forecast.xlsx"))
}



## Final combined data
df1 <- read.xlsx(paste0(my_dir, "output/2020_snake_forecast.xlsx"))
df2 <- read.xlsx(paste0(my_dir, "output/2030_snake_forecast.xlsx"))
df3 <- read.xlsx(paste0(my_dir, "output/2040_snake_forecast.xlsx"))
df4 <- read.xlsx(paste0(my_dir, "output/2050_snake_forecast.xlsx"))
merge_years <- plyr::rbind.fill(df1,df2,df3,df4)

#Make long again (sort of awkward code)
merge_years_f <- merge_years[,c("region_name", "age_group_name", "year_id", "mean_female", "lower_female", "upper_female")]
merge_years_m <- merge_years[,c("region_name", "age_group_name", "year_id", "mean_male", "lower_male", "upper_male")]
merge_years_b <- merge_years[,c("region_name", "age_group_name", "year_id", "mean_both", "lower_both", "upper_both")]
merge_years_f$sex <- "Female"
merge_years_m$sex <- "Male"
merge_years_b$sex <- "Both"
setnames(merge_years_f, c("mean_female", "lower_female", "upper_female"), c("mean", "lower", "upper"))
setnames(merge_years_m, c("mean_male", "lower_male", "upper_male"), c("mean", "lower", "upper"))
setnames(merge_years_b, c("mean_both", "lower_both", "upper_both"), c("mean", "lower", "upper"))
merge_years <- plyr::rbind.fill(merge_years_f, merge_years_m, merge_years_b)

write.xlsx(merge_years, paste0("[DIRECTORY]/snake_forecast_2020_2030_2040_2050.xlsx"))


## Visualizations---------------------------------------------------------------------------------------------------------------
library(ggsci, lib.loc = '[DIRECTORY]/packages')

## Set up dfs
df <- as.data.table(read.xlsx(paste0(my_dir, "output/snake_forecast_2020_2030_2040_2050.xlsx")))
global_cases <- df[df$region_name == "Global" & df$sex!="Both" & df$age_group_name=="All age", ]

df <- merge(df, region_df[,c("region_name", "super_region_name")], by= "region_name")

region_cases <- df[df$region_name != "Global" & df$age_group_name=="All age" & df$sex=="Both", ]
region_agestd <- df[df$region_name != "Global" & df$age_group_name=="Age-standardized" & df$sex =="Both", ]
write_csv(region_agestd, '[DIRECTORY]/age_std_region_forecast.csv')

## Set themes
mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face="bold", size = (11), colour = "black"),
                      legend.title = element_blank(),
                      legend.text = element_text(face = "italic", colour="black",family = "Helvetica" , size=7),
                      axis.title = element_text(family = "Helvetica", size = (10), colour = "black"),
                      axis.text = element_text(family = "Helvetica", colour = "black", size = (10)))

global_cases$sex <- as.factor(global_cases$sex)
global_cases$sex <- factor(global_cases$sex, levels=c("Male", "Female"))

## Global case number
gg_global_cases <- ggplot(data=global_cases, aes(x=year_id, y=mean/1000, group=sex, color=sex, fill=sex)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin=global_cases$lower/1000, ymax=global_cases$upper/1000), linetype=0, alpha=0.15) +
  theme_classic() +
  theme(text = element_text(size = 11, color = "black"), legend.position = c(.06,.92)) +
  scale_x_continuous(breaks = seq(1990, 2050, by = 10)) +
  ggtitle("Forecast to 2050 of global cases of envenomation by sex")  +
  labs(x="Year", y= "Number of cases (thousands)") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_color_lancet(name = "", labels = c("Male", "Female")) +
  scale_fill_lancet(guide = FALSE) +
  #ylim(0,550) +
  mynamestheme
gg_global_cases


## Regional case number
gg_region_cases <- ggplot(data=region_cases, aes(x=year_id, y=mean/1000, group=region_name, color=region_name)) +
  facet_wrap(.~region_cases$super_region_name, scales = "free") +
  geom_line() +
  geom_point() +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(color=guide_legend(ncol=7)) +
  ggtitle("Forecast to 2050 of regional cases of snake bites, both sexes") +
  labs(x="Year", y= "Number of cases (thousands)") +
  scale_x_continuous(breaks = seq(1990, 2050, by = 10)) +
  scale_color_manual(name = "", values = colorRampPalette(solarized_pal()(8))(21)) +
  mynamestheme
gg_region_cases


## Age-standardized rates by region
gg_prev_age_std <- ggplot(data=region_agestd, aes(x=year_id, y=mean, group=region_name, color=region_name, fill=region_name)) +
  #facet_wrap(.~region_agestd$super_region_name, scales = "free") +
  geom_line() +
  geom_point() +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(color=guide_legend(ncol=7)) +
  ggtitle("Forecast to 2050 of envenomation, age-standardized mortality, both sexes") +
  labs(x="Year", y= "Age-standardized mortality per 100,000") +
  #ylim(0, 0.00001) +
  scale_x_continuous(breaks = seq(1990, 2050, by = 10)) +
  mynamestheme
gg_prev_age_std



## Save plots
pdf(paste0(my_dir, "output/snake_forecast_plots.pdf"), width = 12)
gg_global_cases
gg_region_cases
gg_prev_age_std
dev.off()

## Look compared to 1990-2019

source('[DIRECTORY]/get_population.R')
dt <- fread('[DIRECTORY]/region_snake_draws.csv')
dt_long <- melt(dt, id.vars = c('age_group_id', 'sex_id', 'year_id', 'location_id'),
                value.var = patterns('draw_'))
dt_long <- merge(dt_long, ages_2019[, c('age_group_id', 'age_group_weight_value')], by = 'age_group_id')
dt_long[, age_st := weighted.mean(value, w = age_group_weight_value), by = c('location_id', 'year_id', 'sex_id', 'variable')]
dt_long[, mean_agest := mean(age_st), by = c('location_id', 'year_id', 'sex_id')]
dt_long <- merge(dt_long, loc_df[, c('location_id', 'location_name')], by = 'location_id')
dt_long[, region_name := location_name]

# Both sexes too
pop_df <- get_population(age_group_id = unique(dt_long$age_group_id),
                         location_id = unique(dt_long$location_id),
                         sex_id = c(1,2),
                         year_id = unique(dt_long$year_id),
                         gbd_round_id = 6, decomp_step = 'step4')

dt_long <- merge(dt_long, pop_df, by = c('location_id', 'age_group_id', 'year_id', 'sex_id'))
dt_long[, both_sex_rate := weighted.mean(value, w = population), by = c('location_id', 'age_group_id', 'year_id', 'variable')]

both_sex <- unique(dt_long[, c('location_id', 'region_name', 'age_group_id', 'year_id', 'variable', 'both_sex_rate', 'age_group_weight_value')])
both_sex[, both_sex_age_st := weighted.mean(both_sex_rate, w = age_group_weight_value), by = c('year_id', 'variable', 'location_id')]
both_sex[, mean_agest := mean(both_sex_age_st), by = c('year_id', 'location_id')]
both_sex[, lo_agest := quantile(both_sex_age_st, 0.025), by = c('year_id', 'location_id')]
both_sex[, hi_agest := quantile(both_sex_age_st, 0.975), by = c('year_id', 'location_id')]


region_ast <- unique(both_sex[, c('location_id', 'region_name', 'year_id', 'mean_agest', 'lo_agest', 'hi_agest')])
ggplot(data=region_agestd[region_name == 'Western Sub-Saharan Africa']) +
  #facet_wrap(.~region_agestd$super_region_name, scales = "free") +
  geom_line(aes(x=year_id, y=mean, group=region_name, color=region_name, fill=region_name)) +
  geom_point(aes(x=year_id, y=mean, group=region_name, color=region_name, fill=region_name)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(color=guide_legend(ncol=7)) +
  ggtitle("Forecast to 2050 of envenomation, age-standardized mortality, both sexes") +
  labs(x="Year", y= "Age-standardized mortality per 100,000") +
  #ylim(0, 0.00001) +
  scale_x_continuous(breaks = seq(1990, 2050, by = 10)) +
  geom_line(data = region_ast[region_name == 'Western Sub-Saharan Africa'],
            aes(y = mean_agest, x = year_id, color = region_name)) +
  geom_ribbon(data = region_ast[region_name == 'Western Sub-Saharan Africa'],
              aes(ymin = lo_agest, ymax = hi_agest, fill = region_name, x = year_id), alpha = 0.1) +
  mynamestheme
