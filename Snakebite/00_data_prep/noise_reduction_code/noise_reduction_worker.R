## COD Noise reduction worker script

f <- commandArgs()[5]

library(readr)
library(stats)
library(MASS)
library(data.table)
library(parallel)
library(stringr)
library(magrittr)
library(lme4)
library(ggplot2)
library(gridExtra)
library(merTools, lib = '/home/j/temp/nrober75/packages')

source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_location_metadata.R')

ages <- fread('[DIRECTORY]/age_group_ids_2019-11-21_02-42-PM.csv')
ages[, age_group_id := as.character(age_group_id)]

plot_NR <- function(df, model_group, v, s, subnat = F){
  df <- merge(df, ages[, c('age_group_years_start', 'age_group_id')], by = 'age_group_id', all.x = TRUE)
  
  # Format in kind of hacky way
  plotdf <- copy(df)
  plotdf[, predicted := 'Predicted']
  plotdf[, deaths := pred_deaths][, cf := mean]
  df[, predicted := 'Raw']
  plotdf1 <- rbind(df, plotdf)
  
  date <- Sys.Date()
  dir <- paste0('[DIRECTORY]/', model_group, '_')
  dir.create(dir)
  pdf(paste0(dir, v, '_', s, '_', date, '.pdf'), width = 13, height = 10)
  raw <- ggplot(plotdf1[predicted == 'Raw'], aes(x = year_id, y = cf)) +
    geom_point(color = 'red') +
    facet_wrap(~age_group_years_start) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste0('Raw cause fraction - ', model_group, ' (all subnationals) due to ', '\n',  v, ' and sex_id ', s)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  pred <- ggplot(plotdf1[predicted == 'Predicted'], aes(x = year_id, y = cf)) +
    geom_point(color = 'blue') +
    facet_wrap(~age_group_years_start) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste0('Noise reducted cause fraction - ', model_group, 
                   ' (all subnationals) due to ', '\n', v, ' and sex_id ', s)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  grid.arrange(raw, pred, ncol = 2)
  dev.off()
  
  if(subnat){
    dir <- paste0('[DIRECTORY]/', model_group, '/')
    dir.create(dir)
    
    for(loc in unique(df$ihme_loc_id)){
      print(loc)
      pdf(paste0(dir, loc, '_', date, '.pdf'), width = 12, height = 10)
      raw <- ggplot(plotdf1[ihme_loc_id == loc & predicted == 'Raw'], aes(x = year_id, y = cf)) +
        geom_point() +
        facet_wrap(~age_group_years_start) + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle(paste0('Raw cause fraction - ', loc, '\n', ' due to ', v, ' and sex_id ', s)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      
      pred <- ggplot(plotdf1[ihme_loc_id == loc & predicted == 'Predicted'], aes(x = year_id, y = cf)) +
        geom_point() +
        facet_wrap(~age_group_years_start) + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle(paste0('Noise reducted cause fraction - ', model_group, '\n',
                       ' (all subnationals) due to ', v, ' and sex_id ', s)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      grid.arrange(raw, pred, ncol = 2)
      dev.off()
      
    }
    
  }
  
}

## Get predicted var function for if std_err is zero

get_predicted_var <- function(df){
  prior_deaths_col <- 'pred_deaths'
  prior_se_col <- 'std_err_deaths'
  
  df[, predicted_cf := pred_deaths/sample_size]
  # df[, predicted_std_err := exp(std_err_deaths-1)*predicted_cf]
  # df[, predicted_var := predicted_std_err^2]
  
  df[, predicted_std_err := 0]
  df[, predicted_var := ((1/sample_size)*(predicted_cf)*(1-predicted_cf))]
  
  # Set up some stuff
  df[predicted_var == Inf, predicted_var := 10^91*predicted_cf]
  return(df)
}


## Some functions from COD
make_factors <- function(data) {
  data$year_id <- as.factor(data$year_id)
  data$age_group_id <- as.factor(data$age_group_id)
  data$country_id <- as.factor(data$iso3)
  data$subnat_id <- as.factor(data$ihme_loc_id)
  return(data)
}

run_model <- function(model_data, cause_id, sex, loc_agg, predict_col) {
  # Setting max iterations of the fisher scoring technique to 100
  # 100 is what we used in our STATA code
  # Turns out because of a bug in STATA the negative binomial never ran
  # Old code runs a negative binomial if our poisson converged.
  # Previous comments apply as the process is the same though different model.
  # The negative binomial model loosens the poisson assumption of the mean being
  # equal to the variance in the underlying data. If you have overdispered data,
  # i.e. the data shows variance greater than the mean, negative binomial regression
  # produces similar results with the standard errors likely to be less biased than 
  # the poisson on the same underlying data.
  
  formula <- determine_formula(model_data, predict_col)
  print(formula)
  glm.control(maxit=100)
  # Specify the model parameters
  pois_model <- glm(formula = formula,
                    family = poisson(),
                    data=model_data)
  
  if (SAVE) {
    print("Saving model object")
    save(pois_model, file=paste0(model_object_dir, "/model_", loc_agg, "_", sex, "_", cause_id, "_", launch_set_id, ".rda"))
  }
  pois_converged <- pois_model$converged
  
  # We are able to check if the maximum likelihood estimation converged 
  print(paste0("Poisson converged? ", pois_converged))
  if (!pois_converged) {
    return(fill_with_average(model_data, predict_col))
  } else {
    pois_results <- predict.glm(pois_model, new_data=model_data, se.fit=TRUE)
    # We exponentiate the results as the deaths come out in the log space
    fit_col <- exp(pois_results$fit)
    se_col <- pois_results$se.fit
    model_result <- data.table(fit_col, se_col)
    
    predict_col_name <- paste0('pred_', predict_col)
    se_col_name <- paste0('std_err_', predict_col)
    names(model_result) <- c(predict_col_name, se_col_name)
    
    # This produces the fitted values, including standard error for each
    # observation. The standard error is for the mean result given each row's
    # specific set of independent variables, i.e. sets of {location, age, year}
    return(list(model_result = model_result, converged = T))
  }
}

determine_formula <- function(a_df, predict_col) {
  # Return the formula to use based on whether the factor variables
  # have only one value
  
  num_years = length(unique(a_df$year_id))
  num_ages = length(unique(a_df$age_group_id))
  num_countries = length(unique(a_df$iso3))
  num_subnats = length(unique(a_df$ihme_loc_id))
  
  formula <- paste0(predict_col, " ~ offset(log(sample_size))")
  if (num_years > 1) {
    formula <- paste0(formula, " + year_id")
  }
  if (num_ages > 1) {
    formula <- paste0(formula, " + age_group_id")
  }
  if (num_countries > 1) {
    formula <- paste0(formula, " + iso3")
  }
  if (num_subnats > 1) {
    formula <- paste0(formula, " + ihme_loc_id")
  }
  print(formula)
  return(formula)
}

fill_with_average <- function(df, predict_col) {
  
  df$index_value <- as.integer(row.names(df))
  df$pred_col_cf <- df[, predict_col, with=FALSE] / df$sample_size
  aggregated <- aggregate(pred_col_cf ~ age_group_id, df, mean)
  names(aggregated)[names(aggregated) == 'pred_col_cf'] <- 'mean_pred_col_cf'
  df <- merge(df, aggregated, by='age_group_id')
  
  df$pred_col_deaths <- df$mean_pred_col_cf * df$sample_size
  df$mean_pred_col_cf <- NULL
  df$std_err <- rep(0, nrow(df))
  predict_col_name <- paste0('pred_', predict_col)
  se_col_name <- paste0('std_err_', predict_col)
  
  # set index back to what it was originally, before merge changed it
  stopifnot(setequal(df$index_value, row.names(df)))
  df <- df[order(df$index_value), ]
  
  df <- df[, pred_col_deaths, std_err]
  names(df) <- c(se_col_name, predict_col_name)
  
  return(list(model_result = df, converged = F))
}


## Run model

print(f)
model_group <- str_split(f, '/')[[1]][7]
model_group <- str_split(model_group, '[.]')[[1]][1]
print(model_group)
full_df <- fread(f)
stopifnot(nrow(full_df) == nrow(unique(full_df))) # Check if I have duplicates


for(v in unique(full_df$venom)){
  print(v)
  for(s in unique(full_df$sex_id)){
    print(s)
    
    # Clean up stuff
    rm(pois_model)
    rm(pois_converged)
    
    # Subset to the model data frame
    model_df <- full_df[sex_id == s & venom == v]
    
    # Standard error using wilson's method for later for VA
    model_df[, cf := deaths/sample_size]
    model_df[, std_error := sqrt(1/sample_size*cf*(1-cf) + 1/(4*sample_size^2)*1.96^2)]
    
    if(nrow(model_df) == 0){
      print(paste0("No data in ", v, ' ', s))
      next
    }
    

    # Check if just zero
    if(length(unique(model_df$deaths)) == 1 & unique(model_df$deaths)[1] == 0){

      model_df[, mean := 0]
      model_df[, variance := 0]
      write_csv(model_df, 
                paste0('[DIRECTORY]/', model_group, '_', v, '_', s, '.csv'))
      next
    }
    
    
    if(f %like% 'VR' | length(unique(model_df$deaths)) == 1){
      
      print('VR study or only one value...')
      model_df <- make_factors(model_df)
      predict_col <- 'deaths'
      
      if(model_group == 'VR-R10' & v == 'snake' & s == 2){
        formula <- 'deaths ~ offset(log(sample_size)) + year_id + age_group_id + ihme_loc_id'
      } else{
        formula <- determine_formula(model_df, predict_col)
      }
      glm.control(maxit=100) # from COD code
      
      
      model_result <- tryCatch({
        pois_model <- glm(formula = formula, family = poisson(),
                          data = model_df)
        
      }, error = function(err){
        print(paste0('Model broke ', substr(err, 1, 30)))
        print('Filling with average like in the COD set when model doesnt converge')
        pois_converged <-FALSE
        
        # model_result <- fill_with_average(model_df, predict_col)
        # model_result <- model_result$model_result
        
      })
      
      # if model runs, assign it to pois_model. This catches and proceeds as normal if the model works, but will fill_with_average if model breaks
      if('glm' %in% class(model_result)){
        pois_model <- model_result
        pois_converged <- pois_model$converged %>% print()
      } else{
        pois_converged <- FALSE
      }
      
      if (!pois_converged) {
        model_result <- fill_with_average(model_df, predict_col)
        model_result <- model_result$model_result
      } else {
        print('Making results')
        pois_results <- predict.glm(pois_model, new_data=model_df, se.fit=TRUE)
        # We exponentiate the results as the deaths come out in the log space
        fit_col <- exp(pois_results$fit)
        se_col <- pois_results$se.fit
        model_result <- data.table(fit_col, se_col)
        
        predict_col_name <- paste0('pred_', predict_col)
        se_col_name <- paste0('std_err_', predict_col)
        names(model_result) <- c(predict_col_name, se_col_name)
        
        # This produces the fitted values, including standard error for each
        # observation. The standard error is for the mean result given each row's
        # specific set of independent variables, i.e. sets of {location, age, year}
      }
      
      print('Bayesian averaging')
      final_model <- cbind(model_result, model_df)
      
      ## Bayesian averaging
      # This is from get_predicted_var function in noise_reduction.py with default_var = False
      # Now next steps
      prior_deaths_col <- 'pred_deaths'
      prior_se_col <- 'std_err_deaths'
      cf_col = 'cf'
      pre_nr_col = 'cf_pre_nr'
      
      
      non_zero_std_err = final_model[get(prior_se_col) != 0]
      
      # Change zero's through get_predicted_var to get some non-zero variance points
      zero_std_err <- final_model[get(prior_se_col) == 0]
      zero_std_err <- get_predicted_var(zero_std_err)
      
      non_zero_std_err <- rbind(non_zero_std_err, zero_std_err[predicted_var != 0], fill = TRUE)
      zero_std_err <- zero_std_err[predicted_var == 0]
      
      # Check we aren't losing rows
      stopifnot(nrow(non_zero_std_err) + nrow(zero_std_err) == nrow(final_model))
      
      non_zero_std_err[, predicted_cf := pred_deaths/sample_size]
      non_zero_std_err[, predicted_std_err := exp(std_err_deaths - 1)*predicted_cf]
      non_zero_std_err[, predicted_var := predicted_std_err^2]
      non_zero_std_err[is.na(predicted_var) | predicted_var == Inf, predicted_var := 10^91*predicted_cf]
      
      df <- rbind(non_zero_std_err, zero_std_err)
      cf_component <- df$cf * (1-df$cf)
      
      df[, std_err_data := sqrt(
        (cf_component/sample_size) + 
          ((1.96^2)/4*sample_size^2))]
      
      df[, variance_data := std_err_data^2]
      df[, mean_cf_data_component := cf * (predicted_var/(predicted_var + variance_data))]
      df[, mean_cf_prediction_component := predicted_cf*(variance_data/(predicted_var + variance_data))]
      df[, mean := mean_cf_data_component + mean_cf_prediction_component]
      df[, variance_numerator := predicted_var*variance_data]
      df[, variance_denominator := predicted_var + variance_data]
      df[, variance := variance_numerator/variance_denominator]
      
      print('Writing')
      write_csv(df, paste0('[DIRECTORY]/', model_group, '_', v, '_', s, '.csv'))
      plot_NR(df, model_group, v, s, F)
      
      
    } else if(f %like% 'VA'){
      print('VA')
      # VA uses a mixed-effects poisson model
      model_df <- make_factors(model_df)
      predict_col <- 'deaths'
      model_df[, site_id := paste0(nid, '_', location_id)]
      
      # pois_model <- glmer(deaths ~ factor(age_group_id) + (1|site_id), 
      #                    offset = log(sample_size), data = model_df, family = poisson)
      
      # modeling
      print('Making big VA model')
      
      model_result <- tryCatch({
        pois_model <- glmer(deaths ~ factor(age_group_id) + (1|site_id) + 
                              offset(log(sample_size)),
                            data = model_df, family = poisson)
        
        pois_results <- predictInterval(pois_model, model_df)
        fit_col <- exp(pois_results$fit) # Turn deaths into real space
        
        # Upper and lower
        se_upper <- pois_results$upr
        se_lower <- pois_results$lwr
        se_col <- ((se_upper/se_lower)/2/1.96) 
        
        # Model prediction and SE in real space
        model_result <- data.table(fit_col, se_col)
        
        predict_col_name <- paste0('pred_', predict_col)
        se_col_name <- paste0('std_err_', predict_col)
        names(model_result) <- c(predict_col_name, se_col_name)
        
        final_model <- cbind(model_result, model_df)
        
        
        final_model[pred_deaths == Inf | pred_deaths == -Inf, pred_deaths := 0] 

        print('Bayesian averaging')
        # Bayesian averaging for VA. First get cause fraction from deaths in normal space
        final_model[, cf_pred := pred_deaths/sample_size]
        
        # Standard error using delta transform
        final_model[, pred_deaths_log := log(pred_deaths)]
        #Following delta transform: sd_lin = exp(point_estimate_in_log_space)*sd_log
        final_model[, se_pred := pred_deaths*std_err_deaths] # std_error_deaths is the standard deviation in log space. 
        
        final_model[, cf_post := ((se_pred^2/(se_pred^2 + std_error^2))*cf) + 
                      ((std_error^2/(std_error^2 + se_pred^2))*cf_pred)]
        
        final_model[, var_post := (se_pred^2*std_error^2)/(se_pred^2 + std_error^2)]  
        # Line up with VR:
        final_model[, mean := cf_post]
        final_model[, variance := var_post]
        
        print('Done Bayesian averaging')
        
        print('Writing')
        write_csv(final_model, paste0('[DIRECTORY]/', model_group, '_', v, '_', s, '.csv'))
        plot_NR(final_model, model_group, v, s)
        
      }, error = function(err){
        print(paste0('Model broke ', substr(err, 1, 30)))

        model_result <- fill_with_average(model_df, predict_col)
        model_result <- model_result$model_result
        
        final_model <- cbind(model_result, model_df)
        
        ## Bayesian averaging
        # This is from get_predicted_var function in noise_reduction.py with default_var = False
        # Now next steps
        prior_deaths_col <- 'pred_deaths'
        prior_se_col <- 'std_err_deaths'
        cf_col = 'cf'
        pre_nr_col = 'cf_pre_nr'
        
        
        non_zero_std_err = final_model[std_err_deaths != 0]
        
        # Change zero's through get_predicted_var to get some non-zero variance points
        zero_std_err <- final_model[std_err_deaths == 0]
        zero_std_err <- get_predicted_var(zero_std_err)
        
        non_zero_std_err <- rbind(non_zero_std_err, zero_std_err[predicted_var != 0], fill = TRUE)
        zero_std_err <- zero_std_err[predicted_var == 0]
        
        # Check we aren't losing rows
        stopifnot(nrow(non_zero_std_err) + nrow(zero_std_err) == nrow(final_model))
        
        non_zero_std_err[, predicted_cf := pred_deaths/sample_size]
        non_zero_std_err[, predicted_std_err := exp(std_err_deaths - 1)*predicted_cf]
        non_zero_std_err[, predicted_var := predicted_std_err^2]
        non_zero_std_err[is.na(predicted_var) | predicted_var == Inf, predicted_var := 10^91*predicted_cf]
        
        df <- rbind(non_zero_std_err, zero_std_err)
        cf_component <- df$cf * (1-df$cf)
        
        df[, std_err_data := sqrt(
          (cf_component/sample_size) + 
            ((1.96^2)/4*sample_size^2))]
        
        df[, variance_data := std_err_data^2]
        df[, mean_cf_data_component := cf * (predicted_var/(predicted_var + variance_data))]
        df[, mean_cf_prediction_component := predicted_cf*(variance_data/(predicted_var + variance_data))]
        df[, mean := mean_cf_data_component + mean_cf_prediction_component]
        df[, variance_numerator := predicted_var*variance_data]
        df[, variance_denominator := predicted_var + variance_data]
        df[, variance := variance_numerator/variance_denominator]
        
        print('Done Bayesian averaging')
        
        print('Writing')
        write_csv(df, paste0('[DIRECTORY]/', model_group, '_', v, '_', s, '.csv'))
        plot_NR(df, model_group, v, s, F)
        
      })
    }
  }
}





