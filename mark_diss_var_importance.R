library(tidyverse)

################################################################################
# NOTE: REQUIRES CSV FILES OF MODELLING RESULTS TO BE IN WORKING DIRECTORY
################################################################################

model_names <- c(
  'random_lag20_predictors',
  'random_lag20_response',
  'random_lag5_predictors',
  'random_lag5_response',
  'random_no_lag',
  'spatial_buff_lag20_predictors',
  'spatial_buff_lag20_response',
  'spatial_buff_lag5_predictors',
  'spatial_buff_lag5_response',
  'spatial_lag20_predictors',
  'spatial_lag20_response',
  'spatial_lag5_predictors',
  'spatial_lag5_response',
  'spatial_no_lag'
)


var_import_func <- function(model_name){
  model_df <- read.csv(paste(model_name,"_var_importance.csv", sep = ""))
  colnames(model_df) <- 
    c('variable', 'fold1', 'fold2', 'fold3', 'fold4', 'fold5')
  df <- model_df[-1]
  df$mean_VI <- rowMeans(df)
  df$variable <- model_df$variable
  df <- df |>
    select(variable, mean_VI) |>
    mutate(pct_VI = round(mean_VI/max(mean_VI),2)) |>
    arrange(desc(pct_VI)) |>
    select(-mean_VI)
  return(df)
}

random_lag20_predictors_VI <- var_import_func('random_lag20_predictors')
random_lag20_response_VI <- var_import_func('random_lag20_response')
random_lag5_predictors_VI <- var_import_func('random_lag5_predictors')
random_lag5_response_VI <- var_import_func('random_lag5_response')
random_no_lag_VI <- var_import_func('random_no_lag')
spatial_buff_lag20_predictors_VI <- var_import_func('spatial_buff_lag20_predictors')
spatial_buff_lag20_response_VI <- var_import_func('spatial_buff_lag20_response')
spatial_buff_lag5_predictors_VI <- var_import_func('spatial_buff_lag5_predictors')
spatial_buff_lag5_response_VI <- var_import_func('spatial_buff_lag5_response')
spatial_lag20_predictors_VI <- var_import_func('spatial_lag20_predictors')
spatial_lag20_response_VI <- var_import_func('spatial_lag20_response')
spatial_lag5_predictors_VI <- var_import_func('spatial_lag5_predictors')
spatial_lag5_response_VI <- var_import_func('spatial_lag5_response')
spatial_no_lag_VI <- var_import_func('spatial_no_lag')



