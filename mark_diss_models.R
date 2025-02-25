library(tidyverse)
library(sf)
library(spatialsample)
library(tidymodels)

# access CV functions
source("mark_diss_functions.R")

# Importing data, this is the data from EDA
housing <- read.csv('https://raw.githubusercontent.com/MarkMData/Dissertation-data/main/cali_housing_lag_vars.csv')

################################################################################
# NOTE: THE FOLLOWING LINE OF CODE TAKES A SMALL SAMPLE OF THE DATA SET OTHERWISE
# THE SCRIPT WILL TAKE SEVERAL DAYS TO RUN
################################################################################

# Sample 10 random rows from the dataframe using dplyr
housing <- sample_n(housing, 200)


# Running the following code will generate a number of csv files of results that can
# be used by the mark_diss_rmse_results and mark_diss_var_importance scripts


################################################################################
# Random CV with no lag variables
################################################################################

randomCV_no_lag_results <- randomCV_function(
  data = housing,
  model_name = 'random_no_lag',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population
)

################################################################################
# Spatial CV with no lag variables
################################################################################

spatialCV_no_lag_results <- spatial_buffCV_function(
  data = housing,
  model_name = 'spatial_no_lag',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population
)

################################################################################
# Random CV with lag5 predictors
################################################################################

randomCV_lag5_predictors_results <- randomCV_function(
  data = housing,
  model_name = 'random_lag5_predictors',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + housingMedianAge_lag5 + log_medianIncome_lag5 +
    log_averageRooms_lag5 + log_averageBedrooms_lag5 +
    log_popPerHouse_lag5 + log_population_lag5
)

################################################################################
# Spatial CV with with buffer lag5 predictors
################################################################################

spatialCV_buff_lag5_predictors_results <- spatial_buffCV_function(
  data = housing,
  model_name = 'spatial_buff_lag5_predictors',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + housingMedianAge_lag5 + log_medianIncome_lag5 +
    log_averageRooms_lag5 + log_averageBedrooms_lag5 +
    log_popPerHouse_lag5 + log_population_lag5
)

################################################################################
# Random CV with lag20 predictors
################################################################################

randomCV_lag20_predictors_results <- randomCV_function(
  data = housing,
  model_name = 'random_lag20_predictors',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + housingMedianAge_lag20 + log_medianIncome_lag20 +
    log_averageRooms_lag20 + log_averageBedrooms_lag20 +
    log_popPerHouse_lag20 + log_population_lag20
)

################################################################################
# Spatial CV with buffer lag20 _predictors
################################################################################

spatialCV_buff_lag20_predictors_results <- spatial_buffCV_function(
  data = housing,
  model_name = 'spatial_buff_lag20_predictors',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + housingMedianAge_lag20 + log_medianIncome_lag20 +
    log_averageRooms_lag20 + log_averageBedrooms_lag20 +
    log_popPerHouse_lag20 + log_population_lag20
)



################################################################################
# Spatial no inner buffer CV with lag5 predictors
################################################################################

spatialCV_lag5_predictors_results <- spatial_no_bufferCV_function(
  data = housing,
  model_name = 'spatial_lag5_predictors',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + housingMedianAge_lag5 + log_medianIncome_lag5 +
    log_averageRooms_lag5 + log_averageBedrooms_lag5 +
    log_popPerHouse_lag5 + log_population_lag5
)


################################################################################
# Spatial no inner buffer CV with lag20 predictors
################################################################################

spatialCV_lag20_predictors_results <- spatial_no_bufferCV_function(
  data = housing,
  model_name = 'spatial_lag20_predictors',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + housingMedianAge_lag20 + log_medianIncome_lag20 +
    log_averageRooms_lag20 + log_averageBedrooms_lag20 +
    log_popPerHouse_lag20 + log_population_lag20
)

################################################################################
# Random CV with lag5 response
################################################################################

randomCV_lag5_response_results <- randomCV_function(
  data = housing,
  model_name = 'random_lag5_response',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + log_medianHouseValue_lag5
)

################################################################################
# Spatial CV with buffer lag5 response
################################################################################

spatialCV_buff_lag5_response_results <- spatial_buffCV_function(
  data = housing,
  model_name = 'spatial_buff_lag5_response',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + log_medianHouseValue_lag5
)

################################################################################
# Random CV with lag20 response
################################################################################

randomCV_lag20_response_results <- randomCV_function(
  data = housing,
  model_name = 'random_lag20_response',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + log_medianHouseValue_lag20
)

################################################################################
# Spatial CV with buffer lag20 response
################################################################################

spatialCV_buff_lag20_response_results <- spatial_buffCV_function(
  data = housing,
  model_name = 'spatial_buff_lag20_response',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + log_medianHouseValue_lag20
)


################################################################################
# Spatial no inner buffer CV with lag5 response
################################################################################

spatialCV_lag5_response_results <- spatial_no_bufferCV_function(
  data = housing,
  model_name = 'spatial_lag5_response',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + log_medianHouseValue_lag5
)


################################################################################
# Spatial no inner buffer CV with lag20 response
################################################################################

spatialCV_lag20_response_results <- spatial_no_bufferCV_function(
  data = housing,
  model_name = 'spatial_lag20_response',
  model_formula = log_medianHouseValue ~ housingMedianAge + log_medianIncome +
    log_averageRooms + log_averageBedrooms + log_popPerHouse +
    log_population + log_medianHouseValue_lag20
)













