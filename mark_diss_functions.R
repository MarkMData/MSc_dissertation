library(tidyverse)
library(sf)
library(spatialsample)
library(tidymodels)

################################################################################
# Define random CV function
################################################################################
randomCV_function <- function(data, model_name, model_formula){
  # raise error id model name is not a string
  if (!is.character(model_name)) {
    stop("Error: model_name must be a string.")
  }
  
  # create sf object
  housing_sf <- st_as_sf(
    data, coords = c('x', 'y'), remove = FALSE, crs = 3857)
  
  # Creating clusters for outer loop using kmeans
  coords <- data[,c('x', 'y')]
  set.seed(123)
  knn5 <- kmeans(coords, centers = 5, iter.max = 100, nstart = 10)
  
  # Adding clusters to housing data
  housing_sf$cluster1 <- as.factor(knn5$cluster)
  
  # Using spatialsample to add buffer between folds
  spatialCVouter <- spatial_leave_location_out_cv(
    data = housing_sf,
    group = cluster1,
    radius = NULL,
    buffer = 100000)
  
  
  # lists to store important bits
  train_data <- c()
  test_data <- c()
  variable_importance <- c()
  tune_info <- c()
  fold_test_n <- c()
  fold_train_n <- c()
  
  for (i in 1:5){
    # Using outer clusters to create train/test sets
    train <- housing_sf[spatialCVouter$splits[[i]]$in_id,]
    test <- housing_sf[spatialCVouter$splits[[i]]$out_id,]
    
    # Random CV spec
    set.seed(123)
    randomCV <- spatial_buffer_vfold_cv(
      data = train,
      radius = NULL,
      buffer = NULL,
      v=5)
    
    # create RF model spec with parameters for tuning
    rf_spec <- rand_forest(trees = 1000,
                           mtry = tune()) %>%
      set_engine("randomForest") %>%
      set_mode("regression")
    
    # create tuning grid 
    rf_param <- extract_parameter_set_dials(rf_spec) |>
      update(
        mtry = mtry(c(1, 6))
      )
    
    # Create recipe
    rec <- recipe(model_formula, data = train) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) 
    
    # Create workflow
    RF_workflow <- workflow() %>%
      add_recipe(recipe = rec) %>%
      add_model(rf_spec)
    
    # create control grid
    control_g <- control_grid(
      save_pred = TRUE,
      save_workflow = TRUE,
      extract = extract_fit_engine
    )
    
    # Running models
    set.seed(123)
    rf_tune <- 
      RF_workflow |>
      tune_grid(
        randomCV,
        grid = rf_param %>% grid_regular(levels = 6),
        metrics = metric_set(rmse),
        control = control_g
      )
    
    # save tune info
    tune_info[[i]] <- rf_tune$.metrics
    
    # save fold sizes
    test_n <- c()
    train_n <- c()
    for (j in 1:5){
      test_n[j] <- nrow(rf_tune$.predictions[[j]])/6
      train_n[j] <- length(randomCV$splits[[j]]$in_id)
    }
    fold_test_n[[i]] <- test_n
    fold_train_n[[i]] <- train_n
    
    # get best tune
    best_tune <- select_best(rf_tune, metric = 'rmse')

    # Removing tune object from memory
    rm(rf_tune)
    gc()
    
    # Fit best tune to entire training set
    set.seed(123)
    final_workflow <-
      RF_workflow |>
      finalize_workflow(best_tune)
    final_fit <- final_workflow |> fit(train)
    
    # Save variable importance
    variable_importance[[i]] <- final_fit %>%
      extract_fit_parsnip() %>%
      pluck("fit") %>%
      importance()
    
    # Predict on test set
    test_pred <- predict(final_fit, new_data = test)
    test$predicted <- test_pred$.pred
    test_data[[i]] <- test
  }
  

  ################################################################################

  # save predictions as df and csv file
  pred_df <- as.data.frame(bind_rows(test_data)) |> select(-geometry) |>
    mutate(residuals = log_medianHouseValue - predicted)
  write.csv(
    pred_df, file = paste(model_name,"_predictions.csv", sep = ""), row.names = FALSE)
  
  # save variable importance as df and csv file
  var_importance <- as.data.frame(bind_cols(variable_importance))
  rownames(var_importance) <- rownames(variable_importance[[1]])
  write.csv(var_importance, file = paste(model_name,"_var_importance.csv", sep = ""),
            row.names = TRUE)
  
  # save tune info as df and csv file
  for (i in 1:5){
    for (j in 1:5){
      tune_info[[i]][[j]]$cluster <- i
      tune_info[[i]][[j]]$fold <- j
      tune_info[[i]][[j]]$train_n <- fold_train_n[[i]][[j]]
      tune_info[[i]][[j]]$test_n <- fold_test_n[[i]][[j]]
    }
  }
  tune_info <- bind_rows(tune_info)
  write.csv(tune_info, file = paste(
    model_name,"_tune_info.csv", sep = ""), row.names = FALSE)
  
  return(list(pred_df, var_importance, tune_info))
}

################################################################################
# Define spatial CV function
################################################################################
spatial_buffCV_function <- function(data, model_name, model_formula){
  # raise error id model name is not a string
  if (!is.character(model_name)) {
    stop("Error: model_name must be a string.")
  }
  
  # create sf object
  housing_sf <- st_as_sf(
    data, coords = c('x', 'y'), remove = FALSE, crs = 3857)
  
  # Creating clusters for outer loop using kmeans
  coords <- data[,c('x', 'y')]
  set.seed(123)
  knn5 <- kmeans(coords, centers = 5, iter.max = 100, nstart = 10)
  
  # Adding clusters to housing data
  housing_sf$cluster1 <- as.factor(knn5$cluster)
  
  # Using spatialsample to add buffer between folds
  spatialCVouter <- spatial_leave_location_out_cv(
    data = housing_sf,
    group = cluster1,
    radius = NULL,
    buffer = 100000)
  
  
  # lists to store important bits
  train_data <- c()
  test_data <- c()
  variable_importance <- c()
  tune_info <- c()
  tune_train_n <- c()
  tune_test_n <- c()
  fold_test_n <- c()
  fold_train_n <- c()
  
  for (i in 1:5){
    # Using outer clusters to create train/test sets
    train <- housing_sf[spatialCVouter$splits[[i]]$in_id,]
    test <- housing_sf[spatialCVouter$splits[[i]]$out_id,]
    
    
    # Clustering training set using kmeans
    train_coords <- cbind(train$x, train$y)
    set.seed(123)
    knn_train <- kmeans(train_coords, centers = 5, iter.max = 100, nstart = 10)
    train$cluster2 <- as.factor(knn_train$cluster)
    
    # Spatial CV spec
    set.seed(123)
    spatialCV <- spatial_leave_location_out_cv(
      data = train,
      group = cluster2,
      radius = NULL,
      buffer = 25000)
    
    # create RF model spec with parameters for tuning
    rf_spec <- rand_forest(trees = 1000,
                           mtry = tune()) %>%
      set_engine("randomForest") %>%
      set_mode("regression")
    
    # create tuning grid 
    rf_param <- extract_parameter_set_dials(rf_spec) |>
      update(
        mtry = mtry(c(1, 6))
      )
    
    # Create recipe
    rec <- recipe(model_formula, data = train) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) 
    
    # Create workflow
    RF_workflow <- workflow() %>%
      add_recipe(recipe = rec) %>%
      add_model(rf_spec)
    
    # create control grid
    control_g <- control_grid(
      save_pred = TRUE,
      save_workflow = TRUE,
      extract = extract_fit_engine
    )
    
    # Running models
    set.seed(123)
    rf_tune <- 
      RF_workflow |>
      tune_grid(
        spatialCV,
        grid = rf_param %>% grid_regular(levels = 6),
        metrics = metric_set(rmse),
        control = control_g
      )
    
    # save tune info
    tune_info[[i]] <- rf_tune$.metrics
    
    # save fold sizes
    test_n <- c()
    train_n <- c()
    for (j in 1:5){
      test_n[j] <- length(spatialCV$splits[[j]]$out_id)
      train_n[j] <- length(spatialCV$splits[[j]]$in_id)
    }
    fold_test_n[[i]] <- test_n
    fold_train_n[[i]] <- train_n
    
    # get best tune
    best_tune <- select_best(rf_tune, metric = 'rmse')

    # Removing tune object from memory
    rm(rf_tune)
    gc()
    
    # Fit best tune to entire training set
    set.seed(123)
    final_workflow <-
      RF_workflow |>
      finalize_workflow(best_tune)
    final_fit <- final_workflow |> fit(train)
    
    # Save variable importance
    variable_importance[[i]] <- final_fit %>%
      extract_fit_parsnip() %>%
      pluck("fit") %>%
      importance()
    
    # Predict on test set
    test_pred <- predict(final_fit, new_data = test)
    test$predicted <- test_pred$.pred
    test_data[[i]] <- test
  }
  
  
  ################################################################################

  # save predictions as df and csv file
  pred_df <- as.data.frame(bind_rows(test_data)) |> select(-geometry) |>
    mutate(residuals = log_medianHouseValue - predicted)
  write.csv(
    pred_df, file = paste(model_name,"_predictions.csv", sep = ""), row.names = FALSE)
  
  # save variable importance as df and csv file
  var_importance <- as.data.frame(bind_cols(variable_importance))
  rownames(var_importance) <- rownames(variable_importance[[1]])
  write.csv(var_importance, file = paste(model_name,"_var_importance.csv", sep = ""),
            row.names = TRUE)
  
  # save tune info as df and csv file
  for (i in 1:5){
    for (j in 1:5){
      tune_info[[i]][[j]]$cluster <- i
      tune_info[[i]][[j]]$fold <- j
      tune_info[[i]][[j]]$train_n <- fold_train_n[[i]][[j]]
      tune_info[[i]][[j]]$test_n <- fold_test_n[[i]][[j]]
    }
  }
  tune_info <- bind_rows(tune_info)
  write.csv(tune_info, file = paste(
    model_name,"_tune_info.csv", sep = ""), row.names = FALSE)
  
  return(list(pred_df, var_importance, tune_info))
}


################################################################################
# Define spatial no inner buffer CV function
################################################################################
spatial_no_bufferCV_function <- function(data, model_name, model_formula){
  # raise error id model name is not a string
  if (!is.character(model_name)) {
    stop("Error: model_name must be a string.")
  }
  
  # create sf object
  housing_sf <- st_as_sf(
    data, coords = c('x', 'y'), remove = FALSE, crs = 3857)
  
  # Creating clusters for outer loop using kmeans
  coords <- data[,c('x', 'y')]
  set.seed(123)
  knn5 <- kmeans(coords, centers = 5, iter.max = 100, nstart = 10)
  
  # Adding clusters to housing data
  housing_sf$cluster1 <- as.factor(knn5$cluster)
  
  # Using spatialsample to add buffer between folds
  spatialCVouter <- spatial_leave_location_out_cv(
    data = housing_sf,
    group = cluster1,
    radius = NULL,
    buffer = 100000)
  
  
  # lists to store important bits
  train_data <- c()
  test_data <- c()
  variable_importance <- c()
  tune_info <- c()
  tune_train_n <- c()
  tune_test_n <- c()
  fold_test_n <- c()
  fold_train_n <- c()
  
  for (i in 1:5){
    # Using outer clusters to create train/test sets
    train <- housing_sf[spatialCVouter$splits[[i]]$in_id,]
    test <- housing_sf[spatialCVouter$splits[[i]]$out_id,]
    
    
    # Clustering training set using kmeans
    train_coords <- cbind(train$x, train$y)
    set.seed(123)
    knn_train <- kmeans(train_coords, centers = 5, iter.max = 100, nstart = 10)
    train$cluster2 <- as.factor(knn_train$cluster)
    
    # Spatial CV spec
    set.seed(123)
    spatialCV <- spatial_leave_location_out_cv(
      data = train,
      group = cluster2,
      radius = NULL,
      buffer = NULL)
    
    # create RF model spec with parameters for tuning
    rf_spec <- rand_forest(trees = 1000,
                           mtry = tune()) %>%
      set_engine("randomForest") %>%
      set_mode("regression")
    
    # create tuning grid 
    rf_param <- extract_parameter_set_dials(rf_spec) |>
      update(
        mtry = mtry(c(1, 6))
      )
    
    # Create recipe
    rec <- recipe(model_formula, data = train) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) 
    
    # Create workflow
    RF_workflow <- workflow() %>%
      add_recipe(recipe = rec) %>%
      add_model(rf_spec)
    
    # create control grid
    control_g <- control_grid(
      save_pred = TRUE,
      save_workflow = TRUE,
      extract = extract_fit_engine
    )
    
    # Running models
    set.seed(123)
    rf_tune <- 
      RF_workflow |>
      tune_grid(
        spatialCV,
        grid = rf_param %>% grid_regular(levels = 6),
        metrics = metric_set(rmse),
        control = control_g
      )
    
    # save tune info
    tune_info[[i]] <- rf_tune$.metrics
    
    # save fold sizes
    test_n <- c()
    train_n <- c()
    for (j in 1:5){
      test_n[j] <- nrow(rf_tune$.predictions[[j]])/6
      train_n[j] <- length(spatialCV$splits[[j]]$in_id)
    }
    fold_test_n[[i]] <- test_n
    fold_train_n[[i]] <- train_n
    
    # get best tune
    best_tune <- select_best(rf_tune, metric = 'rmse')

    # Removing tune object from memory
    rm(rf_tune)
    gc()
    
    # Fit best tune to entire training set
    set.seed(123)
    final_workflow <-
      RF_workflow |>
      finalize_workflow(best_tune)
    final_fit <- final_workflow |> fit(train)
    
    # Save variable importance
    variable_importance[[i]] <- final_fit %>%
      extract_fit_parsnip() %>%
      pluck("fit") %>%
      importance()
    
    # Predict on test set
    test_pred <- predict(final_fit, new_data = test)
    test$predicted <- test_pred$.pred
    test_data[[i]] <- test
  }
  

  ################################################################################

  # save predictions as df and csv file
  pred_df <- as.data.frame(bind_rows(test_data)) |> select(-geometry) |>
    mutate(residuals = log_medianHouseValue - predicted)
  write.csv(
    pred_df, file = paste(model_name,"_predictions.csv", sep = ""), row.names = FALSE)
  
  # save variable importance as df and csv file
  var_importance <- as.data.frame(bind_cols(variable_importance))
  rownames(var_importance) <- rownames(variable_importance[[1]])
  write.csv(var_importance, file = paste(model_name,"_var_importance.csv", sep = ""),
            row.names = TRUE)
  
  # save tune info as df and csv file
  for (i in 1:5){
    for (j in 1:5){
      tune_info[[i]][[j]]$cluster <- i
      tune_info[[i]][[j]]$fold <- j
      tune_info[[i]][[j]]$train_n <- fold_train_n[[i]][[j]]
      tune_info[[i]][[j]]$test_n <- fold_test_n[[i]][[j]]
    }
  }
  tune_info <- bind_rows(tune_info)
  write.csv(tune_info, file = paste(
    model_name,"_tune_info.csv", sep = ""), row.names = FALSE)
  
  return(list(pred_df, var_importance, tune_info))
}