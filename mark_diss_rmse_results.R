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
# create dfs to store results
cali_combined_test_results_df <- tibble()
cali_combined_valid_results_df <- tibble()

# loop through all models and get validation and prediction results weighted by 
# fold size
for (i in 1:length(model_names)){
  # get RMSE for each outer test for each model
  model_name <- model_names[i]
  model_df <- read.csv(paste(model_name,"_predictions.csv", sep = ""))
  model_df2 <- read.csv(paste(model_name,"_tune_info.csv", sep = ""))
  summary_df <- model_df |>
    group_by(cluster1) |>
    summarise(
      model = model_name,
      test_n = n(),
      test_rmse = sqrt(sum(residuals**2)/(test_n))
      )
  colnames(summary_df)[colnames(summary_df) == "cluster1"] <- "cluster"
  cali_combined_test_results_df <- bind_rows(
    cali_combined_test_results_df, summary_df
    )
  # get mean validation RMSE for each fold for each model 
  valid_df = tibble()
  for (i in 1:5){
    cluster_df <- model_df2 |>
    filter(cluster ==i) |>
      group_by(mtry) |>
      summarise(
        cluster = unique(cluster),
        model = model_name,
        valid_rmse = mean(.estimate),
        valid_n = mean(test_n)
        ) |>
      filter(valid_rmse == min(valid_rmse))
    valid_df <- bind_rows(valid_df, cluster_df)
  }
  cali_combined_valid_results_df <- bind_rows(
    cali_combined_valid_results_df, valid_df
    )
}
combined_results <- full_join(
  cali_combined_valid_results_df, cali_combined_test_results_df)
# Calculate difference between mean validation RMSE and test RMSE 
combined_results <- combined_results |>
  mutate(valid_test_diff = test_rmse - valid_rmse)

# get weighted means across folds, difference in test-valid means plus p-values

cali_rmse_df <- combined_results |>
  group_by(model) |>
  summarise(
    valid_mean_rmse = sum(valid_n*valid_rmse)/sum(valid_n),
    valid_se = sqrt(sum(valid_n*(valid_rmse-valid_mean_rmse)**2)/sum(valid_n-1)),
    test_mean_rmse = sum(test_n*test_rmse)/sum(test_n),
    test_se = sqrt(sum(test_n*(test_rmse-test_mean_rmse)**2)/sum(test_n-1)),
    mean_valid_test_diff = sum(test_n*valid_test_diff)/sum(test_n),
    valid_test_diff_se = sqrt((valid_se**2+test_se**2)/5),
    t_value = mean_valid_test_diff/valid_test_diff_se,
    p_value = 2 * (1 - pt(abs(t_value), 8))
  ) |> # add holm adjusted p-value
  mutate(across(where(is.numeric), \(x) round(x, digits = 3)),
         holm_p_value = p.adjust(p_value, method = 'holm'))

# save csv of results
write.csv(cali_rmse_df, file = 'cali_rmse_results.csv', row.names = FALSE)

################################################################################
#Difference in validation RMSE between models
# No lag models
v_diff_no_lag <- tibble(
  comparison = c('no_lag_spatial_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_no_lag',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_no_lag',]$valid_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_no_lag',]$valid_se**2+
             cali_rmse_df[cali_rmse_df$model=='random_no_lag',]$valid_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))
  
# lag5 predictors models
v_diff_lag5_pred <- tibble(
  comparison = c('lag5_predictors_spatial_minus_random',
                 'lag5_predictors_spatial_minus_buffer',
                 'lag5_predictors_buffer_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_lag5_predictors',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag5_predictors',]$valid_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_lag5_predictors',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_predictors',]$valid_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_predictors',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag5_predictors',]$valid_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag5_predictors',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag5_predictors',]$valid_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag5_predictors',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_predictors',]$valid_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_predictors',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag5_predictors',]$valid_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

# lag5 response models
v_diff_lag5_resp <- tibble(
  comparison = c('lag5_response_spatial_minus_random',
                 'lag5_response_spatial_minus_buffer',
                 'lag5_response_buffer_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_lag5_response',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag5_response',]$valid_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_lag5_response',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_response',]$valid_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_response',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag5_response',]$valid_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag5_response',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag5_response',]$valid_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag5_response',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_response',]$valid_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_response',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag5_response',]$valid_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

# lag20 predictors models
v_diff_lag20_pred <- tibble(
  comparison = c('lag20_predictors_spatial_minus_random',
                 'lag20_predictors_spatial_minus_buffer',
                 'lag20_predictors_buffer_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_lag20_predictors',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag20_predictors',]$valid_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_lag20_predictors',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_predictors',]$valid_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_predictors',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag20_predictors',]$valid_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag20_predictors',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag20_predictors',]$valid_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag20_predictors',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_predictors',]$valid_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_predictors',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag20_predictors',]$valid_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

# lag20 response models
v_diff_lag20_resp <- tibble(
  comparison = c('lag20_response_spatial_minus_random',
                 'lag20_response_spatial_minus_buffer',
                 'lag20_response_buffer_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_lag20_response',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag20_response',]$valid_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_lag20_response',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_response',]$valid_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_response',]$valid_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag20_response',]$valid_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag20_response',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag20_response',]$valid_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag20_response',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_response',]$valid_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_response',]$valid_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag20_response',]$valid_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

valid_diff <- bind_rows(
  v_diff_no_lag,
  v_diff_lag5_pred,
  v_diff_lag5_resp,
  v_diff_lag20_pred,
  v_diff_lag20_resp
)
# add holm adjusted p-values
valid_diff <- valid_diff |> mutate(holm_p_value = p.adjust(p_value, method = 'holm'))

# Save csv of difference in validation RMSE between models
write.csv(valid_diff, file = 'valid_diff.csv', row.names = FALSE)


################################################################################
#Difference in test RMSE between models
# No lag models
t_diff_no_lag <- tibble(
  comparison = c('no_lag_spatial_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_no_lag',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_no_lag',]$test_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_no_lag',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_no_lag',]$test_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

# lag5 predictors models
t_diff_lag5_pred <- tibble(
  comparison = c('lag5_predictors_spatial_minus_random',
                 'lag5_predictors_spatial_minus_buffer',
                 'lag5_predictors_buffer_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_lag5_predictors',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag5_predictors',]$test_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_lag5_predictors',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_predictors',]$test_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_predictors',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag5_predictors',]$test_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag5_predictors',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag5_predictors',]$test_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag5_predictors',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_predictors',]$test_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_predictors',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag5_predictors',]$test_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

# lag5 response models
t_diff_lag5_resp <- tibble(
  comparison = c('lag5_response_spatial_minus_random',
                 'lag5_response_spatial_minus_buffer',
                 'lag5_response_buffer_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_lag5_response',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag5_response',]$test_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_lag5_response',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_response',]$test_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_response',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag5_response',]$test_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag5_response',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag5_response',]$test_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag5_response',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_response',]$test_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag5_response',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag5_response',]$test_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

# lag20 predictors models
t_diff_lag20_pred <- tibble(
  comparison = c('lag20_predictors_spatial_minus_random',
                 'lag20_predictors_spatial_minus_buffer',
                 'lag20_predictors_buffer_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_lag20_predictors',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag20_predictors',]$test_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_lag20_predictors',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_predictors',]$test_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_predictors',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag20_predictors',]$test_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag20_predictors',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag20_predictors',]$test_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag20_predictors',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_predictors',]$test_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_predictors',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag20_predictors',]$test_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

# lag20 response models
t_diff_lag20_resp <- tibble(
  comparison = c('lag20_response_spatial_minus_random',
                 'lag20_response_spatial_minus_buffer',
                 'lag20_response_buffer_minus_random'),
  differnce = c(
    cali_rmse_df[cali_rmse_df$model=='spatial_lag20_response',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag20_response',]$test_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_lag20_response',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_response',]$test_mean_rmse,
    cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_response',]$test_mean_rmse -
      cali_rmse_df[cali_rmse_df$model=='random_lag20_response',]$test_mean_rmse
  ),
  diff_se = c(
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag20_response',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag20_response',]$test_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_lag20_response',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_response',]$test_se**2)/5),
    sqrt((cali_rmse_df[cali_rmse_df$model=='spatial_buff_lag20_response',]$test_se**2+
            cali_rmse_df[cali_rmse_df$model=='random_lag20_response',]$test_se**2)/5)
  )
) |>
  mutate(t_value = differnce/diff_se,
         p_value = 2 * (1 - pt(abs(t_value), 8)))

test_diff <- bind_rows(
  t_diff_no_lag,
  t_diff_lag5_pred,
  t_diff_lag5_resp,
  t_diff_lag20_pred,
  t_diff_lag20_resp
)
# Add holm adjusted p-value
test_diff <- test_diff |> mutate(holm_p_value = p.adjust(p_value, method = 'holm'))

# Save csv of difference in test RMSE for models
write.csv(test_diff, file = 'test_diff.csv', row.names = FALSE)


# Shapiro_Wilks test of normality
shapiro <- tibble()
for (i in 1:length(model_names)){
  model_name <- model_names[i]
  df <- combined_results |>
    filter(model == model_name)
  shap_valid <- shapiro.test(df$valid_rmse)$p.value
  shap_test <- shapiro.test(df$test_rmse)$p.value
  shap_diff <- shapiro.test(df$valid_test_diff)$p.value
  df2 <- tibble(
    model = model_name,
    shap_valid = shap_valid,
    shap_test = shap_test,
    shap_diff = shap_diff
  ) 
  shapiro <- bind_rows(shapiro, df2)
}  
  
  
  
  
  







