## why still z-scores for mean religosity, left-right  etc. 

country_level_df <- readRDS("country_level_df.rds")

# first, let's check which variables have missing values
missing_summary <- colSums(is.na(country_level_df))
missing_summary <- missing_summary[missing_summary > 0]
print(missing_summary)

# Visualize missing data patterns
vis_miss(country_level_df)

# we have the following imputation strategy:
# (1) we will have to delete the variables from the ESS because the Eurobarometer
# survey contains similar information and imputing these values didn't seem appropriate
# (2) we chose to impute rest of the variables with NAs (democracy scores, gender equality index, 
# and the happiness score), using KNN

# (1) 
# delete ESS variables
ESS_variables <- c(
  # original survey variables
  "n_respondents", "n_valid", "lgbt_support_percent", "mean_religiosity", 
  "mean_left_right", "mean_equal_values", "mean_country_attach", 
  "mean_eduyrs", "mean_age", "pct_young", "pct_high_educ", "se_lgbt_support",
  
  # additional ESS metrics
  "pct_missing_lgbt", "age_lgbt_corr", "relig_lgbt_corr", 
  "lgbt_support_inequality", "educ_gradient",
  
  # z-score transformations of ESS variables
  "z_religiosity", "z_left_right", "z_equal_values", 
  "z_country_attach", "z_eduyrs", "z_age")

country_level_df <- country_level_df %>%
  select(-all_of(ESS_variables))

# (2)
# create separate datasets for variables that need imputation
missing_variables <- names(missing_summary)[missing_summary < 5 & 
                                             !(names(missing_summary) %in% ESS_variables)]

# for our KNN imputation, we need to identify variables to use as predictors
# these should be variables without NAs that also correlate with those we want to impute

# identify complete variables that could serve as predictors
complete_variables <- names(country_level_df)[colSums(is.na(country_level_df)) == 0]
complete_variables <- complete_variables[!complete_variables %in% c("Unnamed: 0", "country_name", "iso2", "region")]

print(complete_variables)

# custom function to impute variables with KNN, handling non-numeric data
knn_impute <- function(data, vars_to_impute, predictor_vars, k = 5) {
  # create a copy of the data
  imputed_data <- data
  
  # ensure predictor variables are numeric and complete
  pred_data <- data[, predictor_vars, drop = FALSE]
  pred_data <- as.data.frame(lapply(pred_data, function(x) as.numeric(x)))
  
  # remove any non-numeric columns
  numeric_cols <- sapply(pred_data, is.numeric)
  if(any(!numeric_cols)) {
    warning("Removing non-numeric predictor columns: ", 
            paste(names(pred_data)[!numeric_cols], collapse=", "))
    pred_data <- pred_data[, numeric_cols, drop=FALSE]
    predictor_vars <- predictor_vars[numeric_cols]
  }
  
  # handle missing values in predictor variables by using column means
  for(col in names(pred_data)) {
    if(any(is.na(pred_data[[col]]))) {
      pred_data[[col]][is.na(pred_data[[col]])] <- mean(pred_data[[col]], na.rm=TRUE)
    }
  }
  
  # standardize the predictor variables manually to avoid issues
  pred_data_std <- as.data.frame(scale(pred_data))
  
  # for each variable to impute:
  for (var in vars_to_impute) {
    if (var %in% names(data) && sum(is.na(data[[var]])) > 0) {
      # ensure the target variable is numeric
      if (!is.numeric(data[[var]])) {
        cat("Skipping", var, "as it is not numeric\n")
        next
      }
      
      # identify cases with missing values
      missing_indices <- which(is.na(data[[var]]))
      
      # for each missing value:
      for (idx in missing_indices) {
        # calculate Euclidean distances to all other countries
        distances <- numeric(nrow(data))
        
        for (i in 1:nrow(data)) {
          if (i != idx) {
            # calculate squared differences for each predictor
            squared_diffs <- sapply(names(pred_data_std), function(p) {
              (pred_data_std[idx, p] - pred_data_std[i, p])^2
            })
            
            # sum and take square root for Euclidean distance
            distances[i] <- sqrt(sum(squared_diffs, na.rm=TRUE))
          } else {
            distances[i] <- Inf  # don't use the country itself
          }
        }
        
        # find k nearest neighbors with non-missing values for this variable
        valid_neighbors <- which(!is.na(data[[var]]) & !is.infinite(distances))
        
        if (length(valid_neighbors) > 0) {
          # order the neighbors by distance
          neighbor_order <- order(distances[valid_neighbors])
          valid_neighbors <- valid_neighbors[neighbor_order]
          
          # take the k nearest, or as many as available if fewer than k
          k_actual <- min(k, length(valid_neighbors))
          nearest_k <- valid_neighbors[1:k_actual]
          
          # impute as the average of the k nearest neighbors
          imputed_data[idx, var] <- mean(data[nearest_k, var], na.rm = TRUE)
          
          cat("Imputed", var, "for", data$country_name[idx], 
              "using neighbors:", paste(data$country_name[nearest_k], collapse=", "), "\n")
        } else {
          cat("Warning: Could not impute", var, "for", data$country_name[idx], 
              "- no valid neighbors found\n")
        }
      }
    }
  }
  
  return(imputed_data)
}

# now use the function to impute variables with missing values
# first ensure we have the right variable types
country_df_numeric <- country_level_df

for (var in missing_variables) {
  if (var %in% names(country_level_df) && !is.numeric(country_level_df[[var]])) {
    country_df_numeric[[var]] <- as.numeric(as.character(country_level_df[[var]]))
  }
}

# run the imputation with proper error handling
tryCatch({
  country_df_imputed <- knn_impute(
    data = country_df_numeric,
    vars_to_impute = missing_variables,
    predictor_vars = complete_variables,
    k = 3  # using 3 nearest neighbors
  )
  print("Imputation completed successfully!")
}, error = function(e) {
  # if the knn_impute function still fails, let's try an alternative approach
  print(paste("KNN imputation error:", e$message))
  print("Switching to a simpler mean imputation approach...")
  
  country_df_imputed <- country_df_numeric
  for (var in missing_variables) {
    if (sum(is.na(country_df_imputed[[var]])) > 0) {
      # Calculate mean of non-missing values
      var_mean <- mean(country_df_imputed[[var]], na.rm = TRUE)
      # Replace missing values with mean
      country_df_imputed[[var]][is.na(country_df_imputed[[var]])] <- var_mean
      print(paste("Imputed", var, "with mean value:", var_mean))
    }
  }
  return(country_df_imputed)
})

# save
saveRDS(country_df_imputed, file = "country_df_imputed.rds") 
write.csv(country_df_imputed, "country_df_imputed.csv", row.names = FALSE)

