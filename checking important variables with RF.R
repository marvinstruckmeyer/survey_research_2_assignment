# Load required library
library(randomForest)

# APPROACH 1: Correlation-based ranking (fastest)
# Convert target to numeric if needed
if(is.factor(df_rf_enriched$qc19)) {
  qc19_numeric <- as.numeric(df_rf_enriched$qc19)
} else {
  qc19_numeric <- df_rf_enriched$qc19
}

# Calculate correlations with target
correlations <- numeric(ncol(df_rf_enriched) - 1)
var_names <- names(df_rf_enriched)[names(df_rf_enriched) != "qc19"]

# Use a simple loop to avoid memory issues
for(i in 1:length(var_names)) {
  var <- var_names[i]
  # Handle factors by converting to numeric
  if(is.factor(df_rf_enriched[[var]]) || is.character(df_rf_enriched[[var]])) {
    correlations[i] <- 0  # Skip factors/characters if memory is issue
  } else {
    # Use absolute correlation
    correlations[i] <- abs(cor(qc19_numeric, df_rf_enriched[[var]], 
                               use = "pairwise.complete.obs"))
  }
}

# Create dataframe of results
importance_df <- data.frame(
  Variable = var_names,
  Correlation = correlations
)

# Sort by correlation
importance_df <- importance_df[order(importance_df$Correlation, decreasing = TRUE),]

# Show top 20 variables by correlation
print("Top 20 variables by correlation magnitude:")
print(head(importance_df, 20))

# APPROACH 2: If you still want a small Random Forest (much simpler)
# Take only top 30 variables by correlation and a sample of data
if(nrow(df_rf_enriched) > 5000) {
  # Use a small sample to keep it fast
  set.seed(123)
  sample_rows <- sample(1:nrow(df_rf_enriched), 5000)
  df_sample <- df_rf_enriched[sample_rows, c("qc19", head(importance_df$Variable, 30))]
  
  # Convert target to factor for classification
  df_sample$qc19 <- as.factor(df_sample$qc19)
  
  # Run a very small random forest 
  rf_small <- randomForest(
    qc19 ~ ., 
    data = df_sample,
    ntree = 50,  # Very small number of trees
    importance = TRUE
  )
  
  # Plot importance
  print("Random Forest Variable Importance (on reduced dataset):")
  varImpPlot(rf_small)
}