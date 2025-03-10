# Load the random forest library
library(randomForest)

# Examine the target variable to determine if it should be treated as a factor
print("Summary of target variable 'qc19':")
print(summary(df_rf_enriched$qc19))
print(paste("Number of unique values:", length(unique(df_rf_enriched$qc19))))

# If qc19 has few unique values, treat it as a classification problem
# Convert to factor for classification
df_rf_enriched$qc19 <- as.factor(df_rf_enriched$qc19)
print("Converting qc19 to factor for classification")

# Run the random forest model for classification
rf_model <- randomForest(qc19 ~ ., data=df_rf_enriched, importance=TRUE)

# Print the model
print(rf_model)

# Plot variable importance 
# For classification, use MeanDecreaseGini (type=2) instead of %IncMSE
varImpPlot(rf_model, type=2, pch=19, main="Variable Importance (Mean Decrease in Gini)")

# If you want to see the top variables more clearly, you can sort them
importance_df <- importance(rf_model)
# For classification, we usually focus on MeanDecreaseGini
sorted_importance <- importance_df[order(importance_df[,"MeanDecreaseGini"], decreasing=TRUE),]
# Print top 10 most important variables
print("Top 10 most important variables:")
print(head(sorted_importance, 10))

# You can also plot partial dependence plots for the most important variables
# Example for top 3 variables (replace with your actual variable names)
vars <- rownames(sorted_importance)[1:3]
for(var in vars) {
  partialPlot(rf_model, df_rf_enriched, var, lwd=2)
}