### Cleaning and Preparation

# load and inspect the data -----------------------------------------------------------
library(haven)
data <- read_dta("data/raw/ZA7575.dta")

dim(data)
head(data, 3)

# data cleaning -----------------------------------------------------------
# Function to visualize missing data patterns
plot_missing_patterns <- function(data) {
  # Calculate percentage missing for each variable
  missing_pct <- colSums(is.na(data))/nrow(data) * 100
  
  # Keep only variables with missing values
  missing_pct <- missing_pct[missing_pct > 0]
  
  # Sort in descending order
  missing_pct <- sort(missing_pct, decreasing = TRUE)
  
  # Create barplot
  par(mar = c(10, 4, 4, 2)) # Increase bottom margin for labels
  barplot(missing_pct,
          main = "Percentage of Missing Values by Variable",
          ylab = "% Missing",
          las = 2,  # Rotate labels
          cex.names = 0.7)  # Reduce label size
  
  # Print numerical summary
  missing_summary <- data.frame(
    Variable = names(missing_pct),
    Percent_Missing = round(missing_pct, 2)
  )
  print(missing_summary)
  
  # Return invisibly for potential further use
  invisible(missing_summary)
}

# Function to analyze patterns of missingness by row
analyze_missing_by_row <- function(data) {
  # Count missing values per row
  missing_by_row <- rowSums(is.na(data))
  
  # Create summary statistics
  row_summary <- data.frame(
    Total_Missing = missing_by_row,
    Percent_Missing = (missing_by_row/ncol(data)) * 100
  )
  
  # Print summary statistics
  cat("\nMissing values per row summary:\n")
  print(summary(row_summary))
  
  # Create histogram of missing values per row
  hist(missing_by_row,
       main = "Distribution of Missing Values per Row",
       xlab = "Number of Missing Values",
       breaks = 30)
  
  # Return summary for potential further use
  invisible(row_summary)
}

# Run the analyses
cat("Analyzing missing data patterns...\n")

# Plot missing values by variable
cat("\nPlotting missing values by variable:\n")
plot_missing_patterns(data)

# Analyze patterns by row
cat("\nAnalyzing patterns by row:\n")
analyze_missing_by_row(data)

# Additional numerical summaries
cat("\nCorrelation of missingness:\n")
# Create binary missing data matrix
missing_matrix <- is.na(data) * 1

# Find variables with high correlation in missingness
missing_cor <- cor(missing_matrix)
high_cor <- which(abs(missing_cor) > 0.7 & missing_cor != 1, arr.ind = TRUE)

if(nrow(high_cor) > 0) {
  cor_summary <- data.frame(
    Var1 = rownames(missing_cor)[high_cor[,1]],
    Var2 = colnames(missing_cor)[high_cor[,2]],
    Correlation = missing_cor[high_cor])
  print(cor_summary[order(-abs(cor_summary$Correlation)),])
} else {
  cat("No high correlations (>0.7) found in missing data patterns\n")
}


