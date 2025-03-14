### Cleaning and Preparation

# load and inspect the data -----------------------------------------------------------
library(haven)
data <- read_dta("data/raw/ZA7575.dta")

dim(data)
head(data, 3)

# data cleaning -----------------------------------------------------------
# function to visualize missing data patterns
plot_missing_patterns <- function(data) {
  # calculate percentage missing for each variable
  missing_pct <- colSums(is.na(data))/nrow(data) * 100
  
  # keep only variables with missing values
  missing_pct <- missing_pct[missing_pct > 0]
  
  # sort in descending order
  missing_pct <- sort(missing_pct, decreasing = TRUE)
  
  # create barplot
  par(mar = c(10, 4, 4, 2)) # increase bottom margin for labels
  barplot(missing_pct,
          main = "Percentage of Missing Values by Variable",
          ylab = "% Missing",
          las = 2,  # Rotate labels
          cex.names = 0.7)  # Reduce label size
  
  # print numerical summary
  missing_summary <- data.frame(
    Variable = names(missing_pct),
    Percent_Missing = round(missing_pct, 2)
  )
  print(missing_summary)
  
  # return invisibly for potential further use
  invisible(missing_summary)
}

# function to analyze patterns of missingness by row
analyze_missing_by_row <- function(data) {
  # count missing values per row
  missing_by_row <- rowSums(is.na(data))
  
  # create summary statistics
  row_summary <- data.frame(
    Total_Missing = missing_by_row,
    Percent_Missing = (missing_by_row/ncol(data)) * 100
  )
  
  # print summary statistics
  cat("\nMissing values per row summary:\n")
  print(summary(row_summary))
  
  # create histogram of missing values per row
  hist(missing_by_row,
       main = "Distribution of Missing Values per Row",
       xlab = "Number of Missing Values",
       breaks = 30)
  
  # return summary for potential further use
  invisible(row_summary)
}

# run the analyses
cat("Analyzing missing data patterns...\n")

# plot missing values by variable
cat("\nPlotting missing values by variable:\n")
plot_missing_patterns(data)

# analyse patterns by row
cat("\nAnalyzing patterns by row:\n")
analyze_missing_by_row(data)

# additional numerical summaries
cat("\nCorrelation of missingness:\n")
# create binary missing data matrix
missing_matrix <- is.na(data) * 1

# find variables with high correlation in missingness
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


