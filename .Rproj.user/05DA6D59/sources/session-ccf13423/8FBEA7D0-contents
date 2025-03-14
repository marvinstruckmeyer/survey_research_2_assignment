###############################################################################
# ALL-IN-ONE CODE: 4 BLOCKS (DEMOGRAPHICS, PERSONAL IDENTITY, DISCRIMINATION, PARADATA)
###############################################################################
library(dplyr)
library(ggplot2)
library(corrplot)
library(corrr)

# 1) Define your variable blocks ----------------------------------------------
#    Feel free to rename or move variables to the block you prefer. 
#    We're assuming these columns exist in df_reduced.

# 1A) Demographics block (example variables)
block_demographics <- c(
  "age", "gender", "education", "d8r2",
  "occupation", "urban_rural", "financial_insecurity",
  "social_class", "religion", "political_ideology",
  "lgb_friends", "trans_friends"
)

# 1B) Personal Identity
block_personal_identity <- c(
  "ethnic_minority", "skin_color_minority", "religious_minority",
  "sexual_lgbt_minority", "disability_minority", "other_minority", "none_minority"
)

# 1C) Discrimination
block_discrimination <- c(
  "trans_discrimination_country", "trans_discrimination_personal",
  "trans_discrimination_workplace", "trans_discrimination_political",
  "country_discrimination_efforts", "country_discrimination_efforts_recoded",
  "trans_workplace_diversity", "trans_colleague", "trans_colleague_recoded",
  "trans_child_relationship", "trans_child_relationship_recoded",
  "lgb_rights", "same_sex_relationship", "same_sex_marriage",
  "lgb_school_materials", "trans_school_materials", "intersex_school_materials",
  "two_men_public_affection", "two_men_public_affection_recoded",
  "two_women_public_affection", "two_women_public_affection_recoded",
  "non_gendered_docs"
)

# 1D) Paradata
block_paradata <- c(
  "interview_date",
  "interview_start_time",
  "interview_duration",
  "interview_duration_recoded",
  "people_present_during_interview",
  "respondent_cooperation"
)

# We'll assume qc19 is your primary outcome of interest
outcome_var <- "qc19"

# 2) Define a helper function to plot correlations with qc19 ------------------
plot_corr_with_qc19 <- function(data, vars_block, outcome = "qc19", block_title = "Block") {
  
  # Subset the data to these variables + outcome
  needed_vars <- c(outcome, vars_block)
  df_block <- data[, needed_vars, drop = FALSE]
  
  # Keep only numeric columns
  df_block_num <- df_block %>%
    dplyr::select(where(is.numeric))
  
  # Ensure outcome is present and numeric
  stopifnot(outcome %in% names(df_block_num))
  
  # Identify the other numeric vars
  other_vars <- setdiff(names(df_block_num), outcome)
  
  # Compute correlation with outcome
  cor_df <- data.frame(
    variable = other_vars,
    correlation = sapply(other_vars, function(v) {
      cor(df_block_num[[v]], df_block_num[[outcome]], use = "complete.obs")
    })
  )
  
  # Sort by absolute correlation
  cor_df <- cor_df %>% arrange(desc(abs(correlation)))
  
  # Reorder factor levels so negative correlations plot at bottom
  cor_df <- cor_df %>%
    arrange(correlation) %>%
    mutate(variable = factor(variable, levels = variable))
  
  # Plot horizontal bar chart
  ggplot(cor_df, aes(x = variable, y = correlation)) +
    geom_col(fill = "#4682B4", width = 0.6) +
    geom_text(
      aes(label = sprintf("%.2f", correlation)),
      hjust = ifelse(cor_df$correlation >= 0, -0.1, 1.1),
      color = "black", size = 3.5
    ) +
    coord_flip() +
    scale_y_continuous(
      limits = c(
        min(cor_df$correlation) - 0.1,
        max(cor_df$correlation) + 0.1
      )
    ) +
    labs(
      title = paste0(block_title, ": Correlation with ", outcome),
      x = NULL,
      y = "Correlation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 9),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# 3) Generate bar charts for each block ---------------------------------------
#    We'll assume the main data frame is called df_reduced

# 3A) Demographics
plot_corr_with_qc19(df_reduced, block_demographics, outcome_var, "DEMOGRAPHICS")

# 3B) Personal Identity
plot_corr_with_qc19(df_reduced, block_personal_identity, outcome_var, "PERSONAL IDENTITY")

# 3C) Discrimination
plot_corr_with_qc19(df_reduced, block_discrimination, outcome_var, "DISCRIMINATION")

# 3D) Paradata
plot_corr_with_qc19(df_reduced, block_paradata, outcome_var, "PARADATA")


library(dplyr)
library(corrr)

# 1) Keep only numeric columns
df_reduced_numeric <- df_reduced %>%
  select_if(is.numeric)

# 2) Correlate the numeric columns
cor_df <- correlate(df_reduced_numeric)

# 3) Stretch into a long format, then filter by your threshold
high_cor_pairs <- cor_df %>%
  stretch() %>%
  filter(abs(r) > 0.5, x != y)  # pick threshold, exclude self-correlations

high_cor_pairs