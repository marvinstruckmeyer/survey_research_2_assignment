# libraries
library(dplyr)
library(tidyr)
library(readr)

# load the data
df_rf_new <- read_csv("df_rf_new.csv") # the imputed dataset
country_df_imputed <- readRDS("country_df_imputed.rds") # external dataset

# custom function to calculate country-level aggregates
calculate_country_aggregates <- function(df) {
  country_agg <- df %>%
    group_by(isocntry) %>%
    summarize(
      
      iso2 = first(isocntry),
      
      # demographics
      mean_age = mean(d11, na.rm = TRUE),
      median_age = median(d11, na.rm = TRUE),
      
      # life satisfaction (recoded)
      mean_life_satisfaction = mean(5 - d70, na.rm = TRUE), 
      pct_satisfied = mean(d70 %in% c(1, 2), na.rm = TRUE) * 100,
      
      # political discussions
      mean_natl_political_discuss = mean(d71_1, na.rm = TRUE),
      mean_eu_political_discuss = mean(d71_2, na.rm = TRUE),
      mean_local_political_discuss = mean(d71_3, na.rm = TRUE),
      pct_discuss_natl_politics = mean(d71_1 %in% c(1, 2), na.rm = TRUE) * 100,
      pct_discuss_eu_politics = mean(d71_2 %in% c(1, 2), na.rm = TRUE) * 100,
      pct_discuss_local_politics = mean(d71_3 %in% c(1, 2), na.rm = TRUE) * 100,
      
      # trade and globalization (recoded)
      mean_trade_support = mean(5 - qa1, na.rm = TRUE),
      pct_trade_support = mean(qa1 %in% c(1, 2), na.rm = TRUE) * 100,
      pct_pro_globalization = mean(qa5a %in% c(1, 2, 3, 5, 7, 8), na.rm = TRUE) * 100,
      pct_anti_globalization = mean(qa5a %in% c(4, 6, 9, 10), na.rm = TRUE) * 100,
      mean_eu_trade_support = mean(5 - qa7, na.rm = TRUE),
      pct_support_eu_trade = mean(qa7 %in% c(1, 2), na.rm = TRUE) * 100,
      mean_esg_support = mean(5 - qa9, na.rm = TRUE),
      pct_support_esg = mean(qa9 %in% c(1, 2), na.rm = TRUE) * 100,
      
      # political and social indicators
      mean_left_right = mean(d1, na.rm = TRUE),
      pct_left = mean(d1 %in% c(1, 2, 3, 4), na.rm = TRUE) * 100,
      pct_center = mean(d1 %in% c(5, 6), na.rm = TRUE) * 100,
      pct_right = mean(d1 %in% c(7, 8, 9, 10), na.rm = TRUE) * 100,
      mean_education_years = mean(d8, na.rm = TRUE),
      median_education_years = median(d8, na.rm = TRUE),
      pct_high_education = mean(d8 >= 20, na.rm = TRUE) * 100,
      pct_rural = mean(d25 == 1, na.rm = TRUE) * 100,
      pct_urban = mean(d25 %in% c(2, 3), na.rm = TRUE) * 100,
      pct_large_urban = mean(d25 == 3, na.rm = TRUE) * 100,
      mean_financial_difficulty = mean(4 - d60, na.rm = TRUE),
      pct_financial_difficulty = mean(d60 %in% c(1, 2), na.rm = TRUE) * 100,
      mean_subjective_class = mean(d63, na.rm = TRUE),
      pct_working_class = mean(d63 == 1, na.rm = TRUE) * 100,
      pct_middle_class = mean(d63 %in% c(2, 3, 4), na.rm = TRUE) * 100,
      pct_upper_class = mean(d63 == 5, na.rm = TRUE) * 100,
      mean_voice_in_eu = mean(5 - d72_1, na.rm = TRUE),
      mean_voice_in_country = mean(5 - d72_2, na.rm = TRUE),
      pct_voice_in_eu = mean(d72_1 %in% c(1, 2), na.rm = TRUE) * 100,
      pct_voice_in_country = mean(d72_2 %in% c(1, 2), na.rm = TRUE) * 100,
      
      # diversity indicators
      pct_friends_diff_ethnic = mean(sd1_1 == 1, na.rm = TRUE) * 100,
      pct_friends_diff_skin = mean(sd1_2 == 1, na.rm = TRUE) * 100,
      pct_friends_roma = mean(sd1_3 == 1, na.rm = TRUE) * 100,
      pct_friends_lgbt = mean(sd1_4 == 1, na.rm = TRUE) * 100,
      pct_friends_disabled = mean(sd1_5 == 1, na.rm = TRUE) * 100,
      pct_friends_diff_religion = mean(sd1_6 == 1, na.rm = TRUE) * 100,
      pct_friends_transgender = mean(sd1_7 == 1, na.rm = TRUE) * 100,
      pct_friends_intersex = mean(sd1_8 == 1, na.rm = TRUE) * 100,
      pct_ethnic_minority = mean(sd2_1 == 1, na.rm = TRUE) * 100,
      pct_skin_minority = mean(sd2_2 == 1, na.rm = TRUE) * 100,
      pct_religious_minority = mean(sd2_3 == 1, na.rm = TRUE) * 100,
      pct_roma = mean(sd2_4 == 1, na.rm = TRUE) * 100,
      pct_sexual_minority = mean(sd2_5 == 1, na.rm = TRUE) * 100,
      pct_disability = mean(sd2_6 == 1, na.rm = TRUE) * 100,
      pct_other_minority = mean(sd2_7 == 1, na.rm = TRUE) * 100,
      pct_any_minority = mean(sd2_1 == 1 | sd2_2 == 1 | sd2_3 == 1 | sd2_4 == 1 | 
                                sd2_5 == 1 | sd2_6 == 1 | sd2_7 == 1, na.rm = TRUE) * 100,
      
      # religion
      pct_catholic = mean(sd3 == 1, na.rm = TRUE) * 100,
      pct_orthodox = mean(sd3 == 2, na.rm = TRUE) * 100,
      pct_protestant = mean(sd3 == 3, na.rm = TRUE) * 100,
      pct_other_christian = mean(sd3 == 4, na.rm = TRUE) * 100,
      pct_jewish = mean(sd3 == 5, na.rm = TRUE) * 100,
      pct_muslim = mean(sd3 %in% c(6, 7, 8), na.rm = TRUE) * 100,
      pct_atheist = mean(sd3 == 13, na.rm = TRUE) * 100,
      pct_nonbeliever = mean(sd3 == 14, na.rm = TRUE) * 100,
      pct_nonreligious = mean(sd3 %in% c(13, 14), na.rm = TRUE) * 100,
      
      # subjective discrimination perception (recoded)
      mean_discrim_ethnic = mean(5 - qc1_1, na.rm = TRUE),
      mean_discrim_skin = mean(5 - qc1_2, na.rm = TRUE),
      mean_discrim_roma = mean(5 - qc1_3, na.rm = TRUE),
      mean_discrim_lgbt = mean(5 - qc1_4, na.rm = TRUE),
      mean_discrim_age = mean(5 - qc1_5, na.rm = TRUE),
      mean_discrim_religion = mean(5 - qc1_6, na.rm = TRUE),
      mean_discrim_disability = mean(5 - qc1_7, na.rm = TRUE),
      mean_discrim_transgender = mean(5 - qc1_8, na.rm = TRUE),
      mean_discrim_gender = mean(5 - qc1_9, na.rm = TRUE),
      mean_discrim_intersex = mean(5 - qc1_10, na.rm = TRUE),
      
    )
  return(country_agg)
}

# apply the function
country_aggregates <- calculate_country_aggregates(df_rf_new)

# join 
country_data_combined <- country_df_imputed %>%
  left_join(country_aggregates, by = "iso2")

# while it's less relevant for a ML model such as random forest, for a multi-level 
# regression model, it's important to standardize the values to allow for better
# coefficient interpretation; the goal is to have both z-scores and 0-1 normalised
# values for each of the original variables (except for variables like 'region')

country_data_combined <- country_data_combined %>%
  # first handle all numeric variables that aren't already standardized
  mutate(
    # z-score standardization
    across(where(is.numeric) & 
             !starts_with("z_") & 
             !starts_with("norm_") &
             !matches("country|iso"), 
           list(z = ~scale(.)[,1]),
           .names = "z_{.col}"),
    
    # also create 0-1 normalized versions
    across(where(is.numeric) & 
             !starts_with("z_") & 
             !starts_with("norm_") &
             !matches("country|iso"), 
           list(norm = ~(.-min(., na.rm=TRUE))/(max(., na.rm=TRUE)-min(., na.rm=TRUE))),
           .names = "norm_{.col}"))

# save the combined dataset
saveRDS(country_data_combined, file = "country_data_combined.rds")
write_csv(country_data_combined, "country_data_combined.csv")

# join with df_rf; the actual, imputed survey data
df_rf_enriched_new <- df_rf_new %>%
  # ISO codes as key
  left_join(country_data_combined, by = c("isocntry" = "iso2"))

# save final dataset
write_csv(df_rf_enriched_new, "df_rf_enriched_new.csv")


