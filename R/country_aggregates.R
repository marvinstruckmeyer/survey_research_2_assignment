# libraries
library(dplyr)
library(tidyr)
library(readr)

# load the data
df_rf <- readRDS("R/df_rf.rds") # the imputed dataset
country_df_imputed <- readRDS(country_df_imputed, file = "country_df_imputed.rds") # external dataset

# create a lookup table for country codes to ISO codes
country_iso_lookup <- data.frame(
  country = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28),
  iso2 = c("BE", "DK", "DE", "GR", "ES", "FR", "IE", "IT", "LU", "NL", "PT", "GB", "AT", "SE", "FI", "CY", "CZ", "EE", "HU", "LV", "LT", "MT", "PL", "SK", "SI", "BG", "RO", "HR"),
  country_name = c("Belgium", "Denmark", "Germany", "Greece", "Spain", "France", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "United Kingdom", "Austria", "Sweden", "Finland", "Cyprus", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania", "Malta", "Poland", "Slovakia", "Slovenia", "Bulgaria", "Romania", "Croatia"))

# function to calculate country-level aggregates
calculate_country_aggregates <- function(df) {
  # ensure we're working with the right columns
  required_cols <- c("country", "isocntry", "d11", "d70", "d71_1", "d71_2", "d71_3", 
                     "qa1", "qa5a", "qa7", "qa9", "d1", "d8", "d25", 
                     "d60", "d63", "d72_1", "d72_2", "sd1_1", "sd1_2", 
                     "sd1_3", "sd1_4", "sd1_5", "sd1_6", "sd1_7", "sd1_8",
                     "sd2_1", "sd2_2", "sd2_3", "sd2_4", "sd2_5", "sd2_6", 
                     "sd2_7", "sd2_8", "sd2_9", "sd2_10", "sd3", 
                     "qc1_1", "qc1_2", "qc1_3", "qc1_4", "qc1_5", 
                     "qc1_6", "qc1_7", "qc1_8", "qc1_9", "qc1_10", "qc19")
  
  # check if all required columns exist
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # create country-level aggregates
  country_agg <- df %>%
    group_by(country) %>%
    summarize(
      # add country ISO code (if "isocntry" not available in the dataframe)
      # first_iso = first(isocntry, na.rm = TRUE),
      # D11: Average age
      mean_age = mean(d11, na.rm = TRUE),
      median_age = median(d11, na.rm = TRUE),
      
      # D70: Life satisfaction (alternative to happiness indicator)
      # 1=Very satisfied, 2=Fairly satisfied, 3=Not very satisfied, 4=Not at all satisfied
      # recode so higher values = more satisfied
      mean_life_satisfaction = mean(5 - d70, na.rm = TRUE), # Recoding 1-4 to 4-1
      pct_satisfied = mean(d70 %in% c(1, 2), na.rm = TRUE) * 100, # % Very or fairly satisfied
      
      # D71: Political discussions frequency
      # 1=Frequently, 2=Occasionally, 3=Never
      # Lower values = more frequent discussions
      mean_natl_political_discuss = mean(d71_1, na.rm = TRUE),
      mean_eu_political_discuss = mean(d71_2, na.rm = TRUE),
      mean_local_political_discuss = mean(d71_3, na.rm = TRUE),
      pct_discuss_natl_politics = mean(d71_1 %in% c(1, 2), na.rm = TRUE) * 100, # % of people who discuss at least occasionally
      pct_discuss_eu_politics = mean(d71_2 %in% c(1, 2), na.rm = TRUE) * 100,
      pct_discuss_local_politics = mean(d71_3 %in% c(1, 2), na.rm = TRUE) * 100,
      
      # QA1: Economic liberalism proxy (benefit from international trade)
      # 1=Yes, a lot; 2=Yes, somewhat; 3=No, not really; 4=No, not at all
      # recode sothat  higher values = more supportive of trade
      mean_trade_support = mean(5 - qa1, na.rm = TRUE), # Recoding to 4-1
      pct_trade_support = mean(qa1 %in% c(1, 2), na.rm = TRUE) * 100, # % Yes
      
      # QA5a: Attitudes toward globalization
      # depends on coding - transforming to "in favor of globalization"
      # assuming: 1-9 are various positive views, 10-11 are negative, 12=neither
      pct_pro_globalization = mean(qa5a %in% c(1, 2, 3, 5, 7, 8), na.rm = TRUE) * 100,
      pct_anti_globalization = mean(qa5a %in% c(4, 6, 9, 10), na.rm = TRUE) * 100,
      
      # QA7: Support for EU in trade matters
      # 1=Totally agree, 2=Tend to agree, 3=Tend to disagree, 4=Totally disagree
      # recode so higher values = more supportive
      mean_eu_trade_support = mean(5 - qa7, na.rm = TRUE), # Recoding to 4-1
      pct_support_eu_trade = mean(qa7 %in% c(1, 2), na.rm = TRUE) * 100, # % Agree
      
      # QA9: Support for ESG in trade policy
      # 1=Totally agree, 2=Tend to agree, 3=Tend to disagree, 4=Totally disagree
      # recode so higher values = more supportive
      mean_esg_support = mean(5 - qa9, na.rm = TRUE), # Recoding to 4-1
      pct_support_esg = mean(qa9 %in% c(1, 2), na.rm = TRUE) * 100, # % Agree
      
      # D1: Political orientation (left-right)
      # 1-10 scale where 1=Left and 10=Right
      mean_left_right = mean(d1, na.rm = TRUE),
      pct_left = mean(d1 %in% c(1, 2, 3, 4), na.rm = TRUE) * 100, # % Left (1-4)
      pct_center = mean(d1 %in% c(5, 6), na.rm = TRUE) * 100,     # % Center (5-6)
      pct_right = mean(d1 %in% c(7, 8, 9, 10), na.rm = TRUE) * 100, # % Right (7-10)
      
      # D8: Education (age when finished education)
      mean_education_years = mean(d8, na.rm = TRUE),
      median_education_years = median(d8, na.rm = TRUE),
      pct_high_education = mean(d8 >= 20, na.rm = TRUE) * 100, # % with education beyond 20
      
      # D25: Urban vs rural
      # 1=Rural area or village, 2=Small/middle town, 3=Large town
      pct_rural = mean(d25 == 1, na.rm = TRUE) * 100,
      pct_urban = mean(d25 %in% c(2, 3), na.rm = TRUE) * 100,
      pct_large_urban = mean(d25 == 3, na.rm = TRUE) * 100,
      
      # D60: Financial difficulties
      # 1=Most of the time, 2=From time to time, 3=Almost never/never
      # recode so higher values = more difficulties
      mean_financial_difficulty = mean(4 - d60, na.rm = TRUE), # Recoding to 3-1
      pct_financial_difficulty = mean(d60 %in% c(1, 2), na.rm = TRUE) * 100, # % with difficulties
      
      # D63: Subjective social class
      # 1=Working class to 5=Higher class
      mean_subjective_class = mean(d63, na.rm = TRUE),
      pct_working_class = mean(d63 == 1, na.rm = TRUE) * 100,
      pct_middle_class = mean(d63 %in% c(2, 3, 4), na.rm = TRUE) * 100,
      pct_upper_class = mean(d63 == 5, na.rm = TRUE) * 100,
      
      # D72: Voice counts in EU/country
      # 1=Totally agree, 2=Tend to agree, 3=Tend to disagree, 4=Totally disagree
      # recode so higher values = more agreement
      mean_voice_in_eu = mean(5 - d72_1, na.rm = TRUE), # Recoding to 4-1
      mean_voice_in_country = mean(5 - d72_2, na.rm = TRUE), # Recoding to 4-1
      pct_voice_in_eu = mean(d72_1 %in% c(1, 2), na.rm = TRUE) * 100, # % Agree
      pct_voice_in_country = mean(d72_2 %in% c(1, 2), na.rm = TRUE) * 100, # % Agree
      
      # SD1: Having friends who are different (1=Yes, 2=No)
      pct_friends_diff_ethnic = mean(sd1_1 == 1, na.rm = TRUE) * 100,
      pct_friends_diff_skin = mean(sd1_2 == 1, na.rm = TRUE) * 100,
      pct_friends_roma = mean(sd1_3 == 1, na.rm = TRUE) * 100,
      pct_friends_lgbt = mean(sd1_4 == 1, na.rm = TRUE) * 100,
      pct_friends_disabled = mean(sd1_5 == 1, na.rm = TRUE) * 100,
      pct_friends_diff_religion = mean(sd1_6 == 1, na.rm = TRUE) * 100,
      pct_friends_transgender = mean(sd1_7 == 1, na.rm = TRUE) * 100, # Specifically requested
      pct_friends_intersex = mean(sd1_8 == 1, na.rm = TRUE) * 100,
      
      # SD2: Belonging to a minority (1=Yes for each type)
      pct_ethnic_minority = mean(sd2_1 == 1, na.rm = TRUE) * 100,
      pct_skin_minority = mean(sd2_2 == 1, na.rm = TRUE) * 100,
      pct_religious_minority = mean(sd2_3 == 1, na.rm = TRUE) * 100,
      pct_roma = mean(sd2_4 == 1, na.rm = TRUE) * 100,
      pct_sexual_minority = mean(sd2_5 == 1, na.rm = TRUE) * 100,
      pct_disability = mean(sd2_6 == 1, na.rm = TRUE) * 100,
      pct_other_minority = mean(sd2_7 == 1, na.rm = TRUE) * 100,
      pct_any_minority = mean(sd2_1 == 1 | sd2_2 == 1 | sd2_3 == 1 | sd2_4 == 1 | 
                                sd2_5 == 1 | sd2_6 == 1 | sd2_7 == 1, na.rm = TRUE) * 100,
      
      # SD3: Religion (1-16 different religions)
      pct_catholic = mean(sd3 == 1, na.rm = TRUE) * 100,
      pct_orthodox = mean(sd3 == 2, na.rm = TRUE) * 100,
      pct_protestant = mean(sd3 == 3, na.rm = TRUE) * 100,
      pct_other_christian = mean(sd3 == 4, na.rm = TRUE) * 100,
      pct_jewish = mean(sd3 == 5, na.rm = TRUE) * 100,
      pct_muslim = mean(sd3 %in% c(6, 7, 8), na.rm = TRUE) * 100, # All Muslim denominations
      pct_atheist = mean(sd3 == 13, na.rm = TRUE) * 100,
      pct_nonbeliever = mean(sd3 == 14, na.rm = TRUE) * 100,
      pct_nonreligious = mean(sd3 %in% c(13, 14), na.rm = TRUE) * 100, # Atheist + nonbeliever
      
      # QC1: Perception of discrimination (1=Very widespread to 4=Very rare)
      # recode so higher values = more discrimination perceived
      mean_discrim_ethnic = mean(5 - qc1_1, na.rm = TRUE), # Recoding to 4-1
      mean_discrim_skin = mean(5 - qc1_2, na.rm = TRUE),
      mean_discrim_roma = mean(5 - qc1_3, na.rm = TRUE),
      mean_discrim_lgbt = mean(5 - qc1_4, na.rm = TRUE),
      mean_discrim_age = mean(5 - qc1_5, na.rm = TRUE),
      mean_discrim_religion = mean(5 - qc1_6, na.rm = TRUE),
      mean_discrim_disability = mean(5 - qc1_7, na.rm = TRUE),
      mean_discrim_transgender = mean(5 - qc1_8, na.rm = TRUE),
      mean_discrim_gender = mean(5 - qc1_9, na.rm = TRUE),
      mean_discrim_intersex = mean(5 - qc1_10, na.rm = TRUE),
      
      # QC19: Support for transgender ID change
      # 1=Yes, 2=No
      pct_support_trans_id = mean(qc19 == 1, na.rm = TRUE) * 100,
      
      # calculate sample size for each country
      sample_size = n()
    )
  
  return(country_agg)
}

# apply the function to the dataset
country_aggregates <- calculate_country_aggregates(df_rf)

# add ISO codes to the country aggregates if not already present
if (!"first_iso" %in% names(country_aggregates) || all(is.na(country_aggregates$first_iso))) {
  country_aggregates <- country_aggregates %>%
    left_join(country_iso_lookup, by = "country")
}

# merge with existing imputed country_df_imputed
# using both country_code and iso2 for more reliable merging
country_data_combined <- country_df_imputed %>%
  left_join(country_aggregates, by = c("country_code" = "country"))

# double-check ISO code matching
country_data_combined <- country_data_combined %>%
  mutate(
    iso_match_check = ifelse(iso2.x == iso2.y, "Match", "Mismatch"),
    # keep the original ISO code from country_level_df if there's a mismatch
    iso2 = ifelse(is.na(iso2.y) | iso2.x == iso2.y, iso2.x, paste0(iso2.x, "/", iso2.y))) %>%
  select(-iso2.x, -iso2.y, -iso_match_check) # remove temporary columns

# delete a few variables
country_data_combined <- country_data_combined %>%
  select(-c("country_name.y", "iso2"))

# save
saveRDS(country_data_combined, file = "country_data_combined.rds")
write_csv(country_data_combined, "country_data_combined.csv")

# merge with existing survey data
# left-join on the country variable as the common key
df_rf_enriched <- df_rf %>%
  left_join(country_data_combined, by = c("country" = "country_code"))

# Verify the join worked correctly
glimpse(df_rf_enriched)
# Check a few rows to make sure the country-level variables were properly added
head(df_rf_enriched %>% select(country, isocntry, mean_life_satisfaction, pct_support_trans_id))

# Save the enriched dataset
saveRDS(df_rf_enriched, file = "df_rf_enriched.rds")
write_csv(df_rf_enriched, "df_rf_enriched.csv")




