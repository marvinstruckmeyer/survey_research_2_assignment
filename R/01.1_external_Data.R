# Table of contents -------------------------------------------------------
# 1. vdemdata CHECK
# 2. Rainbowmap CHECK
# 3. Economist Democracy Scores for 2018 CHECK
# 4. Happiness CHECK
# 5. GDP per capita CHECK
# 6. ESS round 9 ||
# 7. Unemployment rate 


## further ideas:
# which political parties governed in the years before (i.e., left, centre-right etc.)
# Gini index
# migrant acceptance scores
# welfare state/ size of government relative to GDP
# physical/ mental health issues; quality and equity of healthcare
# solidarity with minority groups
# education


# load libraries ----------------------------------------------------------
# devtools::install_github("vdeminstitute/vdemdata")

library(tidyverse)
library(dplyr)
library(readr)
library(vdemdata)
library(survey) 
library(countrycode)
library(rvest)
library(stringr)


# mapping -----------------------------------------------------------------
country_mapping <- data.frame(
  country_name = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                   "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                   "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                   "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
                   "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
                   "Spain", "Sweden", "United Kingdom"),
  iso2 = c("AT", "BE", "BG", "HR", "CY", 
           "CZ", "DK", "EE", "FI", "FR", 
           "DE", "GR", "HU", "IE", "IT", # GR = EL in the survey
           "LV", "LT", "LU", "MT", "NL", 
           "PL", "PT", "RO", "SK", "SI", 
           "ES", "SE", "GB"), # GB = UK in the survey
  country_code = 1:28,  
  stringsAsFactors = FALSE)


# 1. vdemdata ----------------------------------------------------------------
# install the package frlm GitHub first
# devtools::install_github("vdeminstitute/vdemdata")
vdem_data <- vdem

eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                  "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                  "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", 
                  "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", 
                  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

# filter for most recent data (2019 to match survey data) for EU countries
vdem_eu_2019 <- vdem_data %>%
  filter(country_name %in% eu_countries, year == 2019) %>%
  select(country_name, v2x_libdem, v2x_polyarchy, v2x_gender, 
         v2x_egaldem, v2x_liberal, v2xcs_ccsi, v2x_freexp)  # select relevant variables

saveRDS(vdem_eu_2019, file = "vdem_eu_2019.rds")
write.csv(vdem_eu_2019, "vdem_eu_2019.csv")


## from the codebook: 
# v2x_libdem: index of liberal democracy
# v2x_polyarchy: index of electoral democracy
# v2x_gender: index of women's political empowerment
# v2x_egaldem: index of egalitarian democracy   
# v2x_liberal: index of civil liberties
# v2xcs_ccsi:  index of civil society participation
# v2x_freexp: index of freedom of expression



# 2. Rainbowmap --------------------------------------------------------------
# https://rainbowmap.ilga-europe.org/

rainbow_data <- read_csv("data/raw/2024-rainbow-map-data.csv")

# but the problem: that's for 2024, not 2019 or before 2019
# hence, we would need to get the data for 2019 and years before:

# create data frames for each year
df_2019 <- data.frame(
  Country = c("Malta", "Belgium", "United Kingdom", "Norway", "France", "Finland",
              "Denmark", "Spain", "Portugal", "Sweden", "Netherlands", "Austria",
              "Germany", "Croatia", "Greece", "Ireland", "Hungary", "Luxembourg",
              "Iceland", "Slovenia", "Montenegro", "Estonia", "Switzerland", "Georgia",
              "Bosnia & Herzegovina", "Slovakia", "Albania", "Serbia", "Bulgaria", 
              "Czechia", "Kosovo", "Andorra", "Cyprus", "Romania", "Ukraine", 
              "Lithuania", "Italy", "Poland", "Latvia", "Belarus", "Moldova", 
              "Russia", "North Macedonia", "Liechtenstein", "San Marino", "Armenia",
              "Turkey", "Monaco", "Azerbaijan"),
  Value = c(74.72, 70.40, 67.62, 63.64, 62.73, 62.20, 60.31, 58.49, 57.50, 52.86,
            49.72, 48.54, 47.45, 45.83, 44.82, 44.72, 42.83, 41.34, 40.20, 39.82, 
            37.16, 34.38, 30.06, 29.49, 29.37, 29.04, 27.74, 26.43, 25.75, 25.50,
            25.34, 23.93, 22.03, 20.67, 19.97, 19.91, 19.43, 18.10, 16.83, 15.82,
            15.10, 11.75, 10.88, 10.08, 9.87, 8.50, 8.50, 7.31, 5.67),
  Year = 2019)

df_2018 <- data.frame(
  Country = c("Malta", "Belgium", "United Kingdom", "Finland", "France", "Norway",
              "Portugal", "Denmark", "Spain", "Sweden", "Netherlands", "Germany",
              "Austria", "Greece", "Ireland", "Croatia", "Slovenia", "Luxembourg",
              "Iceland", "Hungary", "Estonia", "Switzerland", "Montenegro", "Andorra",
              "Albania", "Kosovo", "Bosnia & Herzegovina", "Serbia", "Czechia", 
              "Cyprus", "Slovakia", "Italy", "Georgia", "Bulgaria", "Lithuania",
              "Romania", "Ukraine", "Poland", "Liechtenstein", "Latvia", 
              "North Macedonia", "Belarus", "Moldova", "San Marino", "Russia", 
              "Monaco", "Turkey", "Armenia", "Azerbaijan"),
  Value = c(91.94, 78.70, 73.48, 73.27, 72.81, 72.74, 69.16, 67.69, 67.03, 60.10,
            59.64, 59.00, 56.40, 52.32, 52.22, 50.58, 47.73, 47.48, 47.22, 47.16,
            39.34, 38.44, 37.74, 34.81, 33.24, 32.98, 31.38, 29.68, 29.20, 28.95,
            28.65, 28.82, 25.87, 24.15, 21.78, 21.12, 20.95, 18.23, 17.87, 16.07,
            14.03, 13.35, 13.08, 12.32, 10.80, 9.78, 8.60, 7.20, 4.70),
  Year = 2018)

df_2017 <- data.frame(
  Country = c("Malta", "Norway", "United Kingdom", "Belgium", "France", "Portugal",
              "Finland", "Denmark", "Spain", "Netherlands", "Croatia", "Sweden", 
              "Austria", "Germany", "Ireland", "Iceland", "Greece", "Luxembourg", 
              "Hungary", "Slovenia", "Montenegro", "Andorra", "Estonia", "Albania",
              "Bosnia & Herzegovina", "Switzerland", "Kosovo", "Serbia", "Czechia", 
              "Cyprus", "Slovakia", "Italy", "Georgia", "Bulgaria", "Romania", 
              "Ukraine", "Poland", "Liechtenstein", "Lithuania", "Latvia", 
              "North Macedonia", "Belarus", "Moldova", "San Marino", "Monaco", 
              "Turkey", "Armenia", "Azerbaijan"),
  Value = c(77.74, 75.73, 71.86, 70.82, 69.16, 68.27, 67.69, 67.03, 64.44, 62.36,
            60.10, 55.58, 54.41, 52.22, 47.22, 46.92, 46.48, 44.82, 44.28, 38.64, 
            34.81, 33.31, 33.24, 31.34, 30.94, 30.48, 29.68, 29.20, 28.95, 27.60, 
            26.67, 25.87, 23.15, 21.12, 19.00, 18.23, 17.87, 17.28, 17.12, 16.03, 
            13.35, 13.08, 12.32, 9.78, 8.60, 7.20, 6.40, 4.70),
  Year = 2017)

df_2016 <- data.frame(
  Country = c("Malta", "Belgium", "United Kingdom", "Spain", "Denmark", "Portugal",
              "Finland", "France", "Croatia", "Netherlands", "Norway", "Sweden", 
              "Austria", "Iceland", "Greece", "Germany", "Ireland", "Hungary", 
              "Luxembourg", "Montenegro", "Estonia", "Albania", "Switzerland", 
              "Andorra", "Serbia", "Cyprus", "Slovenia", "Czechia", "Kosovo", 
              "Georgia", "Bosnia & Herzegovina", "Slovakia", "Bulgaria", "Romania",
              "Italy", "Poland", "Liechtenstein", "Lithuania", "Latvia", 
              "North Macedonia", "San Marino", "Moldova", "Belarus", "Monaco", 
              "Turkey", "Armenia", "Azerbaijan"),
  Value = c(77.75, 81.85, 79.19, 70.95, 70.90, 69.55, 67.25, 66.60, 66.55, 66.10, 
            65.15, 64.85, 62.21, 59.00, 58.30, 55.14, 54.70, 51.40, 50.35, 45.20, 
            38.25, 34.40, 33.15, 32.10, 32.00, 31.95, 31.65, 31.60, 31.55, 30.35, 
            29.40, 29.20, 24.00, 23.45, 19.75, 18.30, 18.20, 18.10, 17.35, 15.55, 
            14.40, 14.15, 13.35, 10.80, 8.75, 7.20, 4.85),
  Year = 2016)

df_2015 <- data.frame(
  Country = c("United Kingdom", "Belgium", "Malta", "Sweden", "Croatia", "Netherlands",
              "Norway", "Spain", "Denmark", "Portugal", "France", "Iceland", "Finland", 
              "Germany", "Austria", "Hungary", "Montenegro", "Luxembourg", "Albania",
              "Ireland", "Greece", "Georgia", "Czechia", "Estonia", "Slovenia",
              "Andorra", "Bosnia & Herzegovina", "Serbia", "Slovakia", "Romania",
              "Switzerland", "Bulgaria", "Poland", "Italy", "Liechtenstein", 
              "Lithuania", "Cyprus", "Kosovo", "Latvia", "Moldova", "Belarus",
              "San Marino", "North Macedonia", "Turkey", "Monaco", "Armenia", 
              "Azerbaijan"),
  Value = c(88.00, 83.00, 77.00, 72.00, 71.00, 69.00, 69.00, 69.00, 68.00, 67.00,
            65.00, 63.00, 62.00, 56.00, 52.00, 50.00, 46.00, 43.00, 42.00, 40.00,
            39.00, 36.00, 35.00, 34.00, 32.00, 31.00, 29.00, 29.00, 29.00, 28.00, 
            28.00, 27.00, 26.00, 22.00, 19.00, 19.00, 18.00, 18.00, 18.00, 16.00,
            14.00, 14.00, 13.00, 12.00, 11.00, 9.00, 5.00),
  Year = 2015)

df_2014 <- data.frame(
  Country = c("United Kingdom", "Belgium", "Spain", "Netherlands", "Norway", 
              "Portugal", "Sweden", "France", "Iceland", "Denmark", "Malta", 
              "Croatia", "Germany", "Hungary", "Austria", "Montenegro", "Finland",
              "Albania", "Slovenia", "Czechia", "Estonia", "Ireland", "Greece", 
              "Slovakia", "Serbia", "Bulgaria", "Switzerland", "Luxembourg", 
              "Romania", "Poland", "Italy", "Georgia", "Lithuania", "Andorra",
              "Bosnia & Herzegovina", "Cyprus", "Latvia", "Liechtenstein", "Kosovo",
              "Moldova", "Turkey", "San Marino", "Belarus", "North Macedonia", 
              "Ukraine", "Monaco", "Armenia", "Azerbaijan"),
  Value = c(80.25, 78.10, 73.26, 69.90, 68.40, 66.60, 65.30, 64.10, 63.95, 59.90,
            56.80, 56.30, 55.68, 53.65, 52.10, 47.05, 45.30, 38.40, 35.00, 34.65, 
            34.65, 33.65, 31.15, 30.50, 30.30, 30.00, 28.85, 28.35, 27.95, 27.65, 
            27.40, 28.05, 21.70, 20.60, 20.10, 19.65, 19.65, 18.00, 17.10, 16.50,
            14.15, 13.70, 13.60, 13.30, 11.65, 10.10, 8.50, 6.60),
  Year = 2014)

df_2013 <- data.frame(
  Country = c("United Kingdom", "Belgium", "Norway", "Sweden", "Spain", "Portugal", 
              "France", "Netherlands", "Denmark", "Iceland", "Hungary", "Germany",
              "Croatia", "Finland", "Austria", "Albania", "Malta", "Slovenia", 
              "Czechia", "Ireland", "Romania", "Estonia", "Switzerland", "Luxembourg",
              "Greece", "Slovakia", "Montenegro", "Serbia", "Poland", "Georgia",
              "Lithuania", "Andorra", "Bosnia & Herzegovina", "Cyprus", "Latvia", 
              "Italy", "Bulgaria", "Liechtenstein", "Turkey", "San Marino", "Belarus",
              "Kosovo", "North Macedonia", "Ukraine", "Monaco", "Armenia", "Azerbaijan"),
  Value = c(78.50, 68.73, 65.65, 65.30, 65.04, 64.60, 64.10, 60.00, 59.80, 55.50,
            54.70, 54.29, 48.30, 47.25, 43.35, 38.40, 35.30, 35.00, 34.65, 33.65, 
            31.30, 28.90, 28.85, 28.35, 28.10, 28.90, 28.65, 25.05, 21.65, 21.05, 
            20.70, 20.60, 19.95, 19.65, 19.65, 19.40, 18.00, 15.50, 14.15, 13.70, 
            13.60, 13.50, 13.30, 11.65, 10.10, 7.50, 7.10),
  Year = 2013)

# combine all data frames into one
df_combined <- bind_rows(df_2019, df_2018, df_2017, df_2016, df_2015, df_2014, df_2013)

# create new, compressed df
# step 1: Filter data for 2019 and 2018
df_2019 <- df_combined %>% filter(Year == 2019) %>% rename(Value_2019 = Value)
df_2018 <- df_combined %>% filter(Year == 2018) %>% rename(Value_2018 = Value)

# step 2: Filter data for 2013 and 2014 and calculate the average
df_2013_2014 <- df_combined %>% filter(Year %in% c(2013, 2014)) %>%
  group_by(Country) %>%
  summarise(Avg_2013_2014 = mean(Value, na.rm = TRUE))

# step 3: Join the data frames for 2019 and 2018
df_compressed <- df_2019 %>%
  left_join(df_2018, by = "Country") %>%
  select(Country, Value_2019, Value_2018)

# step 4: Calculate the average for 2019 and 2018
df_compressed <- df_compressed %>%
  mutate(Avg_2019_2018 = (Value_2019 + Value_2018) / 2)

# step 5: Join the average for 2013 and 2014
df_compressed <- df_compressed %>%
  left_join(df_2013_2014, by = "Country")

# step 6: Calculate the difference between the averages
df_compressed <- df_compressed %>%
  mutate(Difference = Avg_2019_2018 - Avg_2013_2014)

# step 7: Select and reorder columns for the final compressed data frame
df_compressed <- df_compressed %>%
  select(Country, Value_2019, Value_2018, Avg_2019_2018, 
         Avg_2013_2014, Difference)

rainbow_df <- df_compressed

# save the data frame
saveRDS(rainbow_df, file = "rainbow_df.rds")
write.csv(rainbow_df, "rainbow_df.csv")



# 3. The Economist: Democracy scores 2018 ---------------------------------
# https://enperspectiva.uy/wp-content/uploads/2019/01/Democracy_Index_2018.pdf
democracy_scores <- data.frame(
  Country = c("Belgium", "Denmark", "Greece", "Spain", "Finland", "France", "Ireland", "Italy", "Luxembourg", "Netherlands", "Austria", "Portugal", "Sweden", "Germany", "United Kingdom", "Bulgaria", "Cyprus", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania", "Malta", "Poland", "Romania", "Slovakia", "Slovenia", "Croatia"),
  ISO2 = c("BE", "DK", "GR", "ES", "FI", "FR", "IE", "IT", "LU", "NL", "AT", "PT", "SE", "DE", "GB", "BG", "CY", "CZ", "EE", "HU", "LV", "LT", "MT", "PL", "RO", "SK", "SI", "HR"),
  Overall_score = c(7.78, 9.22, 7.29, 8.08, 9.14, 7.80, 9.15, 7.71, 8.81, 8.89, 8.29, 7.84, 9.39, 8.68, 8.53, 7.03, 7.59, 7.69, 7.97, 6.63, 7.38, 7.50, 8.21, 6.67, 6.38, 7.10, 7.50, 6.57),
  #Global_rank = c(31, 5, 39, 19, 8, 29, "6=", 33, 12, 11, 16, 27, 3, 13, 14, 46, 35, 34, "23=", "57=", 38, "36=", 18, "54=", "66=", 44, "36=", 60),
  #Regional_rank = c(17, 4, 20, 14, 6, 16, 5, 18, 9, 8, 12, 15, 3, 10, 11, 7, 19, 2, 1, 9, 5, "3=", 13, 8, 12, 6, "3=", 10),
  Electoral_process_and_pluralism = c(9.58, 10.00, 9.58, 9.17, 10.00, 9.58, 9.58, 9.58, 10.00, 9.58, 9.58, 9.58, 9.58, 9.58, 9.58, 9.17, 9.17, 9.58, 9.58, 8.75, 9.58, 9.58, 9.17, 9.17, 9.17, 9.58, 9.58, 9.17),
  Functioning_of_government = c(8.93, 9.29, 5.36, 7.14, 8.93, 7.50, 7.86, 6.07, 8.93, 9.29, 7.86, 7.50, 9.64, 8.57, 7.50, 6.43, 6.43, 6.79, 8.21, 6.07, 6.07, 6.43, 8.21, 6.07, 5.71, 6.79, 6.79, 6.07),
  Political_participation = c(5.00, 8.33, 6.11, 7.78, 8.33, 7.78, 8.33, 7.78, 6.67, 8.33, 8.33, 6.11, 8.33, 8.33, 8.33, 7.22, 6.67, 6.67, 6.67, 5.00, 5.56, 6.11, 6.11, 6.11, 5.00, 5.56, 6.67, 5.56),
  Political_culture = c(6.88, 9.38, 6.88, 7.50, 8.75, 5.63, 10.00, 6.88, 8.75, 8.13, 6.88, 6.88, 10.00, 7.50, 8.13, 4.38, 6.88, 6.88, 6.88, 6.25, 6.88, 6.25, 8.75, 4.38, 4.38, 5.63, 6.25, 5.00),
  Civil_liberties = c(8.53, 9.12, 8.53, 8.82, 9.71, 8.53, 10.00, 8.24, 9.71, 9.12, 8.82, 9.12, 9.41, 9.41, 9.12, 7.94, 8.82, 8.53, 8.53, 7.06, 8.82, 9.12, 8.82, 7.65, 7.65, 7.94, 8.24, 7.06),
  Regime_type = c("Flawed democracy", "Full democracy", "Flawed democracy", "Full democracy", "Full democracy", "Flawed democracy", "Full democracy", "Flawed democracy", "Full democracy", "Full democracy", "Full democracy", "Flawed democracy", "Full democracy", "Full democracy", "Full democracy", "Flawed democracy", "Flawed democracy", "Flawed democracy", "Flawed democracy", "Flawed democracy", "Flawed democracy", "Flawed democracy", "Full democracy", "Flawed democracy", "Flawed democracy", "Flawed democracy", "Flawed democracy", "Flawed democracy"))

saveRDS(democracy_scores, file = "democracy_scores.rds")
write.csv(democracy_scores, "democracy_scores.csv")


# 4. Happiness data 2018 ---------------------------------------------------------------------
# https://s3.amazonaws.com/happiness-report/2018/WHR_web.pdf
happiness_scores <- data.frame(
  Country = c("Finland", "Denmark", "Greece", "Spain", "France", "Ireland", "Italy", "Luxembourg", "Netherlands", "Austria", "Portugal", "Sweden", "Germany", "United Kingdom", "Bulgaria", "Cyprus", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania", "Malta", "Poland", "Romania", "Slovakia", "Slovenia", "Croatia"),
  ISO2 = c("FI", "DK", "GR", "ES", "FR", "IE", "IT", "LU", "NL", "AT", "PT", "SE", "DE", "GB", "BG", "CY", "CZ", "EE", "HU", "LV", "LT", "MT", "PL", "RO", "SK", "SI", "HR"),
  Happiness_Score = c(7.632, 7.555, 5.358, 6.310, 6.489, 6.977, 6.000, 6.910, 7.441, 7.139, 5.410, 7.314, 6.965, 6.814, 4.933, 5.762, 6.711, 5.739, 5.620, 5.933, 5.952, 6.627, 6.123, 5.945, 6.173, 5.948, 5.321))

saveRDS(happiness_scores, file = "happiness_scores.rds")
write.csv(happiness_scores, "happiness_scores.csv")


# 5. GDP per capita ---------------------------------------------------------------------
df_GDP <- read_csv("data/raw/data_20250228194704.csv")
df_GDP <- df_GDP %>%
  select("CountryName", "PeriodCode", "Value") %>%
  filter(CountryName %in% country_mapping$country_name)

df_GDP <- df_GDP %>%
  mutate(Value = as.numeric(as.character(Value)))

df_GDP <- df_GDP %>%
  # group by country
  group_by(CountryName) %>% 
  # find first and last year values
  summarize(
    gdp_2005 = Value[PeriodCode == 2005],
    gdp_2018 = Value[PeriodCode == 2018],
    # calculate relative growth
    gdp_growth = (gdp_2018 - gdp_2005) / gdp_2005 * 100) %>%
  # add ISO2 codes for easier joining with other datasets
  left_join(country_mapping, by = c("CountryName" = "country_name")) %>%
  # select relevant columns
  select(CountryName, iso2, gdp_2005, gdp_2018, gdp_growth)

# add Greece GDP per capita data manually 
greece_gdp <- data.frame(
  CountryName = "Greece",
  iso2 = "GR",
  gdp_2005 = 22054,  # GDP per capita in 2005
  gdp_2018 = 19873,  # GDP per capita in 2018
  gdp_growth = ((19873 - 22054) / 22054) * 100)  # Calculate percent change

# append Greece to the GDP dataset
df_GDP <- rbind(df_GDP, greece_gdp)

saveRDS(df_GDP, file = "df_GDP.rds")
write.csv(df_GDP, "df_GDP.csv")


# 6. ESS Round 9 ----------------------------------------------------------
# https://ess.sikt.no/en/datafile/b2b0bf39-176b-4eca-8d26-3c05ea83d2cb
ess_data <- read_csv("data/raw/ESS9e03_2.csv")

# select interesting variables, country and weight variables
ess_selected <- ess_data %>%
  select(
    # identifiers and weights
    cntry,          # country code
    pspwght,        # post-stratification weight
    dweight,        # design weight
    
    # key variables of interest
    freehms,        # gays and lesbians free to live life as they wish
    lrscale,        # left-right political scale
    rlgdgr,         # how religious are you
    ipeqopt,        # important that people are treated equally
    atchctr,        # attachment to country
    eduyrs,         # years of education
    agea)            # age of respondent

# function to recode ESS special values (negative values are typically missing values)
recode_ess_missing <- function(x) {
  ifelse(x < 0, NA, x)
}

# clean the data
ess_clean <- ess_selected %>%
  # recode special values to NA
  mutate(across(c(freehms, lrscale, rlgdgr, ipeqopt, atchctr, eduyrs, agea), 
                recode_ess_missing)) %>%
  # create derived variables if needed
  mutate(
    # recode freehms to 0-1 scale (originally 1-5 where 1 = agree strongly)
    freehms_support = case_when(
      freehms %in% c(1, 2) ~ 1,  # agree and strongly agree
      freehms %in% c(3, 4, 5) ~ 0,  # neutral, disagree, strongly disagree
      TRUE ~ NA_real_),
    
    # create age groups
    age_group = case_when(
      agea < 35 ~ "18-34",
      agea < 55 ~ "35-54",
      TRUE ~ "55+"
    ),
    
    # standardise left-right scale to 0-1
    lrscale_std = (lrscale - 1) / 9,  # Original scale is 1-10
    
    # create high education indicator (above country median)
    high_educ = NA  # will fill this in after calculating country medians
  )

# calculate country median education for relative education measure
country_medians <- ess_clean %>%
  group_by(cntry) %>%
  summarize(median_educ = median(eduyrs, na.rm = TRUE))

# join back to main data and create high education indicator
ess_clean <- ess_clean %>%
  left_join(country_medians, by = "cntry") %>%
  mutate(high_educ = ifelse(eduyrs > median_educ, 1, 0))

# calculate weighted means by country
country_aggregates <- ess_clean %>%
  # group by country
  group_by(cntry) %>%
  # calculate weighted statistics
  summarize(
    # sample size
    n_respondents = n(),
    n_valid = sum(!is.na(freehms)),
    
    # weighted means
    pct_lgbt_support = weighted.mean(freehms_support, w = pspwght, na.rm = TRUE) * 100,
    mean_religiosity = weighted.mean(rlgdgr, w = pspwght, na.rm = TRUE),
    mean_left_right = weighted.mean(lrscale_std, w = pspwght, na.rm = TRUE),
    mean_equal_values = weighted.mean(ipeqopt, w = pspwght, na.rm = TRUE),
    mean_country_attach = weighted.mean(atchctr, w = pspwght, na.rm = TRUE),
    mean_eduyrs = weighted.mean(eduyrs, w = pspwght, na.rm = TRUE),
    mean_age = weighted.mean(agea, w = pspwght, na.rm = TRUE),
    
    # weighted proportions for categorical variables
    pct_young = weighted.mean(age_group == "18-34", w = pspwght, na.rm = TRUE) * 100,
    pct_high_educ = weighted.mean(high_educ, w = pspwght, na.rm = TRUE) * 100,
    
    # standard errors (for confidence intervals)
    se_lgbt_support = sd(freehms_support, na.rm = TRUE) / sqrt(sum(!is.na(freehms_support))),
    
    # missing data proportions
    pct_missing_lgbt = mean(is.na(freehms)) * 100)

# calculate cross-variable country indicators
country_indicators <- ess_clean %>%
  group_by(cntry) %>%
  summarize(
    # correlation between age and LGBT support within country
    age_lgbt_corr = cor(agea, freehms_support, use = "pairwise.complete.obs", method = "spearman"),
    
    # correlation between religiosity and LGBT support
    relig_lgbt_corr = cor(rlgdgr, freehms_support, use = "pairwise.complete.obs", method = "spearman"),
    
    # inequality in LGBT support (standard deviation)
    lgbt_support_inequality = sd(freehms_support, na.rm = TRUE),
    
    # education gradient in LGBT support (difference between high and low education)
    educ_gradient = weighted.mean(freehms_support[high_educ == 1], w = pspwght[high_educ == 1], na.rm = TRUE) - 
      weighted.mean(freehms_support[high_educ == 0], w = pspwght[high_educ == 0], na.rm = TRUE))

# join the aggregates and indicators
country_data_final <- country_aggregates %>%
  left_join(country_indicators, by = "cntry") %>%
  # create ISO country codes for easier merging with other datasets
  mutate(
    iso2c = countrycode(cntry, "iso2c", "iso2c"),
    iso3c = countrycode(cntry, "iso2c", "iso3c"))

# plot LGBT support by country
ggplot(country_data_final, aes(x = reorder(cntry, pct_lgbt_support), y = pct_lgbt_support)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = pct_lgbt_support - 1.96*se_lgbt_support, 
                    ymax = pct_lgbt_support + 1.96*se_lgbt_support), 
                width = 0.2) +
  labs(title = "Support for LGBT Rights by Country",
       subtitle = "Percent agreeing gays and lesbians should be free to live as they wish",
       x = "Country",
       y = "Support (%)") +
  theme_minimal() +
  coord_flip()

# examine relationship between religiosity and LGBT support
ggplot(country_data_final, aes(x = mean_religiosity, y = pct_lgbt_support, label = cntry)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(hjust = -0.3, vjust = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Religiosity vs. LGBT Support by Country",
       x = "Mean Religiosity Score",
       y = "LGBT Support (%)") +
  theme_minimal()

saveRDS(country_data_final, file = "country_data_final.rds")
write.csv(country_data_final, "country_data_final.csv")


# adjust the country codes to match those in the Eurobarometer dataset


# 7. Unemployment rate ----------------------------------------------------
# https://en.wikipedia.org/wiki/List_of_European_Union_member_states_by_unemployment_rate

# scrape data from Wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_European_Union_member_states_by_unemployment_rate"
page <- read_html(url)
tables <- html_table(page, fill = TRUE)
eu_unemployment_table <- tables[[1]]

# clean column names - simplify them to more standard names
colnames(eu_unemployment_table) <- c("Country", "Unemployment", "Employment", "Year")

# clean up the country names by removing footnote references
eu_unemployment_table$Country <- gsub("\\[.*?\\]", "", eu_unemployment_table$Country)

# make sure numeric columns are properly formatted
eu_unemployment_table$Unemployment <- as.character(eu_unemployment_table$Unemployment)
eu_unemployment_table$Employment <- as.numeric(as.character(eu_unemployment_table$Employment))
eu_unemployment_table$Year <- as.numeric(as.character(eu_unemployment_table$Year))

eu_unemployment_table <- eu_unemployment_table %>%
  mutate(
    # trim spaces from country names
    Country = trimws(Country),
    # convert Unemployment to numeric (remove any % signs or spaces if present)
    Unemployment = as.numeric(gsub("[^0-9.]", "", Unemployment)))

# modify Greece's unemployment rate to the 2018 value
eu_unemployment_table$Unemployment[eu_unemployment_table$Country == "Greece"] <- 19.18
eu_unemployment_table$Year[eu_unemployment_table$Country == "Greece"] <- 2018

# add the UK data manually
uk_data <- data.frame(
  Country = "United Kingdom",
  Unemployment = 4.12, # from Statista
  Employment = 75.25,  # estimated from https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/singlemonthlabourforcesurveyestimates/december2018
  Year = 2018)

# append UK data to the table
eu_unemployment_table <- rbind(eu_unemployment_table, uk_data)

# ignore the year column
eu_unemployment_table <- eu_unemployment_table %>%
  select(-Year)

# save
saveRDS(eu_unemployment_table, file = "eu_unemployment_table.rds")
write.csv(eu_unemployment_table, "eu_unemployment_table.csv", row.names = FALSE)


# Merge the data ----------------------------------------------------------
# create a base dataframe with country identifying variables
country_level_df <- country_mapping

# merge V-Dem data
country_level_df <- country_level_df %>%
  left_join(vdem_eu_2019, by = c("country_name" = "country_name"))

# fix country name in democracy_scores for Czech Republic if needed
if(any(democracy_scores$Country == "Czechia")) {
  democracy_scores$Country[democracy_scores$Country == "Czechia"] <- "Czech Republic"
}

# merge democracy scores
country_level_df <- country_level_df %>%
  left_join(democracy_scores, by = c("iso2" = "ISO2"))

# merge GDP data
country_level_df <- country_level_df %>%
  left_join(df_GDP, by = "iso2")

# rename some countries to match our country_name format
rainbow_country_mapping <- data.frame(
  original = c("Czechia", "Andorra", "Bosnia & Herzegovina", "North Macedonia", "United Kingdom"),
  standardized = c("Czech Republic", "Andorra", "Bosnia and Herzegovina", "Macedonia", "United Kingdom"),
  stringsAsFactors = FALSE)

# apply standardized country names
for(i in 1:nrow(rainbow_country_mapping)) {
  rainbow_df$Country[rainbow_df$Country == rainbow_country_mapping$original[i]] <- 
    rainbow_country_mapping$standardized[i]
}

# extract and rename rainbow map variables for clarity
rainbow_data_clean <- rainbow_df %>%
  select(
    Country,
    rainbow_score_2019 = Value_2019,
    rainbow_score_2018 = Value_2018,
    rainbow_score_avg_2019_2018 = Avg_2019_2018,
    rainbow_score_avg_2013_2014 = Avg_2013_2014,
    rainbow_score_difference = Difference)

# merge Rainbow Map data
country_level_df <- country_level_df %>%
  left_join(rainbow_data_clean, by = c("country_name" = "Country"))

# merge happiness data
country_level_df <- country_level_df %>%
  left_join(happiness_scores, by = c("iso2" = "ISO2"))

# merge unemployment data
country_level_df <- country_level_df %>%
  left_join(eu_unemployment_table, by = c("country_name" = "Country"))

# create a mapping between ESS country codes and ISO2
ess_country_mapping <- data.frame(
  cntry = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", 
            "DE", "HU", "IE", "IT", "LV", "LT", "NL", "PL", "PT", "RO", 
            "SK", "SI", "ES", "SE", "GB"),
  iso2 = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", 
           "DE", "HU", "IE", "IT", "LV", "LT", "NL", "PL", "PT", "RO", 
           "SK", "SI", "ES", "SE", "GB"),
  stringsAsFactors = FALSE)

# first ensure cntry codes match our iso2 codes
country_data_final <- country_data_final %>%
  left_join(ess_country_mapping, by = "cntry") %>%
  select(-iso2c, -iso3c) # remove original ISO codes to avoid confusion

# rename variables for clarity
ess_data_clean <- country_data_final %>%
  select(
    cntry,
    n_respondents,
    n_valid,
    lgbt_support_percent = pct_lgbt_support,
    mean_religiosity,
    mean_left_right,
    mean_equal_values,
    mean_country_attach,
    mean_eduyrs,
    mean_age,
    pct_young,
    pct_high_educ,
    se_lgbt_support,
    pct_missing_lgbt,
    age_lgbt_corr,
    relig_lgbt_corr,
    lgbt_support_inequality,
    educ_gradient,
    #iso3c
  )

# merge ESS data
country_level_df <- country_level_df %>%
  left_join(ess_data_clean, by = c("iso2" = "cntry"))

country_level_df %>% View()

# delete the first Greece row
greece_rows <- which(country_level_df$country_name == "Greece")

if (length(greece_rows) > 1) {
  # remove the first instance of Greece
  country_level_df <- country_level_df[-greece_rows[1], ]
  
  # verify the fix worked
  greece_check <- country_level_df %>%
    filter(country_name == "Greece")
  print("Greece entries after removing the first instance:")
  print(greece_check)
}

# save
saveRDS(country_level_df, file = "country_level_df.rds")
write.csv(country_level_df, "country_level_df.csv")





