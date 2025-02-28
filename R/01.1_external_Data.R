library(tidyverse)

###
devtools::install_github("vdeminstitute/vdemdata")

### rainbowmap: https://rainbowmap.ilga-europe.org/
rainbow_data <- read_csv("data/raw/2024-rainbow-map-data.csv")

# but the problem: that's for 2024, not 2019 or before 2019
# hence, we would need to get the data for 2019 and years before:
# Load necessary library
library(dplyr)

# Create data frames for each year
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
  Country = c("Malta", "Norway", "United Kingdom", "Belgium", "France", "Portugal", "Finland", "Denmark", "Spain", "Netherlands", "Croatia", "Sweden", "Austria", "Germany", "Ireland", "Iceland", "Greece", "Luxembourg", "Hungary", "Slovenia", "Montenegro", "Andorra", "Estonia", "Albania", "Bosnia & Herzegovina", "Switzerland", "Kosovo", "Serbia", "Czechia", "Cyprus", "Slovakia", "Italy", "Georgia", "Bulgaria", "Romania", "Ukraine", "Poland", "Liechtenstein", "Lithuania", "Latvia", "North Macedonia", "Belarus", "Moldova", "San Marino", "Monaco", "Turkey", "Armenia", "Azerbaijan"),
  Value = c(77.74, 75.73, 71.86, 70.82, 69.16, 68.27, 67.69, 67.03, 64.44, 62.36, 60.10, 55.58, 54.41, 52.22, 47.22, 46.92, 46.48, 44.82, 44.28, 38.64, 34.81, 33.31, 33.24, 31.34, 30.94, 30.48, 29.68, 29.20, 28.95, 27.60, 26.67, 25.87, 23.15, 21.12, 19.00, 18.23, 17.87, 17.28, 17.12, 16.03, 13.35, 13.08, 12.32, 9.78, 8.60, 7.20, 6.40, 4.70),
  Year = 2017
)

df_2016 <- data.frame(
  Country = c("Malta", "Belgium", "United Kingdom", "Spain", "Denmark", "Portugal", "Finland", "France", "Croatia", "Netherlands", "Norway", "Sweden", "Austria", "Iceland", "Greece", "Germany", "Ireland", "Hungary", "Luxembourg", "Montenegro", "Estonia", "Albania", "Switzerland", "Andorra", "Serbia", "Cyprus", "Slovenia", "Czechia", "Kosovo", "Georgia", "Bosnia & Herzegovina", "Slovakia", "Bulgaria", "Romania", "Italy", "Poland", "Liechtenstein", "Lithuania", "Latvia", "North Macedonia", "San Marino", "Moldova", "Belarus", "Monaco", "Turkey", "Armenia", "Azerbaijan"),
  Value = c(77.75, 81.85, 79.19, 70.95, 70.90, 69.55, 67.25, 66.60, 66.55, 66.10, 65.15, 64.85, 62.21, 59.00, 58.30, 55.14, 54.70, 51.40, 50.35, 45.20, 38.25, 34.40, 33.15, 32.10, 32.00, 31.95, 31.65, 31.60, 31.55, 30.35, 29.40, 29.20, 24.00, 23.45, 19.75, 18.30, 18.20, 18.10, 17.35, 15.55, 14.40, 14.15, 13.35, 10.80, 8.75, 7.20, 4.85),
  Year = 2016
)

df_2015 <- data.frame(
  Country = c("United Kingdom", "Belgium", "Malta", "Sweden", "Croatia", "Netherlands", "Norway", "Spain", "Denmark", "Portugal", "France", "Iceland", "Finland", "Germany", "Austria", "Hungary", "Montenegro", "Luxembourg", "Albania", "Ireland", "Greece", "Georgia", "Czechia", "Estonia", "Slovenia", "Andorra", "Bosnia & Herzegovina", "Serbia", "Slovakia", "Romania", "Switzerland", "Bulgaria", "Poland", "Italy", "Liechtenstein", "Lithuania", "Cyprus", "Kosovo", "Latvia", "Moldova", "Belarus", "San Marino", "North Macedonia", "Turkey", "Monaco", "Armenia", "Azerbaijan"),
  Value = c(88.00, 83.00, 77.00, 72.00, 71.00, 69.00, 69.00, 69.00, 68.00, 67.00, 65.00, 63.00, 62.00, 56.00, 52.00, 50.00, 46.00, 43.00, 42.00, 40.00, 39.00, 36.00, 35.00, 34.00, 32.00, 31.00, 29.00, 29.00, 29.00, 28.00, 28.00, 27.00, 26.00, 22.00, 19.00, 19.00, 18.00, 18.00, 18.00, 16.00, 14.00, 14.00, 13.00, 12.00, 11.00, 9.00, 5.00),
  Year = 2015
)

df_2014 <- data.frame(
  Country = c("United Kingdom", "Belgium", "Spain", "Netherlands", "Norway", "Portugal", "Sweden", "France", "Iceland", "Denmark", "Malta", "Croatia", "Germany", "Hungary", "Austria", "Montenegro", "Finland", "Albania", "Slovenia", "Czechia", "Estonia", "Ireland", "Greece", "Slovakia", "Serbia", "Bulgaria", "Switzerland", "Luxembourg", "Romania", "Poland", "Italy", "Georgia", "Lithuania", "Andorra", "Bosnia & Herzegovina", "Cyprus", "Latvia", "Liechtenstein", "Kosovo", "Moldova", "Turkey", "San Marino", "Belarus", "North Macedonia", "Ukraine", "Monaco", "Armenia", "Azerbaijan"),
  Value = c(80.25, 78.10, 73.26, 69.90, 68.40, 66.60, 65.30, 64.10, 63.95, 59.90, 56.80, 56.30, 55.68, 53.65, 52.10, 47.05, 45.30, 38.40, 35.00, 34.65, 34.65, 33.65, 31.15, 30.50, 30.30, 30.00, 28.85, 28.35, 27.95, 27.65, 27.40, 28.05, 21.70, 20.60, 20.10, 19.65, 19.65, 18.00, 17.10, 16.50, 14.15, 13.70, 13.60, 13.30, 11.65, 10.10, 8.50, 6.60),
  Year = 2014
)

df_2013 <- data.frame(
  Country = c("United Kingdom", "Belgium", "Norway", "Sweden", "Spain", "Portugal", "France", "Netherlands", "Denmark", "Iceland", "Hungary", "Germany", "Croatia", "Finland", "Austria", "Albania", "Malta", "Slovenia", "Czechia", "Ireland", "Romania", "Estonia", "Switzerland", "Luxembourg", "Greece", "Slovakia", "Montenegro", "Serbia", "Poland", "Georgia", "Lithuania", "Andorra", "Bosnia & Herzegovina", "Cyprus", "Latvia", "Italy", "Bulgaria", "Liechtenstein", "Turkey", "San Marino", "Belarus", "Kosovo", "North Macedonia", "Ukraine", "Monaco", "Armenia", "Azerbaijan"),
  Value = c(78.50, 68.73, 65.65, 65.30, 65.04, 64.60, 64.10, 60.00, 59.80, 55.50, 54.70, 54.29, 48.30, 47.25, 43.35, 38.40, 35.30, 35.00, 34.65, 33.65, 31.30, 28.90, 28.85, 28.35, 28.10, 28.90, 28.65, 25.05, 21.65, 21.05, 20.70, 20.60, 19.95, 19.65, 19.65, 19.40, 18.00, 15.50, 14.15, 13.70, 13.60, 13.50, 13.30, 11.65, 10.10, 7.50, 7.10),
  Year = 2013
)

# Combine all data frames into one
df_combined <- bind_rows(df_2019, df_2018, df_2017, df_2016, df_2015, df_2014, df_2013)

# Print the combined data frame
print(df_combined)