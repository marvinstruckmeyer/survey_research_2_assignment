# Essential EDA for Survey Data
library(tidyverse)
library(naniar)     # For missing data visualization
library(GGally)     # For correlation matrix
library(scales)     # For percentage scales
library(viridis)    # For colorblind-friendly palettes

# Load the data
df <- readRDS("df_reduced.rds")

# Filter to just individual-level variables
individual_vars <- df %>%
  select(
    # target variable
    qc19,
    
    # individual demographics
    d11, d10, d8, d8r2, d15a, d25, d60, d63,
    
    # values and identity
    sd3, d1, sd2_5,
    qc15_1, qc15_2, qc15_3,
    qc6_10, qc6_10r,
    qc12_10, qc12_10r,
    qc13_10, qc13_10r,
    sd1_4, sd1_5, sd1_7, sd1_8,
    qc18_1, qc18_2, qc18_3,
    
    # discrimination perceptions
    qc1_4, qc1_7, qc1_8, qc1_9, qc1_10,
    qc2_4, qc2_5, qc2_6, qc2_7,
    
    # paradata
    p1, p2, p3, p3r, p4, p5,
    
    # keep country identifiers for reference
    country, country_name, isocntry
  )

# Add target variable labels
individual_vars <- individual_vars %>%
  mutate(qc19_label = case_when(
    qc19 == 1 ~ "Yes",
    qc19 == 2 ~ "No",
    qc19 == 3 ~ "Don't Know",
    TRUE ~ NA_character_
  ))

# Basic dimensions
dim(individual_vars)

## Descriptive analysis and preprocessing

### Check feature distributions
# Plot histograms of key variables
key_vars <- c("qc19", "d11", "d8", "d1", "qc15_1", "sd1_4")

df_long <- individual_vars %>% 
  select(all_of(key_vars)) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~feature, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Key Features")

### Target variable distribution
ggplot(individual_vars, aes(x = qc19_label)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Target Variable (qc19)",
       x = NULL,
       y = "Count")

### Key demographics by target variable
# Age and target (filtered to remove special codes)
ggplot(individual_vars %>% filter(d11 < 99), 
       aes(x = qc19_label, y = d11)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Age Distribution by Target Response",
       x = "Response",
       y = "Age (d11)")

# Education and target
ggplot(individual_vars, aes(x = factor(d8), fill = qc19_label)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "Target Variable by Education Level",
       x = "Education Code (d8)",
       y = "Proportion",
       fill = "Response") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Values and identity factors
# Political ideology and target (filtered to remove special codes)
ggplot(individual_vars %>% filter(d1 < 98), 
       aes(x = factor(d1), fill = qc19_label)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "Target Variable by Political Ideology",
       x = "Political Ideology Code (d1)",
       y = "Proportion",
       fill = "Response") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### LGBT-specific factors
# Having LGBT friends and target
ggplot(individual_vars, aes(x = factor(sd1_4), fill = qc19_label)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "Target Variable by Having LGBT Friends",
       x = "Has LGBT Friends Code (sd1_4)",
       y = "Proportion",
       fill = "Response")

# Support for LGBT rights and target
ggplot(individual_vars, aes(x = factor(qc15_1), fill = qc19_label)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "Target Variable by LGBT Rights Support",
       x = "Support Level Code (qc15_1)",
       y = "Proportion",
       fill = "Response")

### Geographic factors
# Target variable across countries (top 10 by sample size)
top_countries <- individual_vars %>%
  count(country_name) %>%
  top_n(10, n) %>%
  pull(country_name)

individual_vars %>%
  filter(country_name %in% top_countries) %>%
  ggplot(aes(x = reorder(country_name, qc19, FUN = function(x) mean(x == 1)), 
             fill = qc19_label)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Target Variable by Country",
       subtitle = "Top 10 countries by sample size",
       x = NULL,
       y = "Proportion",
       fill = "Response")

### Check correlation
# Select numerical variables and filter out special codes
numeric_vars <- individual_vars %>%
  select(qc19, d11, d8, d25, d60, d63, sd3, d1, 
         qc15_1, qc15_2, qc15_3, qc6_10, qc12_10, qc13_10, sd1_4) %>%
  filter(d1 < 98, d11 < 99)

# Create correlation matrix
ggcorr(numeric_vars, label = TRUE, label_size = 3, hjust = 0.8) +
  labs(title = "Correlation Matrix of Key Variables") +
  theme(title = element_text(size = 14))

### Identify key predictors
# Calculate correlations with target
correlations <- cor(numeric_vars$qc19, numeric_vars, use = "pairwise.complete.obs")
cor_df <- data.frame(
  variable = colnames(numeric_vars),
  correlation = as.numeric(correlations)
)

# Plot top correlations
ggplot(cor_df %>% 
         filter(variable != "qc19") %>% 
         top_n(10, abs(correlation)),
       aes(x = reorder(variable, abs(correlation)), y = correlation)) +
  geom_col(aes(fill = correlation > 0)) +
  scale_fill_manual(values = c("firebrick", "steelblue"),
                    labels = c("Negative", "Positive")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Correlations with Target Variable",
       subtitle = "Key predictors for modeling",
       x = NULL,
       y = "Correlation Coefficient",
       fill = "Direction")