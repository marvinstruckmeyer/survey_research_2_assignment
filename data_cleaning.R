### data cleaning

library(tidyverse)
library(sjlabelled)
library(haven)
library(mice)

data <- read_dta("data/raw/ZA7575.dta")

### 
# code the questions properly:
# 'DK' and refusals are especially a problem for questions with ordinal variables, 
# less so for non-ordinal variables; for reasons of completeness, we clean all of
# the 'DK' and refusals (and some other special cases) by converting them to NAs

data_correctly_coded <- data %>%
  mutate(
    # hard cases for me
    d72_1 = ifelse(d72_1 %in% c(5,6), NA, d72_1),
    d72_2 = ifelse(d72_2 %in% c(5,6), NA, d72_2),
    d60 = ifelse(d60 == 7, NA, d60),
    d25 = ifelse(d25 == 8, NA, d25),
    d8 = ifelse(d8 > 70, NA, d8), # subjective decision that no one can have more than 70 years of ed (even full-time professors)
    d7 = ifelse(d7 %in% c(15,97), NA, d7),
    d1 = ifelse(d1 %in% c(97,98), NA, d1),
    
    qa16_1 = ifelse(qa16_1 == 5, NA, qa16_1),
    qa16_2 = ifelse(qa16_2 == 5, NA, qa16_2),
    qa16_3 = ifelse(qa16_3 == 5, NA, qa16_3),
    qa16_4 = ifelse(qa16_4 == 5, NA, qa16_4),
    
    d71_1 = ifelse(d71_1 == 4, NA, d71_1),
    d71_2 = ifelse(d71_2 == 4, NA, d71_2),
    d71_3 = ifelse(d71_3 == 4, NA, d71_3),
    #qb4_1 = ifelse(qb4_1 == 5, NA, qb4_1),
    #qb4_2 = ifelse(qb4_2 == 5, NA, qb4_2),
    #qb4_3 = ifelse(qb4_3 == 5, NA, qb4_3),
    #qb4_4 = ifelse(qb4_4 == 5, NA, qb4_4),
    
    qb3_1 = ifelse(qb3_1 == 5, NA, qb3_1),
    qb3_2 = ifelse(qb3_2 == 5, NA, qb3_2),
    qb3_3 = ifelse(qb3_3 == 5, NA, qb3_3),
    qb3_4 = ifelse(qb3_4 == 5, NA, qb3_4),
    qb3_5 = ifelse(qb3_5 == 5, NA, qb3_5),
    qb3_6 = ifelse(qb3_6 == 5, NA, qb3_6),
    qb3_7 = ifelse(qb3_7 == 5, NA, qb3_7),
    
    qb4_1 = ifelse(qb4_1 == 5, NA, qb4_1),
    qb4_2 = ifelse(qb4_2 == 5, NA, qb4_2),
    qb4_3 = ifelse(qb4_3 == 5, NA, qb4_3),
    qb4_4 = ifelse(qb4_4 == 5, NA, qb4_4),
    qb4_5 = ifelse(qb4_5 == 5, NA, qb4_5),
    
    qb5_1 = ifelse(qb5_1 == 5, NA, qb5_1),
    qb5_2 = ifelse(qb5_2 == 5, NA, qb5_2),
    qb5_3 = ifelse(qb5_3 == 5, NA, qb5_3),
    qb5_4 = ifelse(qb5_4 == 5, NA, qb5_4),
    
    sd1_1 = ifelse(sd1_1 %in% c(3,4), NA, sd1_1),
    sd1_2 = ifelse(sd1_2 %in% c(3,4), NA, sd1_2),
    sd1_3 = ifelse(sd1_3 %in% c(3,4), NA, sd1_3),
    sd1_4 = ifelse(sd1_4 %in% c(3,4), NA, sd1_4),
    sd1_5 = ifelse(sd1_5 %in% c(3,4), NA, sd1_5),
    sd1_6 = ifelse(sd1_6 %in% c(3,4), NA, sd1_6),
    sd1_7 = ifelse(sd1_7 %in% c(3,4), NA, sd1_7),
    sd1_8 = ifelse(sd1_8 %in% c(3,4), NA, sd1_8),
    
    qc1_1 = ifelse(qc1_1 == 6, NA, qc1_1),
    qc1_2 = ifelse(qc1_2 == 6, NA, qc1_2),
    qc1_3 = ifelse(qc1_3 == 6, NA, qc1_3),
    qc1_4 = ifelse(qc1_4 == 6, NA, qc1_4),
    qc1_5 = ifelse(qc1_5 == 6, NA, qc1_5),
    qc1_6 = ifelse(qc1_6 == 6, NA, qc1_6),
    qc1_7 = ifelse(qc1_7 == 6, NA, qc1_7),
    qc1_8 = ifelse(qc1_8 == 6, NA, qc1_8),
    qc1_9 = ifelse(qc1_9 == 6, NA, qc1_9),
    qc1_10 = ifelse(qc1_10 == 6, NA, qc1_10),
    
    qc5_1 = ifelse(qc5_1 == 3, NA, qc5_1),
    qc5_2 = ifelse(qc5_2 == 3, NA, qc5_2),
    qc5_3 = ifelse(qc5_3 == 3, NA, qc5_3),
    qc5_4 = ifelse(qc5_4 == 3, NA, qc5_4),
    
    qc6_1 = ifelse(qc6_1 == 12, NA, qc6_1),
    qc6_2 = ifelse(qc6_2 == 12, NA, qc6_2),
    qc6_3 = ifelse(qc6_3 == 12, NA, qc6_3),
    qc6_4 = ifelse(qc6_4 == 12, NA, qc6_4),
    qc6_5 = ifelse(qc6_5 == 12, NA, qc6_5),
    qc6_6 = ifelse(qc6_6 == 12, NA, qc6_6),
    qc6_7 = ifelse(qc6_7 == 12, NA, qc6_7),
    qc6_8 = ifelse(qc6_8 == 12, NA, qc6_8),
    qc6_9 = ifelse(qc6_9 == 12, NA, qc6_9),
    qc6_10 = ifelse(qc6_10 == 12, NA, qc6_10),
    qc6_11 = ifelse(qc6_11 == 12, NA, qc6_11),
    
    qc9_1 = ifelse(qc9_1 == 7, NA, qc9_1),
    qc9_2 = ifelse(qc9_2 == 7, NA, qc9_2),
    qc9_3 = ifelse(qc9_3 == 7, NA, qc9_3),
    qc9_4 = ifelse(qc9_4 == 7, NA, qc9_4),
    qc9_5 = ifelse(qc9_5 == 7, NA, qc9_5),
    qc9_6 = ifelse(qc9_6 == 7, NA, qc9_6),
    qc9_7 = ifelse(qc9_7 == 7, NA, qc9_7),
    qc9_8 = ifelse(qc9_8 == 7, NA, qc9_8),
    qc9_9 = ifelse(qc9_9 == 7, NA, qc9_9),
    qc9_10 = ifelse(qc9_10 == 7, NA, qc9_10),
    qc9_11 = ifelse(qc9_11 == 7, NA, qc9_11),
    
    qc11_1 = ifelse(qc11_1 == 6, NA, qc11_1),
    qc11_2 = ifelse(qc11_2 == 6, NA, qc11_2),
    qc11_3 = ifelse(qc11_3 == 6, NA, qc11_3),
    qc11_4 = ifelse(qc11_4 == 6, NA, qc11_4),
    qc11_5 = ifelse(qc11_5 == 6, NA, qc11_5),
    qc11_6 = ifelse(qc11_6 == 6, NA, qc11_6),
    
    qc12_1 = ifelse(qc12_1 %in% c(11,12,13), NA, qc12_1),
    qc12_2 = ifelse(qc12_2 %in% c(11,12,13), NA, qc12_2),
    qc12_3 = ifelse(qc12_3 %in% c(11,12,13), NA, qc12_3),
    qc12_4 = ifelse(qc12_4 %in% c(11,12,13), NA, qc12_4),
    qc12_5 = ifelse(qc12_5 %in% c(11,12,13), NA, qc12_5),
    qc12_6 = ifelse(qc12_6 %in% c(11,12,13), NA, qc12_6),
    qc12_7 = ifelse(qc12_7 %in% c(11,12,13), NA, qc12_7),
    qc12_8 = ifelse(qc12_8 %in% c(11,12,13), NA, qc12_8),
    qc12_9 = ifelse(qc12_9 %in% c(11,12,13), NA, qc12_9),
    qc12_10 = ifelse(qc12_10 %in% c(11,12,13), NA, qc12_10),
    qc12_11 = ifelse(qc12_11 %in% c(11,12,13), NA, qc12_11),
    qc12_12 = ifelse(qc12_12 %in% c(11,12,13), NA, qc12_12),
    qc12_13 = ifelse(qc12_13 %in% c(11,12,13), NA, qc12_13),
    qc12_14 = ifelse(qc12_14 %in% c(11,12,13), NA, qc12_14),
    qc12_15 = ifelse(qc12_14 %in% c(11,12,13), NA, qc12_15),
    
    qc13_1 = ifelse(qc13_1 %in% c(11,12,13), NA, qc13_1),
    qc13_2 = ifelse(qc13_2 %in% c(11,12,13), NA, qc13_2),
    qc13_3 = ifelse(qc13_3 %in% c(11,12,13), NA, qc13_3),
    qc13_4 = ifelse(qc13_4 %in% c(11,12,13), NA, qc13_4),
    qc13_5 = ifelse(qc13_5 %in% c(11,12,13), NA, qc13_5),
    qc13_6 = ifelse(qc13_6 %in% c(11,12,13), NA, qc13_6),
    qc13_7 = ifelse(qc13_7 %in% c(11,12,13), NA, qc13_7),
    qc13_8 = ifelse(qc13_8 %in% c(11,12,13), NA, qc13_8),
    qc13_9 = ifelse(qc13_9 %in% c(11,12,13), NA, qc13_9),
    qc13_10 = ifelse(qc13_10 %in% c(11,12,13), NA, qc13_10),
    qc13_11 = ifelse(qc13_11 %in% c(11,12,13), NA, qc13_11),
    qc13_12 = ifelse(qc13_12 %in% c(11,12,13), NA, qc13_12),
    qc13_13 = ifelse(qc13_13 %in% c(11,12,13), NA, qc13_13),
    qc13_14 = ifelse(qc13_14 %in% c(11,12,13), NA, qc13_14),
    qc13_15 = ifelse(qc13_14 %in% c(11,12,13), NA, qc13_15),
    
    qc15_1 = ifelse(qc15_1 == 5, NA, qc15_1),
    qc15_2 = ifelse(qc15_2 == 5, NA, qc15_2),
    qc15_3 = ifelse(qc15_3 == 5, NA, qc15_3),
    
    qc16_1 = ifelse(qc16_1 == 5, NA, qc16_1),
    
    qc17_1 = ifelse(qc17_1 == 5, NA, qc17_1),
    qc17_2 = ifelse(qc17_2 == 5, NA, qc17_2),
    qc17_3 = ifelse(qc17_3 == 5, NA, qc17_3),
    qc17_4 = ifelse(qc17_4 == 5, NA, qc17_4),
    qc17_5 = ifelse(qc17_5 == 5, NA, qc17_5),
    qc17_6 = ifelse(qc17_6 == 5, NA, qc17_6),
    qc17_7 = ifelse(qc17_7 == 5, NA, qc17_7),
    
    qc18_1 = ifelse(qc18_1 %in% c(11,12), NA, qc18_1),
    qc18_2 = ifelse(qc18_2 %in% c(11,12), NA, qc18_2),
    qc18_3 = ifelse(qc18_3 %in% c(11,12), NA, qc18_3),
    
    sd3 = ifelse(sd3 %in% c(15,16), NA, sd3),
    
    # easy cases for Claude
    #q1 = ifelse(q1 %in% c(29,30), NA, q1),
    
    qa1 = ifelse(qa1 == 5, NA, qa1),
    
    qa7 = ifelse(qa7 == 5, NA, qa7),
    
    qa8 = ifelse(qa8 == 4, NA, qa8),
    
    qa9 = ifelse(qa9 == 6, NA, qa9),
    
    qa14 = ifelse(qa14 == 6, NA, qa14),
    
    qa17 = ifelse(qa17 == 5, NA, qa17),
    
    qb6 = ifelse(qb6 == 4, NA, qb6),
    
    qb7 = ifelse(qb7 == 5, NA, qb7),
    
    qc19 = ifelse(qc19 == 3, NA, qc19),
    
    qc20 = ifelse(qc20 == 3, NA, qc20),
    
    d63 = ifelse(d63 %in% c(6,7,8,9), NA, d63), # manually because stupid Claude
    
    d77 = ifelse(d77 == 5, NA, d77),
    
    d70 = ifelse(d70 == 5, NA, d70),
    
    qa4a = ifelse(qa4a %in% c(7,8), NA, qa4a),
    
    qa5a = ifelse(qa5a %in% c(11,12,13), NA, qa5a),
    
    qa11 = ifelse(qa11 %in% c(4,5), NA, qa11),
    
    qa12 = ifelse(qa12 %in% c(5,6), NA, qa12),
    
    qa13 = ifelse(qa13 %in% c(6,7), NA, qa13),
    
    qa18a = ifelse(qa18a %in% c(7,8,9), NA, qa18a),
    
    qb8 = ifelse(qb8 == 5, NA, qb8),
    
    sd3 = ifelse(sd3 == 16, NA, sd3),
    
    qc3 = ifelse(qc3 %in% c(10,11), NA, qc3),
    
    qc7 = ifelse(qc7 %in% c(11,12), NA, qc7),
    
    qc8 = ifelse(qc8 %in% c(11,12), NA, qc8),
    
    qc10 = ifelse(qc10 %in% c(9,10), NA, qc10))

# write_rds(data_correctly_coded, file = "data_correctly_coded.rds")


###
# get rid of all the "r" coded ones
# for the questions with underscores, (1) if they are on a scale, e.g., 1-3 and 'DK'
# is one of them, replace 'DK'; if it's 0-1 coding, keep that (can't do anything about it)

data_correctly_coded <- data_correctly_coded %>%
  select(-matches("_r$|_r[0-9]$|_r[0-9]_")) 
# use setdiff() to check whether it's done properly 


### 
# now analyse the patterns of NAs across columns
missing_data_before <- data_correctly_coded %>%
  summarise(across(everything(), ~sum(is.na(.))/n())) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "missing_proportion") %>%
  arrange(desc(missing_proportion))

# view top variables with missing data
print(head(missing_data_before, 20))

# viz
ggplot(missing_data_before %>% filter(missing_proportion > 0.25), 
       aes(x = reorder(variable, missing_proportion), y = missing_proportion)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variables with >25% Missing Data",
       x = "Variable",
       y = "Proportion Missing") +
  theme_minimal()

# calculate overall proportion of missing data
mean(missing_data_before$missing_proportion)

# deeper analysis
missing_by_prefix <- missing_data_before %>%
  mutate(prefix = str_extract(variable, "^[a-z]+\\d+")) %>%
  group_by(prefix) %>%
  summarise(
    avg_missing = mean(missing_proportion),
    n_vars = n(),
    max_missing = max(missing_proportion),
    min_missing = min(missing_proportion)
  ) %>%
  arrange(desc(avg_missing))

print(head(missing_by_prefix, 15))

# we take out the following variables because (1) they exhibited high missingness
# while at the same aren't super releveant (that's an assumption we make) for our
# subsequent analysis
data_correctly_coded <- data_correctly_coded %>%
  select(
    # Exclude variables with specific prefixes
    -starts_with("p6"),   # Size of locality
    -starts_with("p7"),   # Region
    -starts_with("p13"),  # Language of interview
    -starts_with("d40"),  # Household size
    -starts_with("qa3"),  # Not benefitting from trade
    -starts_with("qa15"), # Countries bought goods from
    -starts_with("qa2"),  # Benefitting from trade
    -starts_with("qb8"),  # EU energy label influence
    -starts_with("qa18b"), # Information sources follow-up
    -starts_with("qc3")   # Discrimination circumstances
  )

# check how much we imporved the missingness rate
missing_data_after <- data_correctly_coded %>%
  summarise(across(everything(), ~sum(is.na(.))/n())) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "missing_proportion") %>%
  arrange(desc(missing_proportion))

mean(missing_data_after$missing_proportion)

# prepare for imputation
data_correctly_coded <-  data_correctly_coded %>%
  mutate(isocntry = as.factor(isocntry)) %>%
  # remove other problematic variables (like IDs) if needed
  select(!any_of(c("doi", "version", "caseid", "uniqid", "serialid"))) %>%
  sjlabelled::remove_all_labels()

# delete variables with zero variance as they cannot ever be useful predictors
var_zero <- data_correctly_coded %>% 
  select(where(is.numeric)) %>%
  summarise(across(everything(), var, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "variance") %>%
  filter(variance == 0 | is.na(variance))

if(nrow(var_zero) > 0) {
  cat("Removing", nrow(var_zero), "variables with zero variance\n")
  data_correctly_coded <- data_correctly_coded %>%
    select(!any_of(var_zero$variable))
}

data_correctly_coded <- data_correctly_coded %>%
  # convert country codes: collapse East and West Germany into one DE
  mutate(isocntry = case_when(
    isocntry %in% c("DE-W", "DE-E") ~ "DE",
    TRUE ~ isocntry))

###
# run the imputation
set.seed(1212)

start_time <- Sys.time()
imp_model <- mice(data_correctly_coded, m = 3, method = 'rf', maxit = 3)
end_time <- Sys.time()
end_time - start_time

df_rf_new <- complete(imp_model, action = 3)

# write_rds(df_rf_new, "df_rf_new.rds")

