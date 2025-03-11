# packages
library(tidyverse)
library(corrplot)
library(cluster)
library(factoextra)
library(ggrepel)

# first calculate country aggregates for individual variables so that we have
# continuous data for which we can more easily calculate correlations

df_reduced_aggregated <- df_reduced %>%
  group_by(country_name, isocntry) %>%
  summarize(
    
    # target
    pct_support_trans = mean(qc19 == 1, na.rm = TRUE) * 100,
    pct_oppose_trans = mean(qc19 == 2, na.rm = TRUE) * 100,
    pct_dk_trans = mean(qc19 == 3, na.rm = TRUE) * 100,
    
    # demographics
    mean_age = mean(age, na.rm = TRUE),
    pct_female = mean(gender == 2, na.rm = TRUE) * 100,
    mean_education_years = mean(education, na.rm = TRUE),
    pct_high_educ = mean(d8r2 >= 3, na.rm = TRUE) * 100,
    pct_rural = mean(urban_rural == 1, na.rm = TRUE) * 100,
    pct_urban = mean(urban_rural == 2, na.rm = TRUE) * 100,
    pct_large_urban = mean(urban_rural == 3, na.rm = TRUE) * 100,
    pct_financial_difficulty = mean(financial_insecurity <= 2, na.rm = TRUE) * 100,
    pct_working_class = mean(social_class == 1, na.rm = TRUE) * 100,
    pct_middle_class = mean(social_class %in% c(2,3,4), na.rm = TRUE) * 100,
    pct_upper_class = mean(social_class == 5, na.rm = TRUE) * 100,
    
    # values and identity
    mean_political_ideology = mean(political_ideology, na.rm = TRUE),
    pct_left = mean(political_ideology <= 3, na.rm = TRUE) * 100,
    pct_center = mean(political_ideology > 3 & political_ideology < 8, na.rm = TRUE) * 100,
    pct_right = mean(political_ideology >= 8, na.rm = TRUE) * 100,
    
    # religious composition
    pct_religious = mean(religion %in% c(1:11), na.rm = TRUE) * 100,
    pct_catholic = mean(religion == 1, na.rm = TRUE) * 100,
    pct_orthodox = mean(religion == 2, na.rm = TRUE) * 100,
    pct_protestant = mean(religion == 3, na.rm = TRUE) * 100,
    pct_other_christian = mean(religion == 4, na.rm = TRUE) * 100,
    pct_muslim = mean(religion %in% c(6:8), na.rm = TRUE) * 100,
    pct_other_religion = mean(religion %in% c(9:11), na.rm = TRUE) * 100,
    pct_atheist = mean(religion == 13, na.rm = TRUE) * 100,
    pct_nonbeliever = mean(religion == 14, na.rm = TRUE) * 100,
    pct_nonreligious = mean(religion %in% c(13,14), na.rm = TRUE) * 100,
    
    # social networks and contact
    pct_lgb_friends = mean(lgb_friends == 1, na.rm = TRUE) * 100,
    pct_trans_friends = mean(trans_friends == 1, na.rm = TRUE) * 100,
    
    # personal identity/minority status
    pct_ethnic_minority = mean(ethnic_minority == 1, na.rm = TRUE) * 100,
    pct_skin_color_minority = mean(skin_color_minority == 1, na.rm = TRUE) * 100,
    pct_religious_minority = mean(religious_minority == 1, na.rm = TRUE) * 100,
    pct_sexual_minority = mean(sexual_lgbt_minority == 1, na.rm = TRUE) * 100,
    pct_disability_minority = mean(disability_minority == 1, na.rm = TRUE) * 100,
    pct_other_minority = mean(other_minority == 1, na.rm = TRUE) * 100,
    pct_no_minority = mean(none_minority == 1, na.rm = TRUE) * 100,
    
    # discrimination perceptions; we need to be careful with interpretation/ coding
    mean_trans_discrim_country = mean(trans_discrimination_country, na.rm = TRUE),
    pct_perceive_trans_discrim = mean(trans_discrimination_country %in% c(1,2), na.rm = TRUE) * 100,
    pct_experienced_trans_discrim = mean(trans_discrimination_personal == 1, na.rm = TRUE) * 100,
    pct_workplace_trans_discrim = mean(trans_discrimination_workplace == 1, na.rm = TRUE) * 100,
    
    # political representation comfort - original scale (1-10, higher = more comfortable)
    mean_trans_political_comfort = mean(trans_discrimination_political, na.rm = TRUE),
    pct_comfortable_trans_political = mean(trans_discrimination_political >= 7, na.rm = TRUE) * 100,
    
    # effectiveness of discrimination efforts (original scale: 1-10, higher = more effective)
    mean_country_discrimination_efforts = mean(country_discrimination_efforts, na.rm = TRUE),
    
    # workplace diversity (1=Yes definitely, 4=No definitely not)
    mean_trans_workplace_diversity = mean(trans_workplace_diversity, na.rm = TRUE),
    pct_support_trans_workplace_diversity = mean(trans_workplace_diversity %in% c(1,2), na.rm = TRUE) * 100,
    
    # LGBT attitudes - REVERSED for these scales where 1=Totally agree, 4=Totally disagree
    # Create reversed versions so higher = more supportive
    mean_lgb_rights_rev = mean(case_when(
      lgb_rights == 1 ~ 4,
      lgb_rights == 2 ~ 3,
      lgb_rights == 3 ~ 2,
      lgb_rights == 4 ~ 1,
      TRUE ~ NA_real_), na.rm = TRUE),
    
    mean_same_sex_relationship_rev = mean(case_when(
      same_sex_relationship == 1 ~ 4,
      same_sex_relationship == 2 ~ 3,
      same_sex_relationship == 3 ~ 2,
      same_sex_relationship == 4 ~ 1,
      TRUE ~ NA_real_), na.rm = TRUE),
    
    mean_same_sex_marriage_rev = mean(case_when(
      same_sex_marriage == 1 ~ 4,
      same_sex_marriage == 2 ~ 3,
      same_sex_marriage == 3 ~ 2,
      same_sex_marriage == 4 ~ 1,
      TRUE ~ NA_real_), na.rm = TRUE),
    
    # also calculate percentage who agree with LGBT rights (original values 1 or 2)
    pct_support_lgb_rights = mean(lgb_rights %in% c(1,2), na.rm = TRUE) * 100,
    pct_support_same_sex_relationship = mean(same_sex_relationship %in% c(1,2), na.rm = TRUE) * 100,
    pct_support_same_sex_marriage = mean(same_sex_marriage %in% c(1,2), na.rm = TRUE) * 100,
    
    # school materials (1=Totally agree, 4=Totally disagree)
    # Create reversed versions so higher = more supportive
    mean_lgb_school_materials_rev = mean(case_when(
      lgb_school_materials == 1 ~ 4,
      lgb_school_materials == 2 ~ 3,
      lgb_school_materials == 3 ~ 2,
      lgb_school_materials == 4 ~ 1,
      TRUE ~ NA_real_), na.rm = TRUE),
    
    mean_trans_school_materials_rev = mean(case_when(
      trans_school_materials == 1 ~ 4,
      trans_school_materials == 2 ~ 3,
      trans_school_materials == 3 ~ 2,
      trans_school_materials == 4 ~ 1,
      TRUE ~ NA_real_), na.rm = TRUE),
    
    mean_intersex_school_materials_rev = mean(case_when(
      intersex_school_materials == 1 ~ 4,
      intersex_school_materials == 2 ~ 3,
      intersex_school_materials == 3 ~ 2,
      intersex_school_materials == 4 ~ 1,
      TRUE ~ NA_real_), na.rm = TRUE),
    
    # also calculate percentage who agree with inclusive school materials
    pct_support_lgb_school_materials = mean(lgb_school_materials %in% c(1,2), na.rm = TRUE) * 100,
    pct_support_trans_school_materials = mean(trans_school_materials %in% c(1,2), na.rm = TRUE) * 100,
    pct_support_intersex_school_materials = mean(intersex_school_materials %in% c(1,2), na.rm = TRUE) * 100,
    
    # comfort with same-sex public displays of affection (1-10)
    mean_men_pda_comfort = mean(two_men_public_affection, na.rm = TRUE),
    mean_women_pda_comfort = mean(two_women_public_affection, na.rm = TRUE),
    pct_comfortable_men_pda = mean(two_men_public_affection >= 7, na.rm = TRUE) * 100,
    pct_comfortable_women_pda = mean(two_women_public_affection >= 7, na.rm = TRUE) * 100,
    
    # comfort with transgender colleagues/children (1-10)
    mean_trans_colleague_comfort = mean(trans_colleague, na.rm = TRUE),
    mean_trans_child_relationship = mean(trans_child_relationship, na.rm = TRUE),
    pct_comfortable_trans_colleague = mean(trans_colleague >= 7, na.rm = TRUE) * 100,
    pct_comfortable_trans_child = mean(trans_child_relationship >= 7, na.rm = TRUE) * 100,
    
    # support for non-gendered documents
    pct_support_nongendered_docs = mean(non_gendered_docs == 1, na.rm = TRUE) * 100,
    pct_oppose_nongendered_docs = mean(non_gendered_docs == 2, na.rm = TRUE) * 100,
    pct_dk_nongendered_docs = mean(non_gendered_docs == 3, na.rm = TRUE) * 100) %>%
  ungroup()

# add country-level variables from the original dataset
# extract country-level variables that don't need to be aggregated
country_level_variables <- df_reduced %>%
  select(country_name, gdp_2018, rainbow_score_2019, rainbow_score_2018, 
         gender_equality_index, Happiness_Score, 
         v2x_libdem, v2x_egaldem, Regime_type, composite_equality, 
         z_composite_equality, norm_composite_equality, z_lgbt_support, 
         norm_lgbt_support, pct_friends_lgbt, z_pct_friends_lgbt, 
         norm_pct_friends_lgbt, mean_left_right, pct_high_education, region) %>%
  distinct()

# merge with aggregated data
df_reduced_aggregated_complete <- df_reduced_aggregated %>%
  left_join(country_level_vars, by = "country_name")

# what we could add
setdiff(colnames(country_data_combined), country_level_variables)
country_data_combined_add <- country_data_combined %>%
  select(iso2, norm_gdp_growth, norm_Unemployment, pct_trade_support, norm_gdp_2018)

df_reduced_aggregated_complete <- df_reduced_aggregated_complete %>%
  left_join(country_data_combined_add, by = c("isocntry" = "iso2"))

# Check the final dataset
dim(df_reduced_aggregated_complete)


### create different plots
# calculate correlation matrix for numeric variables
cor_matrix <- cor(select_if(df_reduced_aggregated_complete, is.numeric), 
                  use = "pairwise.complete.obs")

# (1) visualize complete correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", diag = F,
         tl.col = "black", tl.srt = 45, 
         number.cex = 0.7, tl.cex = 0.7)

# (2) create custom function to analyze correlations with outcome
analyze_correlations_with_outcome <- function(data, outcome_var, min_correlation = 0.3) {
  # numeric columns only
  numeric_data <- data %>% select(where(is.numeric))
  
  # calculate correlations with outcome
  cors <- cor(numeric_data, numeric_data[[outcome_var]], use = "pairwise.complete.obs")
  
  # convert to data frame
  cor_df <- data.frame(
    variable = rownames(cors),
    correlation = cors[,1]) %>%
    # remove the outcome variable correlating with itself
    filter(variable != outcome_var) %>%
    # sort by absolute correlation
    arrange(desc(abs(correlation)))
  
  # filter by minimum correlation threshold
  cor_df <- cor_df %>% filter(abs(correlation) >= min_correlation)
  
  return(cor_df)
}

# run correlation analysis for transgender support
trans_support_correlations <- analyze_correlations_with_outcome(
  df_reduced_aggregated_complete, "pct_support_trans", min_correlation = 0.25)

# display results
print(trans_support_correlations)

# create a visualization of top correlations
ggplot(trans_support_correlations %>% head(15), 
       aes(x = reorder(variable, correlation), y = correlation)) +
  geom_bar(stat = "identity", aes(fill = correlation > 0)) +
  coord_flip() +
  labs(title = "Top correlated variables of transgender rights support",
       subtitle = "Country-level aggregated data",
       x = "Variables",
       y = "Correlation with % supporting transgender document changes") +
  theme_minimal() +
  scale_fill_manual(values = c("firebrick", "steelblue"), 
                    name = "Direction",
                    labels = c("Negative", "Positive"))

# (3) create correlation matrix for key variables
key_vars <- c("pct_support_trans", 
              trans_support_correlations$variable[1:10], # top 10 correlates
              "gdp_2018", "rainbow_score_2019", "gender_equality_index")

key_cor_matrix <- cor(df_reduced_aggregated_complete[, key_vars], 
                      use = "pairwise.complete.obs")

corrplot(key_cor_matrix, method = "color", type = "upper", 
         order = "hclust", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, 
         number.cex = 0.6, tl.cex = 0.7)

### 
# regional comparisons
region_summary <- df_reduced_aggregated_complete %>%
  group_by(region) %>%
  summarize(
    n_countries = n(),
    avg_trans_support = mean(pct_support_trans),
    min_support = min(pct_support_trans),
    max_support = max(pct_support_trans),
    std_dev = sd(pct_support_trans)) %>%
  arrange(desc(avg_trans_support))

# visualize regional differences
ggplot(df_reduced_aggregated_complete, aes(x = reorder(region, pct_support_trans, FUN = mean), 
                                           y = pct_support_trans)) +
  geom_boxplot(aes(fill = region)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  coord_flip() +
  labs(title = "Support for Transgender Rights by European Region",
       x = "Region",
       y = "% Supporting Transgender Rights") +
  theme_minimal() +
  theme(legend.position = "none")

### some regression
# Create a scatterplot matrix with regression lines
ggplot(df_reduced_aggregated_complete, 
       aes(x = pct_nonreligious, y = pct_support_trans)) +
  geom_point(aes(size = rainbow_score_2019, color = region)) +
  geom_text_repel(aes(label = country_name), size = 3, max.overlaps = 10) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray") +
  labs(title = "Relationship Between Non-religiosity and Transgender Rights Support",
       x = "% Non-religious Population",
       y = "% Supporting Transgender Rights",
       size = "Rainbow Score",
       color = "Region") +
  theme_minimal()

ggplot(df_reduced_aggregated_complete, 
       aes(x = gender_equality_index, y = pct_support_trans)) +
  geom_point(aes(size = rainbow_score_2019, color = region)) +
  geom_text_repel(aes(label = country_name), size = 3, max.overlaps = 10) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray") +
  labs(title = "Gender Equality and Transgender Rights Support",
       x = "Gender Equality Index",
       y = "% Supporting Transgender Rights",
       size = "Rainbow Score",
       color = "Region") +
  theme_minimal()

### clusters
cluster_vars <- c("pct_support_trans", "norm_gdp_2018", 
                  "rainbow_score_2019", "gender_equality_index", 
                  "v2x_libdem", "pct_catholic", "pct_nonreligious")

# Prepare data
cluster_data <- df_reduced_aggregated_complete %>%
  select(country_name, all_of(cluster_vars)) %>%
  na.omit() %>%
  column_to_rownames("country_name")

# Scale the data
cluster_data_scaled <- scale(cluster_data)

# Compute distance matrix
dist_matrix <- dist(cluster_data_scaled, method = "euclidean")

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Clustering of Countries by Support for Transgender Rights",
     sub = "", xlab = "", cex = 0.7)
rect.hclust(hc, k = 4, border = "red")


### summary table
country_summary <- df_reduced_aggregated_complete %>%
  select(country_name, pct_support_trans, rainbow_score_2019, 
         gender_equality_index, norm_gdp_2018,
         pct_nonreligious, v2x_libdem, gdp_2018) %>%
  arrange(desc(pct_support_trans))

kable(country_summary, 
      col.names = c("Country", "Trans Rights Support (%)", "Rainbow Score", 
                    "Gender Equality", "LGBT Comfort", 
                    "Non-religious (%)", "Liberal Democracy", "GDP 2018"),
      digits = 1) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" " = 1, "Support Measures" = 2, "Equality Measures" = 2, 
                     "Social Factors" = 2, "Economic" = 1))


### check some bivariate relationships; choose three key demographic variblaes
# (1) age
ggplot(df_reduced, aes(x = age, y = as.numeric(qc19 == 1))) +
  geom_jitter(alpha = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Relationship Between Age and Transgender Rights Support",
       x = "Age", y = "Probability of Support") +
  theme_minimal()

# looks like age has a non-linear effect as the curve is flat for ages until ~60 and then falls off
# this suggests we should use a quadratic term for age

# (2) political ideology
ggplot(df_reduced, aes(x = political_ideology, y = as.numeric(qc19 == 1))) +
  geom_jitter(alpha = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Relationship Between Political Ideology and Transgender Rights Support",
       x = "Political Ideology (Left to Right)", 
       y = "Probability of Support") +
  theme_minimal()

## 

# (3a) religion

ggplot(df_reduced_aggregated_complete, aes(x = pct_nonreligious, y = pct_support_trans)) +
  geom_point(aes(size = rainbow_score_2019, color = region), alpha = 0.8) +
  geom_text_repel(aes(label = country_name), size = 3, max.overlaps = 15) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray") +
  labs(title = "Relationship Between Non-religiosity and Transgender Rights Support",
       subtitle = "Country-level data from Special Eurobarometer 493 (2019)",
       x = "% Non-religious Population",
       y = "% Supporting Transgender Document Changes",
       size = "Rainbow Score",
       color = "Region") +
  theme_minimal() 

# (3b) religion disaggregated
religion_support <- df_reduced %>%
  mutate(religion_group = case_when(
    religion == 1 ~ "Catholic",
    religion == 2 ~ "Orthodox",
    religion == 3 ~ "Protestant",
    religion == 4 ~ "Other Christian",
    religion %in% c(6:8) ~ "Muslim",
    religion %in% c(9:11) ~ "Other Religion",
    religion == 13 ~ "Atheist",
    religion == 14 ~ "Non-believer",
    TRUE ~ "Other/Missing")) %>%
  group_by(religion_group) %>%
  summarize(
    support_rate = mean(qc19 == 1, na.rm = TRUE) * 100,
    sample_size = n(),
    se = sqrt((support_rate/100 * (1-support_rate/100)) / sample_size) * 100) %>%
  filter(religion_group != "Other/Missing", sample_size >= 30) %>%
  arrange(desc(support_rate))

ggplot(religion_support, aes(x = reorder(religion_group, support_rate), y = support_rate)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  geom_errorbar(aes(ymin = support_rate - 1.96*se, 
                    ymax = support_rate + 1.96*se),
                width = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%", support_rate), y = support_rate + 3),
            vjust = 0) +
  geom_text(aes(label = paste0("n=", sample_size), y = -5),
            vjust = 1, size = 3) +
  coord_flip() +
  labs(title = "Support for Transgender Rights by Religious Affiliation",
       subtitle = "Percentage supporting transgender document changes by religion",
       x = NULL,
       y = "Support Rate (%)") +
  ylim(-5, max(religion_support$support_rate) + 10) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold"))









