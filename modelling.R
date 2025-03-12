### modelling

# get the heavily reduced dataset ready
df_rf_enriched <- readRDS("df_rf_enriched.rds")
df_rf_reduced <- readRDS("df_rf_reduced.rds")

# from EDA:
# individual variables: respondent_cooperation (para), non_gendered_docs, same_sex_marriage
# political_ideology, religion, education

# country cariables: z_Functioning_of_Government, z_v2x_libdem, z_rainbow_score_2019,
# z_v2x_egaldem, region.x, z_Happiness_score

df_rf_enriched_modelling_country <- df_rf_enriched %>%
  select(
         # country variables:
         z_Functioning_of_government, z_v2x_libdem, z_v2x_egaldem, 
         z_rainbow_score_2019, region, z_Happiness_Score,
         
         # for joining
         isocntry)

df_rf_enriched_modelling_individual <- df_rf_enriched %>%
  select(
        # individual variables:
        non_gendered_docs = qc20, same_sex_marriage = qc15_3, 
        political_ideology = d1, religion = sd3,
        education = d8, respondent_cooperation = p5,
        
        # our target:
        transgender_support = qc19,
        
        # for joining
        isocntry)

df_rf_enriched_modelling <- df_rf_enriched_modelling_country %>%
  left_join(df_rf_enriched_modelling_individual, by = "isocntry")

# check the variables again
df_rf_enriched_modelling %>% select(where(is.numeric)) %>% summary()


