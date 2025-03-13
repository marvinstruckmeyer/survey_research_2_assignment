library(dplyr)
library(readr)

df_rf_enriched_new <- readRDS("df_rf_enriched_new.rds")

df_reduced <- df_rf_enriched_new %>%
  select(
    # target
    qc19,
    
    # country identifier
    country, country_name, isocntry,
    
    # individual demographics
    age = d11,           
    gender = d10,            
    education = d8, d8r2,      
    occupation = d15a,           
    urban_rural = d25,            
    financial_insecurity = d60,            
    social_class = d63,         
    religion = sd3,            
    political_ideology = d1, 
    lgb_friends = sd1_4,
    trans_friends = sd1_7,
    
    # Personal identity
    ethnic_minority = sd2_1,
    skin_color_minority = sd2_2,
    religious_minority = sd2_3,
    sexual_lgbt_minority = sd2_5,
    disability_minority = sd2_6,
    other_minority = sd2_7,
    none_minority = sd2_8,
    
    # Discrimination
    trans_discrimination_country = qc1_8,
    trans_discrimination_personal = qc2_6,
    trans_discrimination_workplace = qc4_8,
    trans_discrimination_political = qc6_10,
    country_discrimination_efforts = qc7,
    country_discrimination_efforts_recoded = qc7r,
    trans_workplace_diversity = qc9_10,
    trans_colleague = qc12_11,
    trans_colleague_recoded = qc12_11r,
    trans_child_relationship = qc13_11,
    trans_child_relationship_recoded = qc13_11r,
    lgb_rights = qc15_1, 
    same_sex_relationship = qc15_2, 
    same_sex_marriage = qc15_3,
    lgb_school_materials = qc17_3,
    trans_school_materials = qc17_4,
    intersex_school_materials = qc17_5,
    two_men_public_affection = qc18_2,
    two_men_public_affection_recoded = qc18_2r,
    two_women_public_affection = qc18_3,
    two_women_public_affection_recoded = qc18_3r,
    non_gendered_docs = qc20,
    
    
    # Country-level variables
    gdp_2018,                # Economic development
    rainbow_score_2019,      # LGBTI rights/protections
    gender_equality_index,   # Gender equality
    Happiness_Score,         # National well-being
    gdp_2018,                # Economic development
    rainbow_score_2019,      # LGBTI rights/protections
    rainbow_score_2018,      # LGBTI rights/protections (previous year)
    rainbow_score_avg_2019_2018, # Average LGBTI rights/protections
    gender_equality_index,   # Gender equality
    Happiness_Score,         # National well-being
    v2x_libdem,              # Democratic quality
    v2x_egaldem,             # Democratic quality
    Regime_type,             # Democratic quality
    z_functioning_of_government, #Govenment function measure
    composite_equality,     # Equality measure
    z_composite_equality,   # Standardized equality measure
    norm_composite_equality, # Normalized equality measure
    z_lgbt_support,         # Standardized LGBT support
    norm_lgbt_support,      # Normalized LGBT support
    pct_friends_lgbt,       # Percentage of LGBT friends
    z_pct_friends_lgbt,     # Standardized % of LGBT friends
    norm_pct_friends_lgbt,  # Normalized % of LGBT friends
    mean_left_right,         # Average political leaning
    pct_high_education,      # Education level
    region,                  # Geographic region
    
    # Paradata
    interview_date = p1,                      
    interview_start_time = p2,                      
    interview_duration = p3,                      
    interview_duration_recoded = p3r,                     
    people_present_during_interview = p4,                      
    respondent_cooperation = p5                       
  )

# Save to a new file
write_rds(df_reduced, "df_reduced.rds")