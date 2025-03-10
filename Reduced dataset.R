df_reduced <- df_rf_enriched %>%
  select(
    # target
    qc19,
    
    # country identifier
    country, country_name, isocntry,
    
    # individual demographics
    d11,            # age
    d10,            # gender
    d8, d8r2,       # education level
    d15a,           # occupation
    d25,            # urbanicity (rural/urban)
    d60,            # financial difficulties
    d63,            # subjective social class
    
    # values and identity
    sd3,            # religious affiliation
    d1,             # political ideology (left-right)
    sd2_5,          # sexual minority identity
    qc15_1, qc15_2, qc15_3,  # Support for LGBT rights
    qc6_10, qc6_10r,         # Comfort with LGBT people in positions of power
    qc12_10, qc12_10r,       # Comfort with LGBT colleagues
    qc13_10, qc13_10r,       # Comfort with children having LGBT relationships
    sd1_4, sd1_5, sd1_7, sd1_8,  # Having LGBT, disabled, different religion, or transgender friends
    qc18_1, qc18_2, qc18_3,      # Comfort with public displays of affection
    
    # Discrimination perceptions
    qc1_4, qc1_7, qc1_8, qc1_9, qc1_10,  # Perceived discrimination prevalence
    qc2_4, qc2_5, qc2_6, qc2_7,          # Personal experience of discrimination
    
    # Country-level variables
    gdp_2018,                # Economic development
    rainbow_score_2019,      # LGBTI rights/protections
    gender_equality_index,   # Gender equality
    Happiness_Score,         # National well-being
    v2x_libdem, Regime_type, # Democratic quality
    
    # Country religious and demographic composition
    pct_catholic,
    pct_orthodox,
    pct_protestant,
    pct_atheist,
    pct_nonbeliever,
    pct_nonreligious,
    
    mean_left_right,         # Average political leaning
    pct_high_education,      # Education level
    region,                  # Geographic region
    
    # Paradata
    p1,                      # Interview date
    p2,                      # Interview begin time
    p3,                      # Interview duration
    p3r                      # Interview duration recoded
    p4,                      # Number of people present during interview
    p5                       # Respondent cooperation
  )