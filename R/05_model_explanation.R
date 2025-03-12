
##### USE THE RMD NOT THIS ######

library(lme4)      # For multilevel modeling
library(lmerTest)  # For p-values in multilevel models
library(ggplot2)   # For data visualization
library(dplyr)     # For data manipulation
library(sjstats)   # For calculating ICC
library(sjPlot)    # For plotting model results

# Read the data
# No need to read the CSV file if df_reduced is already loaded in your environment

# Quick data check
table(df_reduced$country_name)

# Visualize average qc19 by country
country_means <- aggregate(qc19 ~ country_name, data = df_reduced, FUN = mean)
country_means <- country_means[order(country_means$qc19), ]

ggplot(country_means, aes(x = reorder(country_name, qc19), y = qc19)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Average qc19 by Country",
       x = "Country",
       y = "Mean qc19") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# STEP 1: Null Model (Intercept-only model)
null_model <- lmer(qc19 ~ 1 + (1|country_name), data = df_reduced)
summary(null_model)

# Calculate ICC
performance::icc(null_model)

# STEP 2: Add Individual-Level Predictors (Level 1)
# Let's start with predictors that showed correlation with qc19
model1 <- lmer(qc19 ~ age + gender + education + political_ideology + lgb_friends + trans_friends + (1|country_name), 
               data = df_reduced)
summary(model1)

# STEP 3: Add Country-Level Predictors (Level 2)
model2 <- lmer(qc19 ~ age + gender + education + political_ideology + lgb_friends + trans_friends + 
                 rainbow_score_2019 + gender_equality_index + (1|country_name), 
               data = df_reduced)
summary(model2)

# STEP 4: Add Random Slopes for Individual-Level Predictors
# Let's add random slopes for predictors with the highest correlations
model3 <- lmer(qc19 ~ age + gender + education + political_ideology + lgb_friends + trans_friends + 
                 rainbow_score_2019 + gender_equality_index + 
                 (1 + lgb_friends|country_name), 
               data = df_reduced)
summary(model3)

# Check if the random slope is significant
ranova(model3)

# STEP 5: Add Cross-Level Interactions
# Assuming lgb_friends has a significant random slope
# We can create an interaction between lgb_friends and country-level predictors
model4 <- lmer(qc19 ~ age + gender + education + political_ideology + lgb_friends + trans_friends + 
                 rainbow_score_2019 + gender_equality_index + 
                 lgb_friends:rainbow_score_2019 +
                 (1 + lgb_friends|country_name), 
               data = df_reduced)
summary(model4)

# Compare models
anova(null_model, model1, model2, model3, model4)

# Visualize fixed effects
plot_model(model4, show.values = TRUE, type = "est")

# Visualize random effects
plot_model(model4, type = "re")

# Visualize interaction effect
plot_model(model4, type = "int")

# Check model assumptions
# Residual plots
plot(model4)
qqnorm(resid(model4))
qqline(resid(model4))

# Random effects normality
qqnorm(ranef(model4)$country_name[,1])  # Random intercepts
qqline(ranef(model4)$country_name[,1])
qqnorm(ranef(model4)$country_name[,2])  # Random slopes for lgb_friends
qqline(ranef(model4)$country_name[,2])