# Simple, correct correlation graph
library(ggplot2)
library(dplyr)

## Step 1: Create a simplified dataset with exact correlation values
# This creates a clean dataframe with factors and their correlation values
cor_data <- data.frame(
  factor = c(
    "Women's Political Empowerment",
    "Liberal Component Index",
    "Freedom of Expression Index",
    "Liberal Democracy Index",
    "Egalitarian Democracy Index",
    "LGBTI+ Legal Protections (Rainbow Index)",
    "Civil Society Participation Index",
    "Electoral Democracy Index"
  ),
  correlation = c(0.35, 0.34, 0.17, 0.15, 0.07, 0.07, 0.03, 0.01)
)

# Convert to factor to maintain order
cor_data$factor <- factor(cor_data$factor, levels = cor_data$factor[order(cor_data$correlation, decreasing = TRUE)])

## Step 2: Create a clean, minimalist plot
# This focuses on the bars and values without distracting elements
ggplot(cor_data, aes(x = factor, y = correlation)) +
  # Plain bars with a simple blue color
  geom_col(fill = "#4682B4", width = 0.7) +
  # Add correlation values to end of each bar
  geom_text(aes(label = sprintf("%.2f", correlation)),
            hjust = -0.3, size = 3.5) +
  # Simple, clean scales
  scale_y_continuous(limits = c(0, 0.40),   # Extended limit to accommodate labels
                     breaks = seq(0, 0.4, by = 0.1)) +
  # Flip for horizontal bars
  coord_flip() +
  # Clean labels
  labs(
    title = "Factors Most Strongly Associated with Trans Rights Support",
    x = NULL,
    y = "Correlation Coefficient with Public Support for Trans Rights",
    caption = "Source: Combined analysis of V-Dem indices, Rainbow Map, and Eurobarometer survey"
  ) +
  # Minimalist theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 20)),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )