# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)

# plot 1: employment and unemployment Rates
p_1 <- ggplot(country_level_df, aes(x = reorder(country_name, Employment))) +
  geom_bar(aes(y = Employment), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = Unemployment), color = "darkred", size = 3) +
  scale_y_continuous(
    name = "Employment Rate (%)",
    limits = c(0, 100),
    sec.axis = sec_axis(~., name = "Unemployment Rate (%)")
  ) +
  labs(title = "Employment and Unemployment Rates by Country (2018)",
       subtitle = "Bars: Employment rate | Points: Unemployment rate",
       x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y.left = element_text(color = "steelblue"),
        axis.title.y.right = element_text(color = "darkred"))

# plot 2: GDP per capita and GDP per capita growth rate
p_2 <- ggplot(country_level_df, aes(x = reorder(country_name, gdp_2018))) +
  geom_bar(aes(y = gdp_2018), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = gdp_growth * 500), color = "darkred", size = 3) +  # Scale growth to fit on same axis
  scale_y_continuous(
    name = "GDP per Capita (USD)",
    labels = comma,
    sec.axis = sec_axis(~./500, name = "GDP Growth 2005-2018 (%)")
  ) +
  labs(title = "GDP per Capita (2018) and Growth Rate",
       subtitle = "Bars: GDP per capita | Points: Growth rate",
       x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y.left = element_text(color = "steelblue"),
        axis.title.y.right = element_text(color = "darkred"))

# plot 3: relationship between employment and LGBT support
p_3 <- ggplot(country_level_df, aes(x = Employment, y = lgbt_support_percent)) +
  geom_point(aes(color = Unemployment, size = rainbow_score_2019)) +
  geom_text(aes(label = iso2), hjust = -0.3, vjust = 0, size = 3) +
  scale_color_viridis_c(option = "B", name = "Unemployment\nRate (%)") +
  scale_size_continuous(name = "Rainbow\nScore 2019") +
  labs(title = "Relationship between Employment Rates and LGBT Support",
       subtitle = "Color indicates unemployment rate, size shows level of LGBT legal protection",
       x = "Employment Rate (%)",
       y = "LGBT Support (%)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


# plot 4: relationship between GDP per capita to LGBT support
p_4 <- ggplot(country_level_df, aes(x = gdp_2018, y = lgbt_support_percent)) +
  geom_point(aes(color = rainbow_score_2019, size = gdp_growth)) +
  geom_text(aes(label = iso2), hjust = -0.3, vjust = 0, size = 3) +
  scale_color_viridis_c(option = "E", name = "Rainbow\nScore 2019") +
  scale_size_continuous(name = "GDP Growth\n2005-2018 (%)") +
  scale_x_continuous(labels = comma) +
  labs(title = "Relationship between GDP, LGBT Support and Rights Protections",
       subtitle = "Size indicates GDP growth rate from 2005-2018",
       x = "GDP per Capita (2018, USD)",
       y = "LGBT Support (%)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

