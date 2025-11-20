library(tidyverse)
library(tidyverse)

# Use the correct filename
ghg_data <- read_csv("Data/emissions-by-sector.csv")

# Quick check
head(ghg_data)

colnames(ghg_data)


# Filter for US and 2022
us_2022 <- ghg_data %>%
  filter(Entity == "United States", Year == 2022) %>%
  select(buildings, industry, `land use and forestry`, `fuel combustion`,
         transportation, `manufacturing and construction`, `energy production`,
         `electricity and heat`, `bunker fuels`)
us_2022_long <- us_2022 %>%
  pivot_longer(cols = everything(),
               names_to = "sector",
               values_to = "emissions")

library(RColorBrewer)

library(ggplot2)
library(RColorBrewer)
library(ggplot2)
library(RColorBrewer)

# Create bar chart
ggplot(us_2022_long, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer(palette = "YlGn", direction = 1) +  # Yellow-Green palette
  labs(title = "U.S. Greenhouse Gas Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (Tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#remove scientific notation
library(scales)

ggplot(us_2022_long, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer(palette = "YlGn", direction = 1) +
  scale_y_continuous(labels = scales::comma) +   # ← remove scientific notation
  labs(title = "U.S. Greenhouse Gas Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (Tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



# Filter for Sweden and 2022
sweden_2022 <- ghg_data %>%
  filter(Entity == "Sweden", Year == 2022) %>%
  select(buildings, industry, `land use and forestry`, `fuel combustion`,
         transportation, `manufacturing and construction`, `energy production`,
         `electricity and heat`, `bunker fuels`)

# Convert to long format for ggplot
sweden_2022_long <- sweden_2022 %>%
  pivot_longer(cols = everything(),
               names_to = "sector",
               values_to = "emissions")

# Optional: highlight Transportation
sweden_2022_long <- sweden_2022_long %>%
  mutate(highlight = ifelse(sector == "transportation", "Transportation", "Other"))


ggplot(sweden_2022_long, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer(palette = "YlGn", direction = 1) +  # Yellow-Green palette
  labs(title = "Sweden Greenhouse Gas Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (metric tons CO2e)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Filter for France and 2022
france_2022 <- ghg_data %>%
  filter(Entity == "France", Year == 2022) %>%
  select(buildings, industry, `land use and forestry`, `fuel combustion`,
         transportation, `manufacturing and construction`, `energy production`,
         `electricity and heat`, `bunker fuels`)

# Convert to long format for ggplot
france_2022_long <- france_2022 %>%
  pivot_longer(cols = everything(),
               names_to = "sector",
               values_to = "emissions")

# Optional: highlight Transportation
france_2022_long <- france_2022_long %>%
  mutate(highlight = ifelse(sector == "transportation", "Transportation", "Other"))


ggplot(france_2022_long, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer(palette = "YlGn", direction = 1) +  # Yellow-Green palette
  labs(title = "France Greenhouse Gas Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (metric tons CO2e)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Filter for japan and 2022
japan_2022 <- ghg_data %>%
  filter(Entity == "Japan", Year == 2022) %>%
  select(buildings, industry, `land use and forestry`, `fuel combustion`,
         transportation, `manufacturing and construction`, `energy production`,
         `electricity and heat`, `bunker fuels`)

# Convert to long format for ggplot
japan_2022_long <- japan_2022 %>%
  pivot_longer(cols = everything(),
               names_to = "sector",
               values_to = "emissions")

# Optional: highlight Transportation
japan_2022_long <- japan_2022_long %>%
  mutate(highlight = ifelse(sector == "transportation", "Transportation", "Other"))


# Remove "energy production" from the long dataset
japan_2022_long_filtered <- japan_2022_long %>%
  filter(sector != "energy production")

# Plot without "energy production"
ggplot(japan_2022_long_filtered, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer(palette = "YlGn", direction = 1) +  # Yellow-Green palette
  labs(title = "Japan Greenhouse Gas Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (Tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggplot(japan_2022_long_filtered, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer(palette = "YlGn", direction = 1) +
  scale_y_continuous(labels = scales::comma) +   # ← remove scientific notation
  labs(title = "Japan Greenhouse Gas Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (Tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

