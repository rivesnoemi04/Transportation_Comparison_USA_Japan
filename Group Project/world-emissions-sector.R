#libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

#load data
worl_em <- read_csv("Data/emmisions-by-sector.csv")

# Filter the dataset for year 2022
worl_em_2022 <- worl_em %>%
  filter(Year == 2022)

#filter out the "world" row
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "World")

#delete row
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "Upper-middle-income countries")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "Low-income countries")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "Lower-middle-income countries")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "Europe")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "European Union (27)")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "Serbia")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "North America")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "South America")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "Oceania")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "Asia")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "Africa")
worl_em_2022 <- worl_em_2022 %>%
  filter(Entity != "High-income countries")

#create dataset with continents
continents_em <- worl_em %>%
  filter(Year == 2022,
           Entity %in% c("United Kingdom", 
                       "Europe", 
                       "Asia", 
                       "Africa", 
                       "North America", 
                       "South America", 
                       "Australia"))

#create a new world row and sum up all emissions
world_row <- continents_em %>%
  summarize(
    Entity = "World",
    Code = "WORLD",              
    Year = 2022,
    across(where(is.numeric), sum, na.rm = TRUE)
  )

# Bind this row to the bottom of the dataset
continents_em <- bind_rows(continents_em, world_row)

#convert workd to long
world_bar <- world_row %>%
  pivot_longer(
    cols = where(is.numeric),   # all numeric columns = your sectors
    names_to = "sector",
    values_to = "emissions"
  )

##make bar chart
ggplot(world_bar, aes(x = sector, y = emissions, fill = sector)) +
  geom_bar(stat = "identity") +
  labs(title = "World CO2 Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (Billion Tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#lets improve  the bar chart
world_bar <- world_row %>%
  pivot_longer(
    cols = where(is.numeric) & !Year,   # EXCLUDE the year column
    names_to = "sector",
    values_to = "emissions"
  )
#sort sectors
ggplot(world_bar, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity") +
  labs(title = "World CO2 Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (Tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#remove scientific notation
ggplot(world_bar, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity") +
  labs(title = "World CO2 Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (Tons)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#green
ggplot(world_bar, aes(x = reorder(sector, -emissions), y = emissions, fill = sector)) +
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer(palette = "YlGn", direction = 1) +  # discrete YlGn palette
  labs(title = "World Greenhouse Gas Emissions by Sector (2022)",
       x = "Sector",
       y = "Emissions (Tons)",
       fill = "Sector") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")




