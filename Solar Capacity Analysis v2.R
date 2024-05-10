library(tidyverse)
library(scales)
library(lubridate)
library(WDI)

setwd("C:/Users/kylei/Dropbox/Solar/Solar Capacity Analysis")

# Set scipen to a high value to avoid scientific notation
options(scipen = 999)

##########################################################################################################
# Electricity capacity data

# Data source: Ember yearly electricity data
# https://ember-climate.org/data-catalogue/yearly-electricity-data/
data_raw <- read.csv("yearly_full_release_long_format.csv")

# Clean up data
capacity_data <- data_raw %>%
  filter(Area.type == "Country", Category == "Capacity", Subcategory == "Fuel", Year == 2022) %>%
  select(country = Area, source = Variable, capacity = Value) %>%
  mutate(capacity = replace_na(capacity, 0)) %>% # Replace NAs with 0s
  group_by(country) %>%
  mutate(total_capacity = sum(capacity)) %>% # Get total electricity capacity across all sources
  filter(source == "Solar") %>%
  mutate(solar_share = capacity/total_capacity) %>% # Calculate share of total capacity that is solar
  rename(solar_capacity = capacity) %>%
  select(-source)

capacity_data_fixed <- capacity_data %>%
  mutate(country = ifelse(country == "Bosnia Herzegovina", "Bosnia and Herzegovina", country)) %>%
  mutate(country = ifelse(country == "Congo (the Democratic Republic of the)", "Congo, Dem. Rep.", country)) %>%
  mutate(country = ifelse(country == "Dominican Republic (the)", "Dominican Republic", country)) %>%
  mutate(country = ifelse(country == "Egypt", "Egypt, Arab Rep.", country)) %>%
  mutate(country = ifelse(country == "Iran (Islamic Republic of)", "Iran, Islamic Rep.", country)) %>%
  mutate(country = ifelse(country == "South Korea", "Korea, Rep.", country)) %>%
  mutate(country = ifelse(country == "Kyrgyzstan", "Kyrgyz Republic", country)) %>%
  mutate(country = ifelse(country == "Lao People's Democratic Republic (the)", "Lao PDR", country)) %>%
  mutate(country = ifelse(country == "Niger (the)", "Niger", country)) %>%
  mutate(country = ifelse(country == "Philippines (the)", "Philippines", country)) %>%
  mutate(country = ifelse(country == "Russian Federation (the)", "Russian Federation", country)) %>%
  mutate(country = ifelse(country == "Slovakia", "Slovak Republic", country)) %>%
  mutate(country = ifelse(country == "Sudan (the)", "Sudan", country)) %>%
  mutate(country = ifelse(country == "Syrian Arab Republic (the)", "Syrian Arab Republic", country)) %>%
  mutate(country = ifelse(country == "Turkey", "Turkiye", country)) %>%
  mutate(country = ifelse(country == "United States of America", "United States", country)) %>%
  mutate(country = ifelse(country == "Venezuela (Bolivarian Republic of)", "Venezuela, RB", country)) %>%
  mutate(country = ifelse(country == "Yemen", "Yemen, Rep.", country))

##########################################################################################################
# Macro data
# Get population and GDP for each country for year 2022 from World Bank
indicators <- c(pop = "SP.POP.TOTL", gdp = "NY.GDP.MKTP.CD")
macro_data <- WDI(indicator = indicators, start = 2022, end = 2022, extra = FALSE, cache = NULL) %>%
  select(country, pop, gdp)

aggregates <- c("Africa Eastern and Southern", "Africa Western and Central", "Arab World", "Caribbean small states", "Central Europe and the Baltics", "Early-demographic dividend", "East Asia & Pacific (excluding high income)", "East Asia & Pacific (IDA & IBRD countries)", "East Asia & Pacific", "Euro area", "Europe & Central Asia (excluding high income)", "Europe & Central Asia (IDA & IBRD countries)", 
  "Europe & Central Asia", "European Union", "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)", "High income", "IBRD only", "IDA & IBRD total", "IDA blend", "IDA only", "IDA total", "Late-demographic dividend", "Latin America & Caribbean (excluding high income)", "Latin America & Caribbean, Latin America & the Caribbean (IDA & IBRD countries)", "Least developed countries: UN classification", "Low & middle income", "Low income", "Lower middle income", "Middle East & North Africa (excluding high income)", "Middle East & North Africa (IDA & IBRD countries)", "Middle East & North Africa", "Middle income", "Not classified", "OECD members", "Other small states", "Pacific island small states", "Post-demographic dividend", "Pre-demographic dividend", "South Asia (IDA & IBRD)", "South Asia", "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)", "Sub-Saharan Africa", "Upper middle income")

# Remove aggregates
macro_data <- macro_data %>%
  filter(!country %in% aggregates)

main_data <- left_join(macro_data, capacity_data_fixed, by = "country")

##########################################################################################################
# Get China solar export data by country from Ember

# Data source: Ember China solar PV exports
# https://ember-climate.org/data-catalogue/china-solar-pv-exports/
exports_raw <- read.csv("mart_solar_exports_full_release_monthly.csv")

exports <- exports_raw %>%
  filter(Area.type == "Country") %>%
  mutate(date = as.Date(Date)) %>%
  select(country = Area, date, capacity = Capacity..MW.)

# The data is originally monthly. Now we calculate yearly values for each country (and drop the years from 2022 onwards)
exports_yearly <- exports %>%
  mutate(year = year(date)) %>%
  group_by(country, year) %>%
  summarise(year_capacity = sum(capacity), .groups = 'drop') %>%
  filter(year <= 2021)

exports_total <- exports_yearly %>%
  group_by(country) %>%
  summarise(exports = sum(year_capacity), .groups = 'drop')

exports_fixed <- exports_total %>%
  mutate(country = ifelse(country == "Bosnia Herzegovina", "Bosnia and Herzegovina", country)) %>%
  mutate(country = ifelse(country == "Congo (DRC)", "Congo, Dem. Rep.", country)) %>%
  mutate(country = ifelse(country == "Dominican Republic (the)", "Dominican Republic", country)) %>%
  mutate(country = ifelse(country == "Egypt", "Egypt, Arab Rep.", country)) %>%
  mutate(country = ifelse(country == "Iran", "Iran, Islamic Rep.", country)) %>%
  mutate(country = ifelse(country == "South Korea", "Korea, Rep.", country)) %>%
  mutate(country = ifelse(country == "Kyrgyzstan", "Kyrgyz Republic", country)) %>%
  mutate(country = ifelse(country == "Lao", "Lao PDR", country)) %>%
  mutate(country = ifelse(country == "Niger (the)", "Niger", country)) %>%
  mutate(country = ifelse(country == "The Philippines", "Philippines", country)) %>%
  mutate(country = ifelse(country == "Russia", "Russian Federation", country)) %>%
  mutate(country = ifelse(country == "Slovakia", "Slovak Republic", country)) %>%
  mutate(country = ifelse(country == "Sudan (the)", "Sudan", country)) %>%
  mutate(country = ifelse(country == "Syria", "Syrian Arab Republic", country)) %>%
  mutate(country = ifelse(country == "Türkiye", "Turkiye", country)) %>%
  mutate(country = ifelse(country == "United States of America", "United States", country)) %>%
  mutate(country = ifelse(country == "Venezuela (Bolivarian Republic of)", "Venezuela, RB", country)) %>%
  mutate(country = ifelse(country == "Yemen", "Yemen, Rep.", country))


main_data <- left_join(main_data, exports_fixed, by = "country")

main_data <- main_data %>%
  filter(total_capacity >= 1) %>%
  filter(solar_capacity > 0)

main_data_plot <- main_data %>%
  mutate(solar_capacity = solar_capacity*1000000000) %>%
  mutate(exports = exports*1000000)

# Custom label function for power in Watts to MW and GW
power_label <- function(x) {
  sapply(x, function(x) {
    if (is.na(x)) {
      return("NA")  # Or return("") for no label
    }
    if (x >= 1e9) {
      return(paste(format(x / 1e9, digits = 2, nsmall = 0), "GW"))
    } else if (x >= 1e6) {
      return(paste(format(x / 1e6, digits = 2, nsmall = 0), "MW"))
    } else {
      return(paste(x, "W"))
    }
  })
}

# Plot
windows()
ggplot(main_data_plot, aes(x = exports, y = solar_capacity)) +
  geom_point(color = 'blue', size = 2) +
  geom_text(aes(label = country), nudge_x = 0,
             nudge_y = 0.2, check_overlap = TRUE) +
  scale_x_log10(labels = power_label, breaks = trans_breaks("log10", function(x) 10^x)) +
  scale_y_log10(labels = power_label, breaks = trans_breaks("log10", function(x) 10^x)) +
  labs(
    title = "Solar Power and Chinese Imports",
    x = "Solar imports from China (2017-2021)",
    y = "Solar power capacity (2022)",
    caption = "Kyle Chan - high-capacity.com - Data sources: Ember, IRENA, World Bank"
  ) +
  theme_bw() +
  theme(
    text = element_text(color = "black", family = "Arial", size = 14),  # Default text color
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(color = "black", size = 16),
    axis.text = element_text(color = "black"),
    plot.caption = element_text(hjust = 0)
  )
ggsave("solar_export_plot.png", width = 7, height = 5)


#################################################################################################################
# Logged versions

# Create log version of all data set
logged_data <- main_data %>%
  filter(solar_capacity > 0) %>%
  mutate(log_solar = log(solar_capacity*1000000000), log_exports = log(exports*1000000), log_pop = log(pop), log_gdp = log(gdp))

# Run regressions with different models controlling for population and GDP
model <- lm(log_solar ~ log_exports, data = data_log)
summary(model)

model <- lm(log_solar ~ log_exports + log_pop, data = data_log)
summary(model)

model <- lm(log_solar ~ log_exports + log_gdp, data = data_log)
summary(model)

model <- lm(log_solar ~ log_exports + log_pop + log_gdp, data = data_log)
summary(model)

# Plot
windows()
ggplot(logged_data, aes(x = log_solar, y = log_exports)) +
  theme_minimal()


########################################################################################################################
########   Old code ################
########################################################################################################################

# Combine the solar capacity data with the solar export data
data_combined <- left_join(data, exports_total, by = "country")


# Add population and GDP data from World Bank
data_all <- left_join(data_combined, macro_data, by = "country")

# Create log version of all data set
data_log <- data_all %>%
  filter(solar_capacity > 0) %>%
  mutate(log_solar = log(solar_capacity*1000000000), log_exports = log(exports*1000000), log_pop = log(pop), log_gdp = log(gdp))

windows()
ggplot(data_log, aes(x = log_solar, y = log_exports)) +
  geom_point()



model <- lm(log_solar ~ log_exports + log_pop + log_gdp, data = data_log)
summary(model)


data_all <- data_all %>%
  mutate(exports_per_capita = exports / pop) %>% # Calculate Chinese solar exports per capita
  filter(total_capacity >= 1) # Drop all countries with less than 1 GW of total capacity


# Create a scatterplot
windows()
ggplot(data_all, aes(x = exports_per_capita, y = solar_share)) +
  geom_point(color = 'blue')

data_mod <- data_all %>%
  filter(country != "Netherlands")

ggplot(data_mod, aes(x = exports_per_capita, y = solar_share)) +
  geom_point(color = 'blue')

ggplot(data_all, aes(x = exports, y = solar_capacity)) +
  geom_point(color = 'blue')



# Create log-log version of data
data_log <- data_all %>%
  filter(solar_capacity > 0) %>%
  mutate(log_solar = log(solar_capacity*1000000000), log_exports = log(exports*1000000), log_pop = log(pop))

windows()
ggplot(data_log, aes(x = log_solar, y = log_exports)) +
  geom_point(color = 'blue')



model <- lm(log_solar ~ log_exports + log_pop, data = data_log)
summary(model)


population_data <- WDI(indicator = "SP.POP.TOTL", start = 2022, end = 2022, extra = FALSE, cache = NULL)

population_data <- population_data %>%
  rename(pop = SP.POP.TOTL) %>%
  select(country, pop)