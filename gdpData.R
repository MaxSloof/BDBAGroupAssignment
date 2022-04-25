library(readr)
library(tidyr)
library(dplyr)

# Read the file
gdpData <- read.csv("GDPData.csv")

# Only use the values we want in the dataset
  # Only use GDP at market prices (B1GQ)
gdpData <- gdpData %>% filter(na_item == "B1GQ")
  # Only use GDP at current prices in euros (CLV10_EUR_HAB)
gdpData <- gdpData %>% filter(unit == "CLV10_EUR_HAB")
  # Only use countries from the unemployment dataset (gets filtered anyhow in merge_data.R)
gdpData <- gdpData %>% filter(geo %in% unempCountries)
  # Only use the columns of interest
gdpData <- gdpData[,c(7:9)]

# Change column names to align with other datasets
colnames(gdpData) <- c("country", "date", "gdp")

# Separate YEAR-Quarter notation to YEAR and quarter
gdpData <- gdpData %>% separate(date, c("year", "quarter"), remove = FALSE)

# Create new variable with the corresponding months for every quarter
for (i in 1:nrow(gdpData)) {
  gdpData$month[i] <- quarter$month[match(gdpData$quarter[i], quarter$quarter)]
}

# Create a new row for every month in the quarter
gdpData <- separate_rows(gdpData, month, convert = TRUE)

# Exclude values from 2019 as our analysis starts from Jan 2020
gdpData <- gdpData %>% filter(!year == 2019)

# Create the range across which a new cat variable will be created
gdpRange <- (max(gdpData$gdp) - min(gdpData$gdp)) / 3

# First third of the range will be poor countries, second third average countries, third third rich countries
for (i in 1:nrow(gdpData)) {
  gdpData$ecoSit[i] <- ifelse(gdpData$gdp[i] < (min(gdpData$gdp) + gdpRange), "Poor",
                              ifelse(gdpData$gdp[i] < (min(gdpData$gdp) + gdpRange*2), "Average", "Rich"))
}

# Assign column as type factor
gdpData$ecoSit <- factor(gdpData$ecoSit)

# Only the columns that are required for the binding and the variable of interest
gdpDataClean <- gdpData[,c(1,3,6,7)]

# Column year and month need to be numeric for the join to work
gdpDataClean$year <- as.integer(gdpDataClean$year)
gdpDataClean$month <- as.integer(gdpDataClean$month)
