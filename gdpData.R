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

# Assign column as type character (for the rbind function to work)
gdpData$ecoSit <- as.character(gdpData$ecoSit)
gdpData$country <- as.character(gdpData$country)


# Only the columns that are required for the binding and the variable of interest
gdpDataClean <- gdpData[,c(1,3,6,7)]

# The Economic Situation in the first two months of 2022 are the same as for Dec. 2021
ecoSit12.2021 <- gdpDataClean[which(gdpDataClean$year == 2021 & gdpDataClean$month == 12),]
for (i in 1:nrow(ecoSit12.2021)){
  newRow1 <- c(ecoSit12.2021$country[i], 2022, 1, ecoSit12.2021$ecoSit[i])
  newRow2 <- c(ecoSit12.2021$country[i], 2022, 2, ecoSit12.2021$ecoSit[i])
  gdpDataClean <- rbind(gdpDataClean, newRow1, newRow2)
}

# Sort by country, year, month
gdpDataClean <- gdpDataClean %>% arrange(country, year, month)

# Assign column as type factor
gdpDataClean$ecoSit <- factor(gdpDataClean$ecoSit)
gdpDataClean$country <- factor(gdpDataClean$country)

# Column year and month need to be numeric for the join to work
gdpDataClean$year <- as.integer(gdpDataClean$year)
gdpDataClean$month <- as.integer(gdpDataClean$month)
