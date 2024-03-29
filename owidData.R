library(readr)
library(dplyr)
library(lubridate)

owidData <- read_csv("owidData.csv")
countryCodes <- read_csv("countryCodes.csv")

# Transform three letter country code by two letter country code
owidData$iso_code <- countryCodes$ISO2[match(owidData$iso_code, countryCodes$ISO3)]

# Only use countries that are from the EU
casesData <- owidData %>% filter(iso_code %in% vacCountry)

remove(owidData)

# Rename column
colnames(casesData)[1] <- "country"

# Pick what columns we want in the dataset
colnames(casesData)
subCasesCols <- c("country", "date", "new_cases_per_million", "icu_patients_per_million")
casesData <- casesData[subCasesCols]

# Transform the Year-Week notation into Year and Month
for (i in 1:nrow(casesData)) {
  casesData$month[i] <- str_split(casesData$date[i], "-")[[1]][2]
  casesData$year[i] <- str_split(casesData$date[i], "-")[[1]][1]
} 

# Transform to per figures per month
casesDataClean <- casesData %>% group_by(year, month, country) %>% summarise(new_cases_per_million = sum(new_cases_per_million), 
                                                                             icu_patients_per_million = mean(icu_patients_per_million))
casesDataClean <- arrange(.data = casesDataClean, country, year, month)

# Replace null with 0
casesDataClean$new_cases_per_million[is.na(casesDataClean$new_cases_per_million)] <- 0
casesDataClean$icu_patients_per_million[is.na(casesDataClean$icu_patients_per_million)] <- 0

casesDataClean <- casesDataClean[c("country", "year", "month", "new_cases_per_million", "icu_patients_per_million")]

casesDataClean$year <- as.double(casesDataClean$year)
casesDataClean$month <- as.double(casesDataClean$month)
