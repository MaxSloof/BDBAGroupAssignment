library(readr)
library(dplyr)
library(stringr)

unemp <- read.csv("unemp_month.csv")

# Use the seasonally adjusted data
unemp <- unemp %>% filter(unemp$s_adj == "SA:Seasonally adjusted data")

# identify the required columns
unemp_cols <- c("geo", "TIME_PERIOD", "OBS_VALUE")

# create new dataset with the identified columns
dsUnemp <- unemp[unemp_cols]

# rename columns
colnames(dsUnemp) <-c("country", "date", "unemp")

# Loop through dataset
for (i in 1:nrow(dsUnemp)) {
  
  # Transform the notation to ISO2, like the other datasets
  dsUnemp$country[i] <- str_split(dsUnemp$country, ":")[[i]][1]
  
  # Split the date in separate year and month columns
  dsUnemp$month[i] <- str_split(dsUnemp$date, "-")[[i]][2]
  dsUnemp$year[i] <- str_split(dsUnemp$date, "-")[[i]][1]
}

# Only countries that are used in the other datasets
unempDataClean <- dsUnemp %>% filter(dsUnemp$country %in% vacCountry)

# Country variable is factor
unempDataClean$country <- factor(unempDataClean$country)
unempCountries <- levels(unempDataClean$country)

# Sort the dataset by country, year, month
unempDataClean <- unempDataClean %>% arrange(country, year, month)

unempDataClean <- unempDataClean[!unempDataClean$year == "2018" & !unempDataClean$year == "2019",]

# Pick the unemployment rate at 01-2020 as the baseline per country
for (i in 1:length(unempCountries)) {
  
  # First observation for that country
  Cntry1 <- match(unempCountries[i], unempDataClean$country)
  
  # Last observation for that country
  Cntry2 <- match(unempCountries[i+1], unempDataClean$country) - 1
  
  # Last country has no proceeding country, so last row is taken as last observation
  if(is.na(Cntry2)) {Cntry2 <- nrow(unempDataClean)}
  
  # Find the row number of January 2020 per country
  baseDate <- match("2020-01", unempDataClean$date[Cntry1:Cntry2])
  
  # Store the unemployment rate for January 2020 per country
  unempRate <- unempDataClean$unemp[baseDate + Cntry1 - 1]
  
  # Calculate the difference in unemp. rate compared to baseline for every observation per country
  for (j in Cntry1:Cntry2) {
    unempDataClean$adjUnemp[j] <- unempDataClean$unemp[j] - unempRate
  }
}

unempDataClean <- unempDataClean[c("country", "year", "month", "adjUnemp")]
unempDataClean$year <- as.double(unempDataClean$year)
unempDataClean$month <- as.double(unempDataClean$month)
