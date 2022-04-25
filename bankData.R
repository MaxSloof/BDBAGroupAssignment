library(readr)
library(dplyr)
library(tidyr)

# Read csv file
bankData <- read.csv("bankrupcies.csv")

# Identify the import columns
colnames(bankData)
bankCols = c("geo", "TIME_PERIOD", "OBS_VALUE", "nace_r2")

# Only pick seasonally adjusted data and the bankruptcy data (not interested in business registrations)
bankData <- bankData %>% filter(s_adj == "SCA" & indic_bt == "BKRT")

# Only use countries that we have used in other datasets
bankDataFilter <- bankData[bankCols] %>% filter(bankData$geo %in% unempCountries)

# Rename columns
colnames(bankDataFilter) <- c("country","date", "bankruptcy", "industry")

# Create a dataframe that will be used to convert quarters into months
quarter <- data.frame(quarter = c("Q1","Q2","Q3","Q4"), month = c("1,2,3","4,5,6","7,8,9","10,11,12"))

# Bankruptcies are given per industry, we want the mean of the four industries
bankDataFilter <- bankDataFilter %>% group_by(country, date) %>% summarise(bankruptcy = mean(bankruptcy))

# Date was noted as YYYY-QX and we want to split YYYY and QX
bankDataFilter <- bankDataFilter %>% separate(date, c("year", "quarter"), remove = FALSE)

# Sort by country, year, quarter
bankDataFilter <- bankDataFilter %>% arrange(country, year, quarter)

# Column as type factor
bankDataFilter$country <- factor(bankDataFilter$country)

# Save the different levels 
bankCountries <- levels(bankDataFilter$country)

for (i in 1:length(bankCountries)) {
  
  # First observation for that country
  Cntry1 <- match(bankCountries[i], bankDataFilter$country)
  
  # Last observation for that country
  Cntry2 <- match(bankCountries[i+1], bankDataFilter$country) - 1
  
  # Last country has no proceeding country, so last row is taken as last observation
  if(is.na(Cntry2)) {Cntry2 <- nrow(bankDataFilter)}
  
  for (j in Cntry1:Cntry2) {
    bankDataFilter$month[j] <- quarter$month[match(bankDataFilter$quarter[j], quarter$quarter)]
  }
}

# Separate the observations from quarters to months
bankClean <- separate_rows(bankDataFilter, month, convert = TRUE)

# Remove values from before Jan 2020
bankClean <- bankClean[!bankClean$year == 2018 & !bankClean$year == 2019,]

for (i in 1:length(bankCountries)) {
  
  # First observation for that country
  Cntry1 <- match(bankCountries[i], bankClean$country)
  
  # Last observation for that country
  Cntry2 <- match(bankCountries[i+1], bankClean$country) - 1
  
  # Last country has no proceeding country, so last row is taken as last observation
  if(is.na(Cntry2)) {Cntry2 <- nrow(bankClean)}
  
  # Find the row number of January 2020 per country
  baseDate <- match("2020-Q1", bankClean$date[Cntry1:Cntry2])
  
  # Store the unemployment rate for January 2020 per country
  bankRate <- bankClean$bankruptcy[baseDate + Cntry1 - 1]
  
  # Calculate the difference in unemp. rate compared to baseline for every observation per country as a PERCENTAGE
  for (j in Cntry1:Cntry2) {
    bankClean$adjBankRate[j] <- (bankClean$bankruptcy[j] - bankRate) / bankRate
  }
}

# Convert year and month to type double
bankClean$year <- as.double(bankClean$year)
bankClean$month <- as.double(bankClean$month)
