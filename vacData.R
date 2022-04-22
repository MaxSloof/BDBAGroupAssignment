library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Read CSV
vacData <- read_csv("vacData.csv")

# Country code as type 'factor'
vacData$ReportingCountry <- factor(vacData$ReportingCountry)

# Create subset of columns that are needed
subVacCols <- c("YearWeekISO", "ReportingCountry", "Population", "FirstDose", "SecondDose", "TargetGroup", "Region")
vacData <- vacData[subVacCols]

# Variable with all the countries
vacCountry <- levels(vacData$ReportingCountry)

# Only use vaccination data for whole country instead of per age group or specific regions
vacData <- vacData %>% filter(TargetGroup == "ALL" & Region == ReportingCountry)

# Transform the Year-Week notation into Year and Month
for (i in 1:nrow(vacData)) {
  
  # Split year-week notation into year and week separated
  yr <- str_split(vacData$YearWeekISO[i], "-W")[[1]][1]
  wk <- str_split(vacData$YearWeekISO[i], "-W")[[1]][2]
  if(wk >= 53) {wk <- 52} # lubridate cannot handle 53+ in a year
  
  # Assign the year and month value to a new column
  vacData$year[i] <- yr
    # This function calculates the corresponding month to the week
  vacData$month[i] <- month(as.Date(paste0(yr, "-", wk, "-", 1), 
                                    format = "%Y-%U-%u"))
}

# Exclude the figures from March and April 2022 as they might not be accurate
vacData <- vacData %>% filter(!(Year == 2022 & Month >= 3))

# Create a dataset where all the vaccination figures are summed to monthly basis. FirstDose and SecondDose are still just the vaccinations in that month
vacDataClean <- vacData %>% group_by(Year, Month, ReportingCountry) %>% summarise(FirstDose = sum(FirstDose), SecondDose = sum(SecondDose), Population = max(Population))
vacDataClean <- arrange(.data = vacDataClean, ReportingCountry, Year, Month)

# Go through the dataset and sum the vaccinations per month per country with all the vaccinations p.m. before that.
for (i in 1:length(vacCountry)) {
  Cntry1 <- match(vacCountry[i], vacDataClean$ReportingCountry)
  Cntry2 <- match(vacCountry[i+1], vacDataClean$ReportingCountry) - 1
  if(is.na(Cntry2)) {Cntry2 <- nrow(vacDataClean)}
  
  for (j in Cntry1:Cntry2) {
    vacDataClean[j,7] <- sum(vacDataClean$FirstDose[Cntry1:j])
    vacDataClean[j,8] <- sum(vacDataClean$SecondDose[Cntry1:j])
  }
}

# Change column names
colnames(vacDataClean)[7:8] <- c("firstDoseTotal", "secondDoseTotal")

# Calculate the vaccination rate for every month
for (i in 1:length(vacCountry)) {
  Cntry1 <- match(vacCountry[i], vacDataClean$ReportingCountry)
  Cntry2 <- match(vacCountry[i+1], vacDataClean$ReportingCountry) - 1
  if(is.na(Cntry2)) {Cntry2 <- nrow(vacDataClean)}             
                
  for (j in Cntry1:Cntry2) {
    vacDataClean[j,9] <- round(vacDataClean$firstDoseTotal[j] / vacDataClean$Population[j] * 100, 5)
    vacDataClean[j,10] <- round(vacDataClean$secondDoseTotal[j] / vacDataClean$Population[j] * 100, 5)
  }
}

# Change column names
colnames(vacDataClean)[9:10] <- c("vacRateFirstDose", "vacRateSecondDose")
colnames(vacDataClean)[1:3] <- c("year", "month", "country")

# Align dataset format with other datasets and only keep the keys (think SQL) and the important columns
vacDataClean <- vacDataClean[c("country", "year", "month", "vacRateFirstDose")]
vacDataClean$year <- as.double(vacDataClean$year)
vacDataClean$month <- as.double(vacDataClean$month)
