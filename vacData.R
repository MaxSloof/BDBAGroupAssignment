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
  yr <- str_split(vacData$YearWeekISO[i], "-W")[[1]][1]
  wk <- str_split(vacData$YearWeekISO[i], "-W")[[1]][2]
  if(wk >= 53) {wk <- 52} # lubridate cannot handle 53+ in a year
  
  vacData$Year[i] <- yr
  vacData$Month[i] <- month(as.Date(paste0(yr, "-", wk, "-", 1), format = "%Y-%U-%u"))
}

# Exclude the figures from March and April 2022 as they might not be accurate
vacData <- vacData %>% filter(!(Year == 2022 & Month >= 3))

# Create a dataset where all the vaccination figures are summed to monthly basis. FirstDose and SecondDose are still just the vaccinations in that month
vacDataMonth <- vacData %>% group_by(Year, Month, ReportingCountry) %>% summarise(FirstDose = sum(FirstDose), SecondDose = sum(SecondDose), Population = max(Population))
vacDataMonth <- arrange(.data = vacDataMonth, ReportingCountry, Year, Month)

# Go through the dataset and sum the vaccinations per month per country with all the vaccinations p.m. before that.
for (i in 1:length(vacCountry)) {
  Cntry1 <- match(vacCountry[i], vacDataMonth$ReportingCountry)
  Cntry2 <- match(vacCountry[i+1], vacDataMonth$ReportingCountry) - 1
  if(is.na(Cntry2)) {Cntry2 <- nrow(vacDataMonth)}
  
  for (j in Cntry1:Cntry2) {
    vacDataMonth[j,7] <- sum(vacDataMonth$FirstDose[Cntry1:j])
    vacDataMonth[j,8] <- sum(vacDataMonth$SecondDose[Cntry1:j])
  }
}
colnames(vacDataMonth)[7:8] <- c("FirstDoseTotal", "SecondDoseTotal")

# Calculate the vaccination rate for every month
for (i in 1:length(vacCountry)) {
  Cntry1 <- match(vacCountry[i], vacDataMonth$ReportingCountry)
  Cntry2 <- match(vacCountry[i+1], vacDataMonth$ReportingCountry) - 1
  if(is.na(Cntry2)) {Cntry2 <- nrow(vacDataMonth)}             
                
  for (j in Cntry1:Cntry2) {
    vacDataMonth[j,9] <- round(vacDataMonth$FirstDoseTotal[j] / vacDataMonth$Population[j] * 100, 5)
    vacDataMonth[j,10] <- round(vacDataMonth$SecondDoseTotal[j] / vacDataMonth$Population[j] * 100, 5)
  }
}
colnames(vacDataMonth)[9:10] <- c("VacRateFirstDose", "VacRateSecondDose")

