library(readr)
library(dplyr)

oxData <- read.csv("oxData.csv")
countryCodes <- read_csv("countryCodes.csv")

# Transform three letter country code by two letter country code
oxData$CountryCode <- countryCodes$ISO2[match(oxData$CountryCode, countryCodes$ISO3)]

# Identify variables
oxCols <- c("CountryCode", "Date", "ContainmentHealthIndex", "EconomicSupportIndex")


# Only use countries that are from the EU
oxDataFiltered <- oxData[oxCols] %>% filter(CountryCode %in% vacCountry)

remove(oxData)

oxDataFiltered$CountryCode <- factor(oxDataFiltered$CountryCode)
oxCountries <- levels(oxDataFiltered$CountryCode)




for(i in 1:nrow(oxDataFiltered)) {
  oxDataFiltered$year[i] <- substr(oxDataFiltered$Date[i], start = 0, stop = 4)
  oxDataFiltered$month[i] <- substr(oxDataFiltered$Date[i], start = 5, stop = 6)
}

colnames(oxDataFiltered)[1:2] <- c("country", "date")

oxDataFiltered$year <- as.integer(oxDataFiltered$year)
oxDataFiltered$month <- as.integer(oxDataFiltered$month)

# Transform to per month

oxDataClean <- oxDataFiltered %>% group_by(country, year, month) %>% 
  summarise(containmentHealth = mean(ContainmentHealthIndex), 
                economicSupport = mean(EconomicSupportIndex))

oxDataClean <- oxDataClean %>% filter(!(year == 2022 & month >= 03))


# Checken wat C enzo allemaal betekent

# Alle measure-variabelen (c1, c2, enz) omzetten naar gemiddelde per maand

# Gemiddelden nemen van alle Cs / Hs enz

# Kleurcode --> zeggen in meeting