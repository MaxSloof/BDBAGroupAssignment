library(readr)

oxData <- read.csv("oxData.csv")
countryCodes <- read_csv("countryCodes.csv")

# Transform three letter country code by two letter country code
oxData$CountryCode <- countryCodes$ISO2[match(oxData$CountryCode, countryCodes$ISO3)]

# Only use countries that are from the EU
regData <- oxData %>% filter(CountryCode %in% vacCountry)

remove(oxData)

