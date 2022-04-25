library(dplyr)

# Combine the unemployment, vaccination rate, and COVID cases datasets where rows only in one dataset are still added
dfPrelim <- full_join(unempDataClean, bankClean[c(1,3,5:7)], by = c('country', 'year', 'month'))
dfPrelim <- full_join(dfPrelim, vacDataClean, by = c('country', 'year', 'month'))
dfPrelim <- full_join(dfPrelim, casesDataClean, by = c('country', 'year', 'month'))
dfPrelim <- full_join(dfPrelim, oxDataClean, by = c('country', 'year', 'month'))
dfPrelim <- full_join(dfPrelim, gdpDataClean, by = c('country', 'year', 'month'))


# Set any NA values to 0. NA values occur because the dataset for vaccination does not go back to Jan 2020
dfPrelim[is.na(dfPrelim)] <- 0

dfPrelim$country <- factor(dfPrelim$country)

# Pick which countries you want
df <- dfPrelim %>% filter(country %in% bankCountries)
df$country <- factor(df$country)

# Remove extremely high values from bankrupcy rate variable
df <- df[-which(df$adjBankRate > 10),]

# New cat. variable -------------------------------------------------------

# Template: How to create cat. variables
#   Leave temp_var unchanged
#   Change the columns that you want to combine into a categorical variable
#   Determine how you want to split up the variable
#   Change cases_cat to the name of the new cat.var you want to create
# 

temp_var <- df$new_cases_per_million * (1 - 0.7 * (df$vacRateFirstDose/100))

summary(temp_var)
boxplot(temp_var)
hist(temp_var)

df$cases_cat <- as.factor(
  ifelse(temp_var == 0, 'none',
  ifelse(temp_var < quantile(temp_var, .25)[[1]],'low',
  ifelse(temp_var < quantile(temp_var, .50)[[1]], 'medium',
  ifelse(temp_var < quantile(temp_var, .75)[[1]], 'high', 'very_high')))))

temp_var <- df$adjBankRate
summary(temp_var)
boxplot(temp_var)
hist(temp_var)

# What should be the border for the categories
#   In this case, a large difference is a percentage change of more than 50% from one period compared to jan 2020
#   A good alternative: max(0 - min(temp_var), max(temp_var)) / 2
#     This is the largest range between the 0 and the positive or negative side and divide that by 2
temp_lim <- 0.5 


df$adjBankRate_cat <- as.factor(
  ifelse(temp_var == 0, 'none',
         ifelse(temp_var < -temp_lim,'large_decrease',
                ifelse(temp_var < 0, 'small_decrease',
                       ifelse(temp_var < temp_lim, 'small_increase', 'large_increase')))))



# Create lag variable -----------------------------------------------------

# Lag of 1 for cases_cat variable
df <- df %>% group_by(country) %>% mutate(lag.cases_cat = dplyr::lag(cases_cat, n = 1, default = NA))

temp_array <- sort(df$adjBankRate, decreasing = FALSE)

hist(temp_array, xlim = c(-1,4), breaks = 1000)

which(temp_array > 10)

# Model -------------------------------------------------------------------


# OLS 
mdlA <- adjUnemp ~ icu_patients_per_million + cases_cat + adjBankRate

rsltA <- lm(mdlA, dfPrelim)

predA <- predict(rsltA, dfPrelim)

str(rsltA)

predA

plot(rsltA)


# Random Forest
library(randomForest)

dfSubCols <-c("adjBankRate_cat", "icu_patients_per_million", "cases_cat", "containmentHealth", "economicSupport", "adjUnemp", "ecoSit")

dfSub <- df[dfSubCols]

mdlB <- adjBankRate_cat ~ .

# Training and test split
pTrain <- 0.7
nTrain <- pTrain * nrow(dfSub)
obsTrain <- sample(1:nrow(dfSub), nTrain)

dfTrain <- dfSub[obsTrain,]
dfTest <- dfSub[-obsTrain, ]

rsltRF <- randomForest(
  mdlB, 
  data = dfTrain, 
  ntree = 100,
  importance = TRUE)

classRForest <- predict(rsltRF, dfTest, type = 'class')
accRForest <- mean(classRForest == dfTest$adjBankRate_cat)

table(Predicted = classRForest, Observed = dfTest$adjBankRate_cat)
