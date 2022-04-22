# Combine the unemployment, vaccination rate, and COVID cases datasets where rows only in one dataset are still added
df <- full_join(unempDataClean, vacDataClean, by = c('country', 'year', 'month'))
df <- full_join(df, casesDataClean, by = c('country', 'year', 'month'))

# Set any NA values to 0. NA values occur because the dataset for vaccination does not go back to Jan 2020
df[is.na(df)] <- 0


# New cat. variable -------------------------------------------------------

# Template: How to create cat. variables
#   Leave temp_var unchanged
#   Change the columns that you want to combine into a categorical variable
#   Determine how you want to split up the variable
#   Change cases_cat to the name of the new cat.var you want to create
# 

temp_var = df$new_cases_per_million * (1 - 0.7 * (df$vacRateFirstDose/100))

summary(temp_var)
boxplot(temp_var)
hist(temp_var)

df$cases_cat <- as.factor(
  ifelse(temp_var == 0, 'none',
  ifelse(temp_var < quantile(temp_var, .25)[[1]],'low',
  ifelse(temp_var < quantile(temp_var, .50)[[1]], 'medium',
  ifelse(temp_var < quantile(temp_var, .75)[[1]], 'high', 'very_high')))))

# Model -------------------------------------------------------------------


# OLS 
mdlA <- adjUnemp ~ icu_patients_per_million + cases_cat

rsltA <- lm(mdlA, df)

predA <- predict(rsltA, df)

str(rsltA)

predA

plot(rsltA)
