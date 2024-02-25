# Reading CSV
cars_df <- read.csv('WranglerElantra2018.csv')

# Train-test split (2017 before and after)
train <- subset(cars_df, Year <= 2017) 
test  <- subset(cars_df, Year >  2017)


#### A Part i ####
# 5 Var regression model
model.w.all <- lm(Wrangler.Sales ~ Year+Unemployment.Rate+Wrangler.Queries+CPI.Energy+CPI.All, data=train)
summary(model.w.all)

# All variables except CPI.All are significant;
# CPI.All has a p-value of 0.11971 > 0.05.


#### A Part ii-1 ####
model.w.sub <-lm(Wrangler.Sales ~ Year+Unemployment.Rate+Wrangler.Queries+CPI.Energy, data=train)
summary(model.w.sub)

# The 4 chosen variables are Year, Unemployment.Rate, Wrangler.Queries and CPI.Energy.
# The variable CPI.All is dropped as it has the highest p-value of 0.11971 and is not statistically significant to the model.
# CPI.All also has a weak linear relationship with variable Wrangler.Sales.
# All variables in the new linear model have p-values < 0.001.
# Change in R-squared is minimal from 0.8078 to 0.8025.
# Change in adjusted R-squared is minimal from 0.7971 to 0.7938.
# F-statistic increased from 75.64 to 92.44, indicating a more significant result.


#### A Part ii-2 ####
model.w.sub$coefficients
# Regression equation:
# Wrangler Sales = 5255036 − 2608 × Year − 2458 × Unemployment Rate + 315.1 × Wrangler Queries + 33.73 × CPI Energy
# Intercept (5255036): Expected value of Wrangler Sales when all the independent variables are zero (impossible as year cannot be 0).
# Year (-2608): Addition of one year will decrease 2608 units in Wrangler Sales with other variables constant. This means sales would decrease over time.
# Unemployment Rate (−2458): Addition of one unit increase in Unemployment Rate will decrease 2458 units in Wrangler Sales with other variables constant. This means that lower car sales can be expected during economic downturns that increase unemployment rate.
# Wrangler Queries (315.1): Addition of one unit increase in Wrangler Queries will increase 315.1 units in Wrangler Sales with other variables constant. This means higher crowd interest in the Wrangler will increase sales.
# CPI Energy (33.73): Addition of one unit increase in CPI Energy will increase 33.73 units in Wrangler Sales with other variables constant. An increase in energy prices is associated with a slight increase in Wrangler sales.
  
  
#### A Part ii-3 ####
# Yes, the signs of the model's coefficients make sense based on economic intuition: 
# Year: The negative coefficient (-2608) suggests that as the year increases (representing the passage of time), Wrangler sales decrease. This is consistent with the idea that over time, consumer preferences, market conditions, and competitive factors may change to reduce demand for the Wrangler, potentially leading to declines in sales. 
# Unemployment Rate: The negative coefficient (-2458) indicates that higher unemployment rates are associated with lower Wrangler sales. This aligns with economic theory, as higher unemployment rates typically lead to reduced consumer confidence and spending, particularly on big-ticket items like automobiles. 
# Wrangler Queries: The positive coefficient (315.1) implies that higher volumes of Google searches for "jeep wrangler" are associated with higher Wrangler sales. This makes sense, as increased consumer interest and online activity may indicate higher demand for the product, leading to increased sales. 
# CPI Energy: The positive coefficient (33.73) suggests that higher energy prices, as reflected by the Consumer Price Index for Energy, are associated with higher Wrangler sales. This could be interpreted as consumers opting for more fuel-efficient vehicles like the Wrangler in response to rising energy costs, thereby driving up sales.

  
#### A Part iii ####
# OSR^2 function
osr2 <- function(train, test, pred) {
  SSE <- sum((test - pred)^2)
  SST <- sum((test - mean(train))^2)
  1 - SSE / SST
}
# calculates OSR^2 for Wrangler given model
osr2.wrangler <- function(model) {
  osr2(
    train = train$Wrangler.Sales,
    test  = test$Wrangler.Sales,
    pred  = predict(model, newdata = test)
  )
}
# calculates OSR^2 for Elantra given model
osr2.elantra <- function(model) {
  osr2(
    train = train$Elantra.Sales,
    test  = test$Elantra.Sales,
    pred  = predict(model, newdata = test)
  )
}

# Calculating OSR^2 for model.w.sub
osr2.wrangler(model.w.sub)  # 0.5491

# R^2:
# Multiple R-squared 0.8025 indicates 80.25% of the variability in Wrangler sales is explained by the model using the predictors, and being near to 1 indicates model fits the training data well.
# Adjusted R-squared of 0.7938 indicates a good fit of training data.
# F-statistic of 92.44 with a p-value < 2.2e-16 indicates model is statistically significant.

# OSR^2:
# Out-of-sample R-squared value of 0.5491 indicates model performs better than the baseline model in predicting Wrangler sales.
# A higher R-squared indicates a good fit on training data and a positive OSR^2 value indicates that the model's ability to generalize on new data is good.


#### B ####
# The trend is generally increasing with cycles of peaks and troughs about the trend line.
# The increasing accessibility of the internet and Jeep's marketing efforts, among other macro factors, could explain the generally increasing trend.
# On the other hand, the periodic fluctuations could be related to more seasonal or specific events, such as new model releases or seasonal demands.


#### C Part i ####
# New linear model with Month.Factor variable
model.w.mth <- lm(Wrangler.Sales ~ Month.Factor+Year+Unemployment.Rate+Wrangler.Queries+CPI.Energy, data = train)
summary(model.w.mth)

# Regression Equation
# Wrangler.Sales = (3.996894e+06) -3.869343e+03*Month.FactorJanuary - 3.096908e+03*Month.FactorFebruary - 4.521229e+02*Month.FactorMarch + 1.363715e+03*Month.FactorMay + 1.731028e+00*Month.FactorJune - 4.652989e+02*Month.FactorJuly - 9.071317e+02*Month.FactorAugust - 2.236498e+03*Month.FactorSeptember - 2.696888e+03*Month.FactorOctober - 3.832779e+03*Month.FactorNovember - 2.495233e+03*Month.FactorDecember - 1.974273e+03*Year - 3.082858e+03*Unemployment.Rate + 1.354675e+02*Wrangler.Queries + 2.721454e+01*CPI.Energy
# Each month's coefficient represents the average difference in sales compared to the base month (April in this case).


#### C Part ii ####
# Calculating OSR^2
osr2.wrangler(model.w.mth)  # 0.6494

# The significant variables, including the months, are: Year, Unemployment.Rate, Wrangler.Queries, CPI.Energy, Month.FactorDecember, Month.FactorFebruary, Month.FactorJanuary, Month.FactorMay, Month.FactorNovember, Month.FactorOctober, and Month.FactorSeptember.
# Training set multiple R^2 is 0.9092.
# OSR^2 is 0.6494.


#### C Part iii ####
# Adding the independent variable Month.Factor has improved the quality of the model.
# The training set R^2 value has increased, meaning the model predictors better explain the variability in Wrangler sales and fit the training data better.
# The OSR^2 value has also increased, suggesting the model is better at generalizing on new data.
# Since certain months might have seen higher or lower Wrangler sales than average, adding Month.Factor provided more temporal information, enabling the model to capture the seasonal nature of Wrangler sales.


#### C Part iv ####
# Another approach to modeling seasonality could involve creating dummy variables for specific seasons rather than individual months. For example, instead of representing each month separately, we could create dummy variables for each season (e.g., winter, spring, summer, fall). This approach would capture broader seasonal trends and may provide a clearer interpretation of the effects of seasonality on sales.
# In addition, Month.Numeric could be used instead of its categorical counterpart, representing the specific order of months.
# To capture the cyclical nature of months, the sine function can also be applied over Month.Numeric to create a new variable.
# An example of a model using Month.Numeric and Month.Sine is as follows:

train$Month.Sine <- sin(pi * (train$Month.Numeric - 1) / 12)
test$Month.Sine  <- sin(pi * (test$Month.Numeric - 1) / 12)
model.w.sin <- lm(
  Wrangler.Sales ~ Unemployment.Rate+CPI.Energy+Wrangler.Queries+
    Year*Month.Sine+Month.Sine*Month.Numeric+Year:Month.Sine:Month.Numeric,
  data = train
)
summary(model.w.sin)  # multiple R^2 = 0.9109, adjusted R^2 = 0.9016
osr2.wrangler(model.w.sin)  # 0.7186

# The model above offers even higher multiple R^2 and OSR^2 values compared to model.w.mth, suggesting an improvement.


#### C Part v ####
# Linear Model for Elantra
# The Year variable has been removed as it has the highest p-value in a model with all 5 independent variables.
model.e <- lm(Elantra.Sales ~ Unemployment.Rate+Elantra.Queries+CPI.Energy+CPI.All, data = train)
summary(model.e)
osr2.elantra(model.e)  # -0.7098625

# R-squared is 0.3509, Adjusted R-squared is 0.3224.
# OSR^2 is -0.7099.

#### C Part vi ####
cor(cars_df[, c("Elantra.Sales", "Year", "Unemployment.Rate", "Elantra.Queries", "CPI.All", "CPI.Energy")])
#                   Elantra.Sales       Year Unemployment.Rate Elantra.Queries    CPI.All CPI.Energy
# Elantra.Sales         1.0000000  0.2513623        -0.2869429       0.3633973  0.2999192  0.1426053

# Correlation between Elantra.Sales and other variables, i.e. Year (0.2513623), Unemployment.Rate (-0.2869429), Elantra.Queries (0.3633973), CPI.All (0.2999192), and CPI.Energy (0.1426053) range from weak to moderate in comparison to Wrangler's correlation.
# This can explain the model's poor fit over training data and a worse generalization over new data than baseline.
