data <- read.csv("WranglerElantra2018.csv")

# Part a.

# split by year 2017
data.train <- subset(data, Year <= 2017)
data.test  <- subset(data, Year >  2017)

# calculates OSR^2 given
# training, test, and predicted y values
osr2 <- function(train, test, pred) {
  SSE <- sum((pred - test)^2)
  SST <- sum((mean(train) - test)^2)
  1 - SSE / SST
}
# calculates OSR^2 for Wrangler given model
osr2.wrangler <- function(mod) {
  osr2(
    train = data.train$Wrangler.Sales,
    test = data.test$Wrangler.Sales,
    pred = predict(mod, newdata = data.test)
  )
}
# calculates OSR^2 for Elantra given model
osr2.elantra <- function(mod) {
  osr2(
    train = data.train$Elantra.Sales,
    test = data.test$Elantra.Sales,
    pred = predict(mod, newdata = data.test)
  )
}

# Part a.i.
mod.wrangler.1 <- lm(
  Wrangler.Sales ~ Year+Unemployment.Rate+CPI.Energy+CPI.All+Wrangler.Queries,
  data = data.train
)
summary(mod.wrangler.1)
# all variables except CPI.All are significant
osr2.wrangler(mod.wrangler.1)  # 0.4953

# Part a.ii.
# variable CPI.All is excluded
mod.wrangler.2 <- lm(
  Wrangler.Sales ~ Year+Unemployment.Rate+CPI.Energy+Wrangler.Queries,
  data = data.train
)
summary(mod.wrangler.2)
# all variables have p-value < 0.001;
# multiple R^2 = 0.8025 => good at predicting
# training set observations
osr2.wrangler(mod.wrangler.2)  # 0.5491
# only slightly better than baseline at predicting
# test set observations

mod.wrangler.2$coefficients

# Part b.
# trend is overall increasing with up-down cycles;
# makes sense with new model releases or seasonal demands changes

# Part c.i.
mod.wrangler.3 <- lm(
  Wrangler.Sales ~ Year+Unemployment.Rate+CPI.Energy+Wrangler.Queries+Month.Factor,
  data = data.train
)
summary(mod.wrangler.3)
# Jan, Feb, May, Sep, Oct, Nov, Dec are significant
# => more likely to affect demand;

# Part c.ii.
# multiple R^2 = 0.9092
osr2.wrangler(mod.wrangler.3)  # 0.6494

# Part c.iii.
# Month.Factor has improved quality of model by providing more information

# Part c.iv.
# use sine to capture cyclical nature of months
data.train$Month.Sine <- sin(pi * (data.train$Month.Numeric - 1) / 12)
data.test$Month.Sine  <- sin(pi * (data.test$Month.Numeric - 1) / 12)
mod.wrangler.4 <- lm(
  Wrangler.Sales ~ Unemployment.Rate+CPI.Energy+Wrangler.Queries+
    Year*Month.Sine+Month.Sine*Month.Numeric+Year:Month.Sine:Month.Numeric,
  data = data.train
)
summary(mod.wrangler.4)  # multiple R^2 = 0.9109
osr2.wrangler(mod.wrangler.4)  # 0.7186

# Part c.v.
# use all 5 variables as a baseline
mod.elantra.1 <- lm(
  Elantra.Sales ~ Year+Unemployment.Rate+Elantra.Queries+CPI.Energy+CPI.All,
  data = data.train
)
summary(mod.elantra.1)
# Year and CPI.All have p-value > 0.05
osr2.elantra(mod.elantra.1)  # -0.5642

# Year is removed first for having a higher p-value than CPI.All
mod.elantra.2 <- lm(
  Elantra.Sales ~ Unemployment.Rate+Elantra.Queries+CPI.Energy+CPI.All,
  data = data.train
)
summary(mod.elantra.2)
# multiple R^2 = 0.3509
osr2.elantra(mod.elantra.2)  # -0.7099

# Part c.vi.
cor(data[,c("Elantra.Sales", "Year", "Unemployment.Rate",
            "Elantra.Queries", "CPI.All", "CPI.Energy")])
# Elantra.Sales is much less correlated to the 5 independent variables
# compared to Wrangler.Sales with its variables.

#                   Elantra.Sales       Year Unemployment.Rate Elantra.Queries    CPI.All CPI.Energy
# Elantra.Sales         1.0000000  0.2513623        -0.2869429       0.3633973  0.2999192  0.1426053

cor(data[,c("Wrangler.Sales", "Year", "Unemployment.Rate",
            "Wrangler.Queries", "CPI.All", "CPI.Energy")])
