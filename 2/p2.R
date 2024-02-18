data <- read.csv("WranglerElantra2018.csv")

data.train <- subset(data, Year <= 2017)
data.test  <- subset(data, Year >  2017)

osr2 <- function(train, test, pred) {
  SSE <- sum((pred - test)^2)
  SST <- sum((mean(train) - test)^2)
  1 - SSE / SST
}
osr2.wrangler <- function(mod) {
  osr2(
    train = data.train$Wrangler.Sales,
    test = data.test$Wrangler.Sales,
    pred = predict(mod, newdata = data.test)
  )
}
osr2.elantra <- function(mod) {
  osr2(
    train = data.train$Elantra.Sales,
    test = data.test$Elantra.Sales,
    pred = predict(mod, newdata = data.test)
  )
}

mod.wrangler.1 <- lm(
  Wrangler.Sales ~ Year+Unemployment.Rate+CPI.Energy+CPI.All+Wrangler.Queries,
  data = data.train
)
summary(mod.wrangler.1)
osr2.wrangler(mod.wrangler.1)

mod.wrangler.2 <- lm(
  Wrangler.Sales ~ Year+Unemployment.Rate+CPI.Energy+Wrangler.Queries,
  data = data.train
)
summary(mod.wrangler.2)
osr2.wrangler(mod.wrangler.2)

mod.wrangler.3 <- lm(
  Wrangler.Sales ~ Year+Unemployment.Rate+CPI.Energy+Wrangler.Queries+Month.Factor,
  data = data.train
)
summary(mod.wrangler.3)
osr2.wrangler(mod.wrangler.3)

mod.wrangler.4 <- lm(
  Wrangler.Sales ~ Year+Unemployment.Rate+CPI.Energy+Wrangler.Queries+Month.Numeric,
  data = data.train
)
summary(mod.wrangler.4)
osr2.wrangler(mod.wrangler.4)

mod.elantra.1 <- lm(
  Elantra.Sales ~ Year+Unemployment.Rate+Elantra.Queries+CPI.Energy+CPI.All,
  data = data.train
)
summary(mod.elantra.1)
osr2.elantra(mod.elantra.1)

mod.elantra.2 <- lm(
  Elantra.Sales ~ Unemployment.Rate+Elantra.Queries+CPI.Energy,
  data = data.train
)
summary(mod.elantra.2)
osr2.elantra(mod.elantra.2)

cor(data[,c("Elantra.Sales", "Year", "Unemployment.Rate",
            "Elantra.Queries", "CPI.All", "CPI.Energy")])
