# load libraries
library(readxl)

sbc <- read_excel("SBC.xlsx")
colnames(sbc) <- c(
  "Year",
  "AnnualSales",
  "Hops",
  "Malt",
  "Advertising",
  "Bitterness",
  "Investment"
)
sbc.train <- subset(sbc, Year <= 40)
sbc.test  <- subset(sbc, Year >  40)

get_r_squared <- function(m) {
  pred <- predict(m, newdata = sbc.test)
  SSE <- sum((pred - sbc.test$AnnualSales)^2)
  SST <- sum((mean(sbc.train$AnnualSales) - sbc.test$AnnualSales)^2)
  return(1 - SSE / SST)
}

mod.1 <- lm(AnnualSales ~ Year+Hops+Malt+Advertising+Bitterness+Investment,
              data = sbc.train)
summary(mod.1)
get_r_squared(mod.1)

mod.2 <- lm(AnnualSales ~ Year+Hops+Malt+Advertising+Bitterness, data = sbc.train)
summary(mod.2)
get_r_squared(mod.2)


