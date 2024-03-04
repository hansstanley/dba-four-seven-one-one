library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

osr2 <- function(pred, test, train) {
  SSE <- sum((test - pred)^2)
  SST <- sum((test - mean(train))^2)
  1 - SSE/SST
}

data <- read.csv("readmission.csv")

# Part a.

sum(is.na(data))
# -> there are 19777 NA values
names(which(colSums(is.na(data)) > 0))
# -> the NA values come from columns:
#    "row", "gender", "admissionType", "admissionSource"

summary(data)

n = nrow(data)
# proportion of unplanned re-admissions
length(which(data$readmission == 1)) / n * 100  # 11.16%

cor(data[, c("readmission",
             "numberOutpatient",
             "numberEmergency",
             "numberInpatient")])
cor(data[, c(
  "readmission",
  "acarbose",
  "chlorpropamide",
  "glimepiride",
  "glipizide",
  "glyburide",
  "glyburide.metformin",
  "insulin",
  "metformin",
  "nateglinide",
  "pioglitazone",
  "repaglinide",
  "rosiglitazone"
)])
cor(data[, c(
  "readmission",
  "timeInHospital",
  "numLabProcedures",
  "numNonLabProcedures",
  "numMedications",
  "numberDiagnoses"
)])
cor(data[, c(
  "readmission",
  "diagAcuteKidneyFailure",
  "diagAnemia",
  "diagAsthma",
  "diagAthlerosclerosis",
  "diagBronchitis",
  "diagCardiacDysrhythmia",
  "diagCardiomyopathy",
  "diagCellulitis",
  "diagCKD",
  "diagCOPD",
  "diagDyspnea",
  "diagHeartFailure",
  "diagHypertension",
  "diagHypertensiveCKD",
  "diagIschemicHeartDisease",
  "diagMyocardialInfarction",
  "diagOsteoarthritis",
  "diagPneumonia",
  "diagSkinUlcer"
)])

# plot the proportion of readmissions against numberInpatient
ggplot(
  aggregate(readmission ~ numberInpatient, data = data, FUN = mean),
  aes(x = numberInpatient, y = readmission)
) +
  geom_point() +
  labs(x = "numberInpatient", y = "Proportion of readmission") +
  ggtitle("Proportion of readmission against numberInpatient")

# Part b.

# split data
set.seed(998)
splitter <- createDataPartition(
  data$readmission, p = 0.75, list = F
)
data.train <- data[splitter,]
data.test <- data[-splitter,]

# undersample imbalanced training data
class_0 <- data.train[data.train$readmission == 0,]
class_1 <- data.train[data.train$readmission == 1,]
min_size <- min(nrow(class_0), nrow(class_1))
class_0.sampled <- class_0[sample(nrow(class_0), min_size),]
class_1.sampled <- class_1[sample(nrow(class_1), min_size),]
data.train <- rbind(class_0.sampled, class_1.sampled)
table(data.train$readmission)

# train CART models
model.class <- rpart(
  readmission ~ .,
  data = data.train,
  method = "class",
  control = rpart.control(cp = 0.002)
)
model.anova <- rpart(
  readmission ~ .,
  data = data.train,
  method = "anova",
  control = rpart.control(cp = 0.002)
)

# visualize models
rpart.plot(model.class, roundint = F, digits = 4)
rpart.plot(model.anova, roundint = F, digits = 4)

# predict on test data
pred.class <- predict(model.class, newdata = data.test)[, 2]
pred.anova <- predict(model.anova, newdata = data.test)

# out-of-sample R-squared values
osr2(pred.class, data.test$readmission, data.train$readmission)
osr2(pred.anova, data.test$readmission, data.train$readmission)

# threshold at 0.5
pred.class.thold <- as.integer(pred.class > 0.5)
pred.anova.thold <- as.integer(pred.anova > 0.5)

# confusion matrices
confusionMatrix(as.factor(pred.class.thold),
                as.factor(data.test$readmission),
                positive = "1")
confusionMatrix(as.factor(pred.anova.thold),
                as.factor(data.test$readmission),
                positive = "1")
