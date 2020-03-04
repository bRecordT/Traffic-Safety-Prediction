#crash severity analysis
#baseline binary logit model
#select variable based on rf imp rank
rm(list=ls())
gc()
library(dummies)
library(MASS)
library(car)
library(DescTools)
library(caret)

#load data
data <- read.csv("G:/*****/severity data.csv", header = T, stringsAsFactors = F)
data$Inj_level <- ifelse(data$Inj_level == 3, 0, 1)

#create indicators
#categorical
data <- dummy.data.frame(data, names = c("Veh_type", "Col_type", "Quarter", "Rdway_cond", "Rdway_surf"))
#continuous
data$SpeedLow <- ifelse(is.na(data$Speed), NA, ifelse(data$Speed <= 45, 1, 0))
data$SpeedHigh <- ifelse(is.na(data$Speed), NA, ifelse(data$Speed > 45, 1, 0))

data$AgeYoung <- ifelse(is.na(data$Age), NA, ifelse(data$Age <= 20, 1, 0))
data$AgeMid <- ifelse(is.na(data$Age), NA, ifelse(data$Age >20 & data$Age < 65, 1, 0))
data$AgeOld <- ifelse(is.na(data$Age), NA, ifelse(data$Age >= 65, 1, 0))

#data split
set.seed(1234)
trainindex <- sample(1:nrow(data), 280700, replace = F)
trainset <- data[trainindex,]
testset <- data[-trainindex,]

#only top 10 vars
bimodel2 <- glm(Inj_level ~ Seatbelt + Veh_type1 + Veh_type2 + Veh_type3 + Veh_type4 + Veh_type5 + Age + 
                  Col_type1 + Col_type2 + Col_type3 + Col_type4 + Col_type5 + Col_type6 + Col_type7 + 
                  Col_type8 + Drug + Quarter1 + Quarter2 + Quarter3 + Alcohol + Gender + 
                  SpeedHigh + Crash_loc, family = binomial(link = 'logit'), data = trainset)
summary(bimodel2)
PseudoR2(bimodel2)
logLik(bimodel2)
#predict
predglm1 <- predict(bimodel2, trainset, type = "response")
predglm1 <- as.data.frame(predglm1)
predglm1 <- ifelse(predglm1 >= 0.5, 1, 0)
predglm2 <- predict(bimodel2, testset, type = "response")
predglm2 <- as.data.frame(predglm2)
predglm2 <- ifelse(predglm2 >= 0.5, 1, 0)
#positive class is Fat_inj
confusionMatrix(as.factor(predglm1), as.factor(trainset$Inj_level), positive = "1")
confusionMatrix(as.factor(predglm2), as.factor(testset$Inj_level), positive = "1")

#include all variable
bimodel3 <- glm(Inj_level ~ Seatbelt + Veh_type1 + Veh_type2 + Veh_type3 + Veh_type4 + Veh_type5 + Age + Col_type1 +
                  Col_type2 + Col_type3 + Col_type4 + Col_type5 + Col_type6 + Col_type7 + Col_type8 + Drug + 
                  Quarter2 + Quarter3 + Alcohol + Gender + SpeedHigh + Crash_loc + Peak_hour + Urban + Fati_asleep +
                  Rdway_surf1 + Rdway_surf2 + Curve + Divided_way + Weekend + License, 
                family = binomial(link = 'logit'), data = trainset)
summary(bimodel3)
PseudoR2(bimodel3)
logLik(bimodel3)
#predict
predglm3 <- predict(bimodel3, trainset, type = "response")
predglm3 <- as.data.frame(predglm3)
predglm3 <- ifelse(predglm3 >= 0.5, 1, 0)
predglm4 <- predict(bimodel3, testset, type = "response")
predglm4 <- as.data.frame(predglm4)
predglm4 <- ifelse(predglm4 >= 0.5, 1, 0)
confusionMatrix(as.factor(predglm3), as.factor(trainset$Inj_level), positive = "1")
confusionMatrix(as.factor(predglm4), as.factor(testset$Inj_level), positive = "1")
