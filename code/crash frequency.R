#crash frequency analysis
rm(list=ls())
gc()
library(MASS)
library(car)
library(DescTools)
library(partykit)

#load data
data <- read.csv("G:/*****/tlrh crash frequency dataset.csv", header = T, stringsAsFactors = F)
summary(data)

#data split
set.seed(1234)
trainindex <- sample(1:nrow(data), 118650, replace = F)
trainset <- data[trainindex,]
testset <- data[-trainindex,]

#total frequency as dependent var
#baseline NB
nb_total_base <- glm.nb(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + offset(lnlength), data = trainset)
summary(nb_total_base)
AIC(nb_total_base)
logLik(nb_total_base)

#MOB
#Poisson
mob1 <- glmtree(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + offset(lnlength) | speed + pass_zone,
                data = trainset, family = poisson, maxdepth = 3)
summary(mob1)
AIC(mob1)
logLik(mob1)

#fixed overdispersion parameter
mob2 <- glmtree(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + offset(lnlength) | speed + pass_zone,
              data = trainset, family = negative.binomial(1.9446), maxdepth =3 )
summary(mob2)
summary(mob2, node=5)
AIC(mob2)
logLik(mob2)
plot(mob2)

#NB with splitting var
nb_total_improve1 <- glm.nb(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + speed + pass_zone + 
                              offset(lnlength), data=trainset)
summary(nb_total_improve1)
AIC(nb_total_improve1)
logLik(nb_total_improve1)

#NB with interaction b/w splitting var
data <- data %>% mutate(pass_speed1 = ifelse(pass_zone == 0 & speed <= 50, 1, 0),
                        pass_speed2 = ifelse(pass_zone == 0 & speed > 50, 1, 0),
                        pass_speed3 = ifelse(pass_zone == 1 & speed <= 45, 1, 0),
                        pass_speed4 = ifelse(pass_zone == 1 & speed > 45, 1, 0))
trainset <- data[trainindex,]
testset <- data[-trainindex,]

nb_total_improve2 <- glm.nb(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + pass_zone +
                              pass_speed1 + pass_speed3 + offset(lnlength), data = trainset)
summary(nb_total_improve2)
AIC(nb_total_improve2)
logLik(nb_total_improve2)

#prediction
pred1 <- predict(nb_total_base, testset, type = "response")
MSE(pred1, testset$total, na.rm = T)
pred2 <- predict(mob1, testset, type = "response")
MSE(pred2, testset$total, na.rm = T)
pred3 <- predict(mob2, testset, type="response")
MSE(pred3, testset$total, na.rm = T)
pred4 <- predict(nb_total_improve1, testset, type = "response")
MSE(pred4, testset$total, na.rm = T)
pred5 <- predict(nb_total_improve2, testset, type = "response")
MSE(pred5, testset$total, na.rm = T)

#100 iterations to check stability
pred_mx <- as.data.frame(matrix(NA, nrow = 100, ncol = 11))
names(pred_mx) <- c("Theta", "NB_TRAIN", "NB_TEST", "MOBP_TRAIN", "MOBP_TEST", "MOBNB_TRAIN", "MOBNB_TEST",
                    "NBIMP1_TRAIN", "NBIMP1_TEST", "NBIMP2_TRAIN", "NBIMP2_TEST")
for(i in 1:100){
  #split data   
  set.seed(i)
  trainindex <- sample(1:nrow(data), 118650, replace = F)
  trainset <- data[trainindex,]
  testset <- data[-trainindex,]
  #nb
  nb_base <- glm.nb(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + offset(lnlength), data = trainset)
  pred_mx[i,1] <- nb_base$theta
  pred_mx[i,2] <- MSE(predict(nb_base, trainset, type = "response"), trainset$total, na.rm = T)
  pred_mx[i,3] <- MSE(predict(nb_base, testset, type = "response"), testset$total, na.rm = T)
  #mob-p
  mob1 <- glmtree(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + offset(lnlength) | speed + pass_zone,
                  data = trainset, family = poisson, maxdepth = 3)
  pred_mx[i,4] <- MSE(predict(mob1, trainset, type = "response"), trainset$total, na.rm = T)
  pred_mx[i,5] <- MSE(predict(mob1, testset, type = "response"), testset$total, na.rm = T)
  #mob-nb
  mob2 <- glmtree(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + offset(lnlength) | speed + pass_zone,
                  data = trainset, family = negative.binomial(pred_mx[i,1]), maxdepth = 3)
  pred_mx[i,6] <- MSE(predict(mob2, trainset, type = "response"), trainset$total, na.rm = T)
  pred_mx[i,7] <- MSE(predict(mob2, testset, type = "response"), testset$total, na.rm = T)
  #nb_improve1
  nb_imp1 <- glm.nb(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + speed + pass_zone + offset(lnlength),
                    data = trainset)
  pred_mx[i,8] <- MSE(predict(nb_imp1, trainset, type = "response"), trainset$total, na.rm = T)
  pred_mx[i,9] <- MSE(predict(nb_imp1, testset, type = "response"), testset$total, na.rm = T)
  #nb_improve2
  nb_imp2 <- glm.nb(total ~ lnaadt + acc_den + curve_den + curdeg_seg + sh_rs + pass_zone + pass_speed1 +
                      pass_speed3 + offset(lnlength), data = trainset)
  pred_mx[i,10] <- MSE(predict(nb_imp2, trainset, type = "response"), trainset$total, na.rm = T)
  pred_mx[i,11] <- MSE(predict(nb_imp2, testset, type = "response"), testset$total, na.rm = T)
}

#average over iterations
pred_sum <- as.data.frame(matrix(NA, nrow=2, ncol=10))
names(pred_sum) <- c("NB_TRAIN", "NB_TEST", "MOBP_TRAIN", "MOBP_TEST", "MOBNB_TRAIN", "MOBNB_TEST",
                     "NBIMP1_TRAIN", "NBIMP1_TEST", "NBIMP2_TRAIN", "NBIMP2_TEST")
for(i in 1:10){
  pred_sum[1,i] <- mean(pred_mx[,i+1])
  pred_sum[2,i] <- sqrt(var(pred_mx[,i+1]))
}

