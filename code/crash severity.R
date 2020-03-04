#crash severity analysis
rm(list=ls())
gc()
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

#load data
data <- read.csv("G:/*****/severity data.csv", header = T, stringsAsFactors = F)
#exclude obs with missing values for random forest
data<-na.omit(data)
summary(data)
#re-label severity levels: 0 is PDO, 1 is fatal and injury
data$Inj_level <- ifelse(data$Inj_level == 3, 0, 1)
#convert categorical var into factor
data <- data %>% mutate_if(sapply(., max, na.rm =T) < 10, factor) %>%
  mutate(Inj_level = factor(Inj_level, labels=c("PDO", "Fat_inj"))) %>% 
  select(-CRN)

#split data
set.seed(1234)
trainindex <- createDataPartition(data$Inj_level, p = 0.8, list = F)
trainset <- data[trainindex,]
testset <- data[-trainindex,]
#CART
#example model based on 1% minimum node size
ctrl3 <- rpart.control(minbucket = 2807, cp = 0.0001)
rtree3 <- rpart(Inj_level ~ ., data = trainset, method = "class", control = ctrl3)
#prune tree
printcp(rtree3)
par(pin=c(4,3),mai=c(1,1,1,1))
plotcp(rtree3,col="red",pch=19,lty=5)
#find optimal cp using 1-SE rule
mincp <- min(rtree3$cptable[,4])
mincp_ub <- mincp + rtree3$cptable[min(which(rtree3$cptable[,4] == mincp)),5]
cp_index <- min(which(rtree3$cptable[,4] <= mincp_ub))
cp_opt <- rtree3$cptable[cp_index,1]
prtree3 <- prune(rtree3, cp = cp_opt)
#plot
rpart.plot(prtree3, type = 2, extra = 2, cex = 0.8, font = 1, digits = 4, fallen.leaves = T,
           under = F, branch.lty = 1, nn = F, ni = T, nn.cex = 0.7, nn.font = 1)
#prediction
p_tree_train <- predict(prtree3, newdata = trainset, type = "class")
p_tree_test <- predict(prtree3, newdata = testset, type = "class")
confusionMatrix(p_tree_train, trainset$Inj_level)
confusionMatrix(p_tree_test, testset$Inj_level)


#####
#effect of node size
#repeat 100 times
n_effect <- as.data.frame(matrix(NA, nrow = 100, ncol = 10))
names(n_effect) <- c("Minimum Node Size", "Terminal Node", "Train Overall Accuracy", "Train Sensitivity", 
                     "Train Precision", "Train F1 Score", "Test Overall Accuracy", "Test Sensitivity", 
                     "Test Precision", "Test F1 Score")
temp_record <- as.data.frame(matrix(NA, nrow = 100, ncol = 9))
names(temp_record) <- c("Terminal Node", "Train Overall Accuracy", "Train Sensitivity", "Train Precision", 
                        "Train F1 Score", "Test Overall Accuracy", "Test Sensitivity", "Test Precision", 
                        "Test F1 Score")
for (i in 1:100) {
  for (j in 1:100){
    #grow the tree 
    ctrl <- rpart.control(minbucket = round(280700 * i / 1000), cp = 0.0001)
    rtree <- rpart(Inj_level ~ .,data = trainset, method = "class", control = ctrl)
    #find optimal cp
    mincp <- min(rtree$cptable[,4])
    mincp_ub <- mincp + rtree$cptable[min(which(rtree$cptable[,4] == mincp)),5]
    cp_index <- min(which(rtree$cptable[,4] <= mincp_ub))
    cp_opt <- rtree$cptable[cp_index,1]
    #prune the tree
    prtree <- prune(rtree, cp_opt)
    #predict on trainset and testset
    pred_train <- predict(prtree, newdata = trainset, type = "class")
    pred_test <- predict(prtree, newdata = testset, type = "class")
    conf_mtx_train <- confusionMatrix(pred_train, trainset$Inj_level)
    conf_mtx_test <- confusionMatrix(pred_test, testset$Inj_level)
    #record evaluation metrics
    temp_record[j,1] <- rtree$cptable[cp_index,2] + 1
    temp_record[j,2] <- conf_mtx_train$overall[1]
    temp_record[j,3] <- conf_mtx_train$byClass[1]
    temp_record[j,4] <- conf_mtx_train$byClass[3]
    temp_record[j,5] <- conf_mtx_train$byClass[7]
    temp_record[j,6] <- conf_mtx_test$overall[1]
    temp_record[j,7] <- conf_mtx_test$byClass[1]
    temp_record[j,8] <- conf_mtx_test$byClass[3]
    temp_record[j,9] <- conf_mtx_test$byClass[7]
  }
  #record evaluation metrics
  n_effect[i,1] <- i / 10
  n_effect[i,2] <- mean(temp_record[,1])
  n_effect[i,3] <- mean(temp_record[,2])
  n_effect[i,4] <- mean(temp_record[,3])
  n_effect[i,5] <- mean(temp_record[,4])
  n_effect[i,6] <- mean(temp_record[,5])
  n_effect[i,7] <- mean(temp_record[,6])
  n_effect[i,8] <- mean(temp_record[,7])
  n_effect[i,9] <- mean(temp_record[,8])
  n_effect[i,10] <- mean(temp_record[,9])
}

#####
#RANDOM FOREST
#example model based on 1% minimum node size
rf3 <- randomForest(Inj_level ~ ., data = trainset, ntree = 300, mtry = 3, importance = T, nodesize = 2807)
plot(rf3, main = " ")
legend("right", colnames(rf3$err.rate), col = 1:4, cex = 0.8, fill = 1:4)
#var importance
importance(rf3)
varImpPlot(rf3, cex = 1)
#predict
p_rf_train <- predict(rf3, newdata = trainset, type = "class")
p_rf_test <- predict(rf3, newdata = testset, type = "class")
confusionMatrix(p_rf_train, trainset$Inj_level)
confusionMatrix(p_rf_test, testset$Inj_level)

#effect of mvar
#based on 1% minimum node size and 300 tree
rf_v_effect <- as.data.frame(matrix(NA, nrow = 25, ncol = 11))
v_var_imp <- as.data.frame(matrix(NA, nrow = 25, ncol = 25))
names(rf_v_effect) <- c("Number of Variables", "Overall OOB Times", "Overall OOB Error", "Train Overall Accuracy",
                        "Train Sensitivity", "Train Precision", "Train F1 Score", "Test Overall Accuracy",
                        "Test Sensitivity", "Test Precision", "Test F1 Score")
for (i in 1:25) {
  #grow the forest 
  set.seed(1234)
  rf <- randomForest(Inj_level ~ ., data = trainset, ntree = 300, mtry = i, importance = T, nodesize = 2807)
  #predict on trainset and testset
  pred_train <- predict(rf, newdata = trainset, type = "class")
  pred_test <- predict(rf, newdata = testset, type = "class")
  conf_mtx_train <- confusionMatrix(pred_train, trainset$Inj_level)
  conf_mtx_test <- confusionMatrix(pred_test, testset$Inj_level)
  #record evaluation metrics
  rf_v_effect[i,1] <- i
  rf_v_effect[i,2] <- mean(rf$oob.times)
  rf_v_effect[i,3] <- mean(rf$err.rate[,1])
  rf_v_effect[i,4]<-conf_mtx_train$overall[1]
  rf_v_effect[i,5]<-conf_mtx_train$byClass[1]
  rf_v_effect[i,6]<-conf_mtx_train$byClass[3]
  rf_v_effect[i,7]<-conf_mtx_train$byClass[7]
  rf_v_effect[i,8]<-conf_mtx_test$overall[1]
  rf_v_effect[i,9]<-conf_mtx_test$byClass[1]
  rf_v_effect[i,10]<-conf_mtx_test$byClass[3]
  rf_v_effect[i,11]<-conf_mtx_test$byClass[7]
  #record variable importance
  variable <- importance(rf)
  #importance ranking
  variable <- variable[order(-variable[,3]),]
  v_var_imp[i,] <- row.names(variable)
}
#convert into numerical ranking
v_var_rank <- as.data.frame(matrix(NA, nrow = 25, ncol = 25))
names(v_var_rank) <- sort(colnames(trainset[,-9]))
var_list <- colnames(v_var_rank)
for (i in 1:25){
  temp_rank <- which(v_var_imp == var_list[i], arr.ind = T)
  temp_rank <- temp_rank[order(temp_rank[,1]),]
  v_var_rank[,i] <- temp_rank[,2]
}

