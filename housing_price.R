train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <-  read.csv("test.csv", stringsAsFactors = FALSE)
# see correlation between features
library(corrplot)
contVar <- names(train)[which(sapply(train, is.numeric))]
trainCont <- train[, contVar]
correlations <- cor(trainCont, use = "pairwise.complete.obs")
corrplot(correlations, method = "square")
# look up top 10 feature correlated to SalePrice
cor <- as.data.frame(as.table(correlations))
cor <- subset(cor, cor$Var2 == "SalePrice")
cor <- cor[order(cor$Freq, decreasing = T)[1:10],]
cor
# dealing with missing data
sum.na <- sort(sapply(train, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
sum.na.percent <- sort(sapply(train, function(x) { sum(is.na(x)/dim(train)[1])}), decreasing=TRUE)
sum.na.percent
keep.col <- names(which(sum.na < 2))
train <- train[, keep.col]
train <- train[-which(is.na(train$Electrical)),]
sum.na <- sort(sapply(train, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
# dealing with outliers
train_scaled <- train
train_scaled$SalePrice <- scale(train_scaled$SalePrice)
sort(train_scaled$SalePrice,decreasing = T)
plot(train$GrLivArea,train$SalePrice)
order(train$GrLivArea,decreasing = T)[1:2]
train <- train[-1299,]
train <- train[-524,]
# data transformation for normality and homoscedasticity
plot(train$TotalBsmtSF,train$SalePrice)
train$GrLivArea <- log(train$GrLivArea)
plot(train$GrLivArea,log(train$SalePrice))
train$TotalBsmtSF <- log(train$TotalBsmtSF)
train$TotalBsmtSF[which(is.infinite(train$TotalBsmtSF))] <- 0
plot(train$TotalBsmtSF,log(train$SalePrice))
train$HaveBsmt <- 0
train$HaveBsmt[which(train$TotalBsmtSF>0)] <- 1
# ===================for test set=======================
# dealing with missing data
sum.na <- sort(sapply(test, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
keep.col[1:length(keep.col)-1]
test <- test[, keep.col[1:length(keep.col)-1]]
sum.na <- sort(sapply(test, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
test[which(is.na(test$TotalBsmtSF)),]
cols = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')
test[which(is.na(test$TotalBsmtSF)),cols] <- 0
sum.na <- sort(sapply(test, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
cols <- c('GarageCars','GarageArea')
test[which(is.na(test$GarageCars)),cols] <- 0
sum.na <- sort(sapply(test, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
cols <- c('BsmtFullBath','BsmtHalfBath')
test[which(is.na(test$BsmtFullBath)),cols] <- 0
sum.na <- sort(sapply(test, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
library(mice)
test$Exterior1st <- as.factor(test$Exterior1st)
test$Exterior2nd <- as.factor(test$Exterior2nd)
test$MSZoning <- as.factor(test$MSZoning)
test$Functional <- as.factor(test$Functional)
test$KitchenQual <- as.factor(test$KitchenQual)
test$SaleType <- as.factor(test$SaleType)
test$Utilities <- as.factor(test$Utilities)

imp.test <- mice(test, m=1, method='cart', printFlag=FALSE)
test <- complete(imp.test)
sum.na <- sort(sapply(test, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
test$Utilities[is.na(test$Utilities)] <- 'AllPub'
sum.na <- sort(sapply(test, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
test$SalePrice <- 0 #add a saleprice column to make it the same feature column with train 
test$HaveBsmt <- 0
test$HaveBsmt[which(test$TotalBsmtSF>0)] <- 1
test$GrLivArea <- log(test$GrLivArea)
test$TotalBsmtSF <- log(test$TotalBsmtSF)
test$TotalBsmtSF[which(is.infinite(test$TotalBsmtSF))] <- 0
write.csv(test, file = "TestComplete.csv",row.names = FALSE)
write.csv(train, file = "TrainComplete.csv",row.names = FALSE)

whole<-rbind(train,test)
sort(sapply(whole, function(x) { sum(is.na(x))}), decreasing=TRUE)

# ===================build model=======================
catg_feature <- c("MSSubClass", "MSZoning", "Street", "LotShape", "LandContour",
                  "Utilities", "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2",
                  "BldgType", "HouseStyle", "OverallQual", "OverallCond", "RoofStyle", "RoofMatl",
                  "Exterior1st", "Exterior2nd", "ExterQual", "ExterCond", "Foundation",
                  "Heating",
                  "HeatingQC", "CentralAir", "Electrical", "KitchenQual", "Functional", 
                  "PavedDrive",
                  "MoSold", "SaleType", "SaleCondition","HaveBsmt")

whole[catg_feature] <- lapply(whole[catg_feature], as.factor)
train.data <- whole[1:dim(train)[1], ]
test.data <- whole[(dim(train)[1]+1):dim(whole)[1], ]
# linear regression
mod.simp <- lm(log(SalePrice) ~.-Id-SalePrice, data = train.data)
sort(abs(mod.simp$coefficients),decreasing = T)
#Residual Diagnosis
plot(mod.simp)
feature.matrix <- model.matrix( ~., subset(train.data, select = -c(Id,SalePrice)))
test.feature.matrix <- model.matrix( ~., subset(test.data, select = -c(Id,SalePrice)))
target.test <- read.csv('sample_submission.csv', stringsAsFactors = FALSE)
test.label <- log(target.test$SalePrice)
target.train <- log(train.data$SalePrice)

#elastic net
library(glmnet)
set.seed(113)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(feature.matrix, target.train,  alpha=i/10))}

pred0 <- predict(fit0, s=fit0$lambda.1se, newx=test.feature.matrix)
pred1 <- predict(fit1, s=fit1$lambda.1se, newx=test.feature.matrix)
pred2 <- predict(fit2, s=fit2$lambda.1se, newx=test.feature.matrix)
pred3 <- predict(fit3, s=fit3$lambda.1se, newx=test.feature.matrix)
pred4 <- predict(fit4, s=fit4$lambda.1se, newx=test.feature.matrix)
pred5 <- predict(fit5, s=fit5$lambda.1se, newx=test.feature.matrix)
pred6 <- predict(fit6, s=fit6$lambda.1se, newx=test.feature.matrix)
pred7 <- predict(fit7, s=fit7$lambda.1se, newx=test.feature.matrix)
pred8 <- predict(fit8, s=fit8$lambda.1se, newx=test.feature.matrix)
pred9 <- predict(fit9, s=fit9$lambda.1se, newx=test.feature.matrix)
pred10 <- predict(fit10, s=fit10$lambda.1se, newx=test.feature.matrix)
# after submission, pred8 get the best score.

#--------------------------------decision tree-------------------------------------
library(rpart)
formula <- "log(SalePrice) ~.-SalePrice-Id "
set.seed(123)
tree1 <- rpart(formula, method = 'anova', data = train.data, 
               control=rpart.control(cp=0))

cp.tab <- as.data.frame(tree1$cptable)
bestcp <- cp.tab$CP[with(cp.tab, min(which(xerror - xstd <= min(xerror))))]
tree.pruned <- prune(tree1, cp = bestcp)

train.pred <- predict(tree.pruned, train.data)
test.pred <- predict(tree.pruned, test.data)
sqrt(mean((train.pred - target.train)^2))#RMSE=0.152
sqrt(mean((test.pred - test.label)^2))#RMSE=0.37

result <- data.frame(Id=test.data$Id)
result$SalePrice <- exp(test.pred)
write.csv(result,file= "result_dt.csv",row.names= FALSE)
#-----------------------------random forest-----------------------------------------

library(randomForest)
set.seed(111)
rf.formula <- paste("log(SalePrice) ~ .-SalePrice-Id ")
rf <- randomForest(as.formula(rf.formula), data = train.data, importance = TRUE)
train.pred <- predict(rf, train.data)
test.pred <- predict(rf, test.data) 
sqrt(mean((train.pred - target.train)^2))#RMSE=0.05,overfitting
sqrt(mean((test.pred - test.label)^2))#RMSE=0.34
varImpPlot(rf)

result <- data.frame(Id=test.data$Id)
result$SalePrice <- exp(test.pred)
write.csv(result,file= "result_rf.csv",row.names= FALSE)

#-----------------------------boosting tree-------------------------------------------

library(xgboost)
# grid searching for parameters.
best_param = list()
best_seednumber = 42
best_test_rmse = Inf
best_test_rmse_index = 0

for (iter in 1:100) {
  param <- list(objective = "reg:linear",
                max_depth = sample(5:12, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 500
  cv.nfold = 10
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=feature.matrix, label = target.train, params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = F, early_stop_round=8, 
                 maximize=FALSE)
  min_test_rmse = min(mdcv$evaluation_log$test_rmse_mean)
  min_test_rmse_index = which.min(mdcv$evaluation_log$test_rmse_mean)
  
  if (min_test_rmse < best_test_rmse) {
    best_test_rmse = min_test_rmse
    best_test_rmse_index = min_test_rmse_index
    best_seednumber = seed.number
    best_param = param
  }
  
}

nround = best_test_rmse_index
set.seed(best_seednumber)

gbt <- xgboost(data = feature.matrix, 
               label = target.train,
               nround = nround,
               params = best_param,
               nthread=6)

test.pred <- predict(gbt, test.feature.matrix) 

result <- data.frame(Id=test.data$Id)
result$SalePrice <- exp(test.pred)
write.csv(result,file= "result_xgb.csv",row.names= FALSE)









