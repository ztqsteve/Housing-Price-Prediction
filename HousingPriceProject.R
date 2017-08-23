train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <-  read.csv("test.csv", stringsAsFactors = FALSE)
#--------------------------------------impute training set NA-------------------------------------
sum.na <- sort(sapply(train, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na

# replace NA with none according to data description file
train$PoolQC[is.na(train$PoolQC)] <- 'No'
train$MiscFeature[is.na(train$MiscFeature)] <- 'No'
train$Alley[is.na(train$Alley)] <- 'No'
train$Fence[is.na(train$Fence)] <- 'No'
train$FireplaceQu[is.na(train$FireplaceQu)] <- 'No'
train$GarageFinish[is.na(train$GarageFinish)] <- 'No'
train$GarageQual[is.na(train$GarageQual)] <- 'No'
train$GarageCond[is.na(train$GarageCond)] <- 'No'
train$GarageType[is.na(train$GarageType)] <- 'No'
#---------------------------------------------------------------------------
train$BsmtExposure[is.na(train$BsmtQual)] <- 'NoBsmt'
train$BsmtFinType2[is.na(train$BsmtQual)] <- 'NoBsmt'

train$BsmtQual[is.na(train$BsmtQual)] <- 'NoBsmt'
train$BsmtCond[is.na(train$BsmtCond)] <- 'NoBsmt'
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 'NoBsmt'

sum.na <- sort(sapply(train, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na

train <- train[ , !(names(train) %in% c("LotFrontage","GarageYrBlt"))]

library(mice)
train$Electrical <- as.factor(train$Electrical)
train$MasVnrType <- as.factor(train$MasVnrType)
train$BsmtExposure <- as.factor(train$BsmtExposure)
train$BsmtFinType2 <- as.factor(train$BsmtFinType2)
imp.train <- mice(train, m=1, method='cart', printFlag=FALSE)
train.complete <- complete(imp.train)
sum.na <- sort(sapply(train.complete, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
write.csv(train.complete, file = "TrainComplete.csv",row.names = FALSE)
#------------------------------------impute test set NA------------------------------------------------------------

sum.na <- sort(sapply(test, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na

test$PoolQC[is.na(test$PoolQC)] <- 'No'
test$MiscFeature[is.na(test$MiscFeature)] <- 'No'
test$Alley[is.na(test$Alley)] <- 'No'
test$Fence[is.na(test$Fence)] <- 'No'
test$FireplaceQu[is.na(test$FireplaceQu)] <- 'No'

test$GarageFinish[is.na(test$GarageType)] <- 'No'
test$GarageQual[is.na(test$GarageType)] <- 'No'
test$GarageCond[is.na(test$GarageType)] <- 'No'
test$GarageType[is.na(test$GarageType)] <- 'No'


test <- test[ , !(names(test) %in% c("LotFrontage","GarageYrBlt"))]


test$BsmtCond[is.na(test$BsmtFinType1)] <- 'NoBsmt'
test$BsmtExposure[is.na(test$BsmtFinType1)] <- 'NoBsmt'
test$BsmtQual[is.na(test$BsmtFinType1)] <- 'NoBsmt'
test$BsmtFullBath[is.na(test$BsmtFinType1)] <- 0
test$BsmtHalfBath[is.na(test$BsmtFinType1)] <- 0
test$BsmtFinSF1[is.na(test$BsmtFinType1)] <- 0
test$BsmtFinSF2[is.na(test$BsmtFinType1)] <- 0
test$BsmtUnfSF[is.na(test$BsmtFinType1)] <- 0
test$TotalBsmtSF[is.na(test$BsmtFinType1)] <- 0
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- 'NoBsmt'
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- 'NoBsmt'

test$Utilities[is.na(test$Utilities)] <- 'AllPub'

test$MasVnrType <- as.factor(test$MasVnrType)
test$MSZoning <- as.factor(test$MSZoning)
test$BsmtCond <- as.factor(test$BsmtCond)
test$BsmtQual <- as.factor(test$BsmtQual)
test$BsmtExposure <- as.factor(test$BsmtExposure)
test$GarageFinish <- as.factor(test$GarageFinish)
test$GarageQual <- as.factor(test$GarageQual)
test$GarageCond <- as.factor(test$GarageCond)
test$Exterior1st <- as.factor(test$Exterior1st)
test$Exterior2nd <- as.factor(test$Exterior2nd)
test$Functional <- as.factor(test$Functional)
test$KitchenQual <- as.factor(test$KitchenQual)
test$SaleType <- as.factor(test$SaleType)

imp.test <- mice(test, m=1, method='cart', printFlag=FALSE)
test.complete <- complete(imp.test)

sort(sapply(test.complete, function(x) { sum(is.na(x))}), decreasing=TRUE)
test.complete$SalePrice <- 0 #add a saleprice column to make it the same feature column with train 
write.csv(test.complete, file = "TestComplete.csv",row.names = FALSE)

whole<-rbind(train.complete,test.complete)
sort(sapply(whole, function(x) { sum(is.na(x))}), decreasing=TRUE)


#---------------------------------------feature engineering----------------------------------------------------

#Total size of a house
whole$Totalsize <- with(whole, TotalBsmtSF + X1stFlrSF + X2ndFlrSF + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch +
                          X3SsnPorch + ScreenPorch + PoolArea + LotArea + MasVnrArea)
whole$FrontSF <- with(whole, WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)
#TotalBath
whole$TotalBath <- with(whole, BsmtFullBath + 0.5 * BsmtHalfBath + FullBath + 0.5 * HalfBath)
whole$GrTotalBath <- with(whole,FullBath + 0.5 * HalfBath)
whole$BsmtTotalBath <- with(whole, BsmtFullBath + 0.5 * BsmtHalfBath )
whole$LivArea <- with(whole, X1stFlrSF + X2ndFlrSF)
#YearRemodAdd
whole$remod <- with(whole, ifelse(YearBuilt != YearRemodAdd, 1, 0))

#UsingYears
whole$years <- with(whole, YrSold - YearBuilt)
whole$new <- with(whole, ifelse(years <= 10, 1, 0))

whole$AllSF <- with(whole, GrLivArea + TotalBsmtSF)
whole$HighCorSF <- with(whole, GrLivArea + GarageArea)
whole$GrArea <- with(whole, GrLivArea + LotArea)
whole$AllSFPlus <- with(whole, AllSF + GarageArea)
whole$AllSFPlus2 <- with(whole,AllSF + LotArea)
whole$Parking <- with(whole, 0.8*GarageArea + 0.2*LotArea)
whole$SimMoSold <- with(whole, ifelse(MoSold >= 3 & MoSold <=5 , "spring",
                                           ifelse(MoSold >= 6 & MoSold <=8, "summer",
                                                  ifelse(MoSold >= 9 & MoSold <= 11,"fall","winter"))))

whole <- whole[ , !(names(whole) %in% c("MoSold"))]


catg_feature <- c("MSSubClass", "MSZoning", "Street", "Alley", "LotShape", "LandContour",
                  "Utilities", "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2",
                  "BldgType", "HouseStyle", "OverallQual", "OverallCond", "RoofStyle", "RoofMatl",
                  "Exterior1st", "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation",
                  "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "Heating",
                  "HeatingQC", "CentralAir", "Electrical", "KitchenQual", "Functional", "FireplaceQu",
                  "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PavedDrive", "PoolQC",
                  "Fence", "MiscFeature", "SimMoSold", "SaleType", "SaleCondition","remod","new")

whole[catg_feature] <- lapply(whole[catg_feature], as.factor)

# deal with new levels in test
train.data <- whole[1:dim(train)[1], ]
test.data <- whole[(dim(train)[1]+1):dim(whole)[1], ]

dim(train.data)
dim(test.data)


#---------------------------------linear regression------------------------------------------
mod.simp <- lm(log(SalePrice) ~.-Id-SalePrice, data = train.data)
#Residual Diagnosis
plot(mod.simp)
feature.matrix <- model.matrix( ~., subset(train.data, select = -c(Id,SalePrice)))
test.feature.matrix <- model.matrix( ~., subset(test.data, select = -c(Id,SalePrice)))
target.test <- read.csv('sample_submission.csv', stringsAsFactors = FALSE)
test.label <- log(target.test$SalePrice)
target.train <- log(train.data$SalePrice)
#elastic net
library(glmnet)
set.seed(42)
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

mse0 <- mean((test.label - pred0)^2)
mse1 <- mean((test.label - pred1)^2)
mse2 <- mean((test.label - pred2)^2)
mse3 <- mean((test.label - pred3)^2)
mse4 <- mean((test.label - pred4)^2)
mse5 <- mean((test.label - pred5)^2)
mse6 <- mean((test.label - pred6)^2)
mse7 <- mean((test.label - pred7)^2)
mse8 <- mean((test.label - pred8)^2)
mse9 <- mean((test.label - pred9)^2)
mse10 <- mean((test.label - pred10)^2)

all_mse<-c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10)
plot(seq( 0,  1, 0.1),all_mse,type='l',xlab="alpha",ylab="MSE")
which.min(all_mse)-1
train.pred <- predict(fit7, s=fit7$lambda.1se, newx = feature.matrix)
sqrt(mean((target.train-train.pred)^2))#RMSE=0.18
sqrt(min(all_mse))#RMSE=0.285

coef.name <- which(coef(fit4, s = "lambda.1se") != 0)
coef(fit4, s = "lambda.1se")[coef.name,]
#--------------------------------decision tree-------------------------------------
library(rpart)
formula <- "log(SalePrice) ~.-SalePrice-Id "
set.seed(42)
tree1 <- rpart(formula, method = 'anova', data = train.data, 
               control=rpart.control(cp=0))

cp.tab <- as.data.frame(tree1$cptable)
bestcp <- cp.tab$CP[with(cp.tab, min(which(xerror - xstd <= min(xerror))))]
tree.pruned <- prune(tree1, cp = bestcp)

train.pred <- predict(tree.pruned, train.data)
test.pred <- predict(tree.pruned, test.data)
sqrt(mean((train.pred - target.train)^2))#RMSE=0.155
sqrt(mean((test.pred - test.label)^2))#RMSE=0.37

#-----------------------------random forest-----------------------------------------

library(randomForest)
set.seed(42)
rf.formula <- paste("log(SalePrice) ~ .-SalePrice-Id ")
rf <- randomForest(as.formula(rf.formula), data = train.data, importance = TRUE)
train.pred <- predict(rf, train.data)
test.pred <- predict(rf, test.data) 
sqrt(mean((train.pred - target.train)^2))#RMSE=0.05,overfitting
sqrt(mean((test.pred - test.label)^2))#RMSE=0.35
varImpPlot(rf)
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
write.csv(result,file= "result3.csv",row.names= FALSE)










