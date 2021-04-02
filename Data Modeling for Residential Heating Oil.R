install.packages("readxl")
library(readxl)
S1<-read_excel("Group26_RData(RESE)_FinalData.xlsx")[2:229, 1:32]
str(S1)
S1[,3:32] <- lapply(S1[,3:32], as.numeric) 
S1[, 1:2] <- lapply(S1[,1:2], as.factor)
str(S1)
table(is.na(S1))
install.packages("Hmisc")
library(Hmisc)
install.packages("MASS")
install.packages("ISLR")
install.packages("glmnet")
install.packages("leaps")
install.packages("ElemStatLearn")
library(ElemStatLearn)
library(glmnet)
library(ISLR)
library(MASS)
library(leaps)
install.packages("nlme")
library(mgcv)
install.packages("earth")
library(earth)
install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)
install.packages("ada")
library(ada)
install.packages("gbm")
library(gbm)

#Checking for normality of data
#Response
hist(S1$RCPC,prob=T,xlab='',
     main='Histogram of CONSUMPTION(RESE)',col='pink')
lines(density(S1$RCPC,na.rm=T))
rug(S1$RCPC)

#Boxplots for Response Vs Year and Month
boxplot(S1$RCPC~I(S1$YEAR), main = "Annual Consumption(RESE) Distribution",col="yellow", xlab = "Years",ylab = "Consumption(RESE)", varwidth = TRUE)
rug(jitter(S1$RCPC),side=2)
abline(h=mean(S1$RCPC,na.rm=T),lty=2) 

plot( tapply(S1$RCPC, S1$MONTH, mean), main = "Monthwise Yearly Average Consumption(RESE)", xlab = "Month" , ylab = "Consumption(RESE)", type = "h", col = "Red")

#SCATTERPLOT+LINES THROUGH THE POINTS
plot(S1$RCPC,xlab='')
abline(h=mean(S1$RCPC,na.rm=T),lty=1)
abline(h=mean(S1$RCPC,na_rm=T)+sd(S1$RCPC,na.rm=T))

plot(S1$PRICE, S1$RCPC, main="Scatter plot",
     xlab="PRICE", ylab=" Rsidential Consumption ", pch=19)

# Response Vs Temperature Variables
plot(~S1$RCPC+S1$DT00+S1$DX90+S1$EMNT+S1$EMXT+S1$MNTM+S1$MMXT+S1$MMNT+S1$MDPT)
rcorr(cbind(S1$RCPC,S1$DT00,S1$DX90,S1$EMNT,S1$EMXT,S1$MNTM,S1$MMXT,S1$MMNT,S1$MDPT), type = "pearson")
#Remove MNTM

#Response Vs Precipitation Variables
plot(~S1$RCPC+S1$DP01+S1$DP10+S1$EMXP+S1$MNPM)
rcorr(cbind(S1$RCPC,S1$DP01,S1$DP10,S1$EMXP,S1$MNPM), type = "pearson")
# Keep all 

#Response Vs Snow Variables
plot(~S1$RCPC+S1$DSND+S1$DSNF+S1$EMSD+S1$EMSF+S1$TMSF+S1$MSND)
rcorr(cbind(S1$RCPC,S1$DSND,S1$DSNF,S1$EMSD,S1$EMSF,S1$TMSF,S1$MSND), type = "pearson")
#Keep all

#Response Vs Cooling and heating days
plot(~S1$RCPC+S1$CMX65+S1$CMN65+S1$HMN65+S1$HMX65)
rcorr(cbind(S1$RCPC,S1$CMX65,S1$CMN65,S1$HMN65,S1$HMX65), type = "pearson")
#Keep all

#Response Vs Wind variables
plot(~S1$RCPC+S1$VISIB+S1$GUST+S1$WDSP+S1$MXSPD)
rcorr(cbind(S1$RCPC,S1$VISIB,S1$GUST,S1$WDSP,S1$MXSPD), type = "pearson")
#Keep all

#Response Vs Socioeconomic Variables
plot(~S1$RCPC+S1$UNEMPRATE+S1$GDP)
rcorr(cbind(S1$RCPC,S1$UNEMPRATE,S1$GDP), type = "pearson")
#Keep all

Final <- S1[,c(-22)]
Data <- Final[, c(-1, -2)]
View(Data)

#Splitting data into training and tesing 
set.seed(123) 
train_idx <- sample(x = 1:nrow(Data), size = floor(0.80*nrow(Data)))
train_data <- Data[train_idx,]
test_data <- Data[-train_idx,]

#Null model MSE
null.msetrain<-mean((mean(train_data$RCPC)-train_data$RCPC)^2)
null.rmsetrain<-sqrt(null.msetrain)

null.msetest<-mean((mean(test_data$RCPC)-test_data$RCPC)^2)
null.rmsetest<-sqrt(null.msetest)


#Multiple Linear Regression)
lm.fit <- lm(RCPC ~ ., data=train_data)
summary(lm.fit)
fit2=lm(RCPC ~ PRICE+WDSP+DSND+MXSPD+GDP, data = train_data)
summary(fit2)
fit3=lm(RCPC ~ PRICE+WDSP+DSND+MXSPD+GDP, data = test_data)
summary(fit3)

par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(2,2))
plot(fit3)

prediction = predict(fit3,newdata=test_data)
prediction

mean(fit2$residuals^2)
sqrt(mean(fit2$residuals^2))
linear.mse= mean((fit3$residuals)^2)
linear.rmse= sqrt(linear.mse)
print(linear.rmse)

#Cross validation for Multiple Linear Regression
install.packages('caret')
library('caret')
set.seed(123)
train.control<-trainControl(method="cv", number=10)
cv_lm<-train(RCPC~., data=train_data, method="lm",
             trControl=train.control)
print(cv_lm)

train.control<-trainControl(method="cv", number=10)
cv_lm<-train(RCPC~., data=test_data, method="lm",
             trControl=train.control)
print(cv_lm)   

#Ridge Regression
set.seed(123)
train.X <- as.matrix(train_data[,-1])
train.Y <- as.matrix(train_data[,1])
test.X <- as.matrix(test_data[,-1])
test.Y <- as.matrix(test_data[,1])
library(glmnet)
set.seed(123)
cv <- cv.glmnet(x=train.X, y=train.Y, family = 'gaussian', alpha = 0)
cv$lambda.min

ridge.fit <- glmnet(x=train.X, y=train.Y, family = 'gaussian', alpha = 0, lambda = cv$lambda.min)
#ridge.fit <- glmnet(x=train.X, y=train.Y, family = 'gaussian', alpha = 0)
ridge.p<- predict(ridge.fit,newx=train.X)
ridge.msetrain<-mean((ridge.p-train.Y)^2)
ridge.rmsetrain<-sqrt(ridge.msetrain)

ridge.pred <- predict(ridge.fit, newx=test.X)
ridge.mse <- mean((ridge.pred - test.Y)^2)
ridge.rmse<-sqrt(ridge.mse)

plot(test.Y, ridge.pred, pch="o", col='black',lty=5,  main="LM: Ridge Actual vs Predicted",
     xlab = "Actual Sales", 
     ylab="Predicted Sales")

plot(train.Y, ridge.p, pch="o", col='black',lty=5,  main="LM: Ridge Actual vs Predicted",
     xlab = "Actual Sales", 
     ylab="Predicted Sales")
coef(ridge.fit)

#Lasso Regression
set.seed(123)
cv2 <- cv.glmnet(x=train.X, y=train.Y, family='gaussian', alpha = 1)
cv2$lambda.min

lasso.fit <- glmnet(x=train.X, y=train.Y, family='gaussian', alpha = 1, lambda = cv2$lambda.min)
#lasso.fit <- glmnet(x=train.X, y=train.Y, family = 'gaussian', alpha = 1)
lasso.p<-predict(lasso.fit,newx =train.X)
lasso.msetrain<-mean((lasso.p - train.Y)^2)
lasso.rmsetrain<-sqrt(lasso.msetrain)
plot(train.Y, lasso.p, pch="o", col='black',lty=5,  main="LM: Lasso Actual vs Predicted",
     xlab = "Actual Sales", 
     ylab="Predicted Sales")

lasso.pred <- predict(lasso.fit, newx=test.X)
lasso.mse <- mean((lasso.pred - test.Y)^2)
lasso.rmse<-sqrt(lasso.mse)
plot(test.Y, lasso.pred, pch="o", col='black',lty=5,  main="LM: Lasso Actual vs Predicted",
     xlab = "Actual Sales", 
     ylab="Predicted Sales")
coef(lasso.fit)

#Best Subset
library(MASS)
library(ISLR)
library(glmnet)
library(ElemStatLearn)
library(leaps)
set.seed(123)
regfit.full= regsubsets(RCPC~., data = train_data, method = "exhaustive", nvmax = 21)
plot(summary(regfit.full)$cp)
num_var <- which.min(summary(regfit.full)$cp)
print(num_var)
summary(regfit.full)

feat.cp <- which(summary(regfit.full)$which[num_var,] == TRUE)
subset.mod <- lm(RCPC ~ ., data=train_data[,feat.cp])
subset.predtrain <- predict(subset.mod, newdata=train_data[,feat.cp])
subset.msetrain <- mean((subset.predtrain - train.Y)^2)
subset.rmsetrain<-sqrt(subset.msetrain)

subset.pred <- predict(subset.mod, newdata=test_data[,feat.cp])
subset.mse <- mean((subset.pred - test.Y)^2)
subset.rmse<-sqrt(subset.mse)

#Forward subset
set.seed(123)
regfit.full=regsubsets(RCPC~., data = train_data, method = "forward", nvmax = 21)
plot(summary(regfit.full)$cp)
num_var <- which.min(summary(regfit.full)$cp)
print(num_var)
summary(regfit.full)

feat.cp <- which(summary(regfit.full)$which[num_var,] == TRUE)
forward.mod <- lm( RCPC~ ., data=train_data[,feat.cp])
forward.predtrain <- predict(forward.mod, newdata=train_data[,feat.cp])
forward.msetrain <- mean((forward.predtrain - train.Y)^2)
forward.rmsetrain<-sqrt(forward.msetrain)

forward.pred <- predict(forward.mod, newdata=test_data[,feat.cp])
forward.mse <- mean((forward.pred - test.Y)^2)
forward.rmse<-sqrt(forward.mse)

#Backward subset
set.seed(123)
regfit.full=regsubsets(RCPC~., data = train_data, method = "backward")
plot(summary(regfit.full)$cp)
num_var <- which.min(summary(regfit.full)$cp)
print(num_var)
summary(regfit.full)

feat.cp <- which(summary(regfit.full)$which[num_var,] == TRUE)
backward.mod <- lm(RCPC ~ ., data=train_data[,feat.cp])
backward.predtrain <- predict(backward.mod, newdata=train_data[,feat.cp])
backward.msetrain <- mean((backward.predtrain - train.Y)^2)
backward.rmsetrain<-sqrt(backward.msetrain)

backward.pred <- predict(backward.mod, newdata=test_data[,feat.cp])
backward.mse <- mean((backward.pred - test.Y)^2)
backward.rmse<-sqrt(backward.mse)

#Generalized Additive Model
install.packages("nlme")
library(mgcv)
attach(train_data)
set.seed(123)
gam1 <- gam(RCPC ~ s(PRICE, bs="cr", k=3) + s(CMX65, bs="cr",k=3)
            + s(CMN65, bs="cr",k=3) + s(DP01, bs="cr",k=3)
            + s(DP10, bs="cr",k=3) + s(DSND, bs="cr",k=3) + s(DT00, bs="cr",k=3)
            + s(DX90, bs="cr",k=3)
            + s(EMNT, bs="cr",k=3) + s(EMSD, bs="cr",k=3) + s(HMN65 , bs="cr",k=3)
            + s(VISIB, bs="cr",k=3) + s(HMX65, bs="cr",k=3) + s(MXSPD, bs="cr",k=3)
            + s(MMNT, bs="cr",k=3) + s(GUST, bs="cr",k=3) + s(WDSP, bs="cr",k=3)
            + s(UNEMPRATE, bs="cr",k=3) + s(GDP, bs="cr",k=3) + s(DSNF,bs="cr",k=3) +s(EMSF,bs="cr",k=3)
            + s(EMXP, bs="cr",k=3) + s(EMXT,bs="cr",k=3) +s(MNPM,bs="cr",k=3) +s(TMSF,bs="cr",k=3) + s(MMXT,bs="cr",k=3)
            + s(MDPT, bs="cr",k=3) + s(MSND,bs="cr",k=3), data=train_data)
par(mfrow=c(1,2))
gam.check(gam1)
summary(gam1)
par(mfrow=c(2,4))
plot(gam1, se=TRUE)

gam2 <- update(gam1, .~. -s(PRICE, bs="cr", k=3) 
               - s(DP01, bs="cr",k=3)
               - s(DP10, bs="cr",k=3) - s(DSND, bs="cr",k=3) - s(DT00, bs="cr",k=3)
               - s(DX90, bs="cr",k=3)
               - s(EMNT, bs="cr",k=3) - s(EMSD, bs="cr",k=3)  - s(HMX65, bs="cr",k=3) - s(MXSPD, bs="cr",k=3)
               - s(GUST, bs="cr",k=3) - s(WDSP, bs="cr",k=3)
               - s(UNEMPRATE, bs="cr",k=3)  -s(EMSF,bs="cr",k=3)
               - s(EMXT,bs="cr",k=3) -s(MNPM,bs="cr",k=3) -s(TMSF,bs="cr",k=3) - s(MMXT,bs="cr",k=3)
               - s(MDPT, bs="cr",k=3)  + PRICE+DP01+DP10+DSND+DT00+DX90+EMNT+EMSD+HMX65+MXSPD+GUST+WDSP
               +UNEMPRATE+EMSF+EMXT+MNPM+TMSF+MMXT+MDPT, data=train_data)
summary(gam2)
par(mfrow=c(1,1))
plot(gam2)

layout(matrix(c(1:1),1,1,byrow=TRUE))
residuals.gam <- c()
gam.predicttrain <- predict(gam2, newdata = train_data, type="response")
residuals.gam <- (train_data$RCPC-gam.predicttrain)
library(car)
qqPlot(residuals.gam,main = "GAM:Residual Plot") 
plot(train_data$RCPC, gam.predicttrain, pch="o", col='black',lty=5,  main="GAM(TRAIN): Actual vs Fitted",
     xlab = "Actual Consumption", 
     ylab="Predicted Consumption")
gam.msetrain <-mean((gam.predicttrain - train_data$RCPC)^2)
gam.rmsetrain<-sqrt(gam.msetrain)


layout(matrix(c(1:1),1,1,byrow=TRUE))
residuals.gam3 <- c()
gam.predict <- predict(gam2 , newdata = test_data, type="response")
residuals.gam3<- (test_data$RCPC-gam.predict)
library(car)
qqPlot(residuals.gam3,main = "GAM:Residual Plot") 
plot(test_data$RCPC, gam.predict, pch="o", col='black',lty=5,  main="GAM(TEST): Actual vs Fitted",
     xlab = "Actual Consumption", 
     ylab="Predicted Consumption")
gam.mse <- mean((gam.predict - test_data$RCPC)^2)
gam.rmse<-sqrt(gam.mse)

#MARS
library(earth)
set.seed(123)
mars1 <- earth(RCPC~., data=train_data, pmethod="cv", nfold=10, ncross=10)
print(mars1)
summary(mars1)
plot(mars1,which=1)
install.packages("dplyr")
library(dplyr)
hyper_grid <- expand.grid(
        degree = 1:3, 
        nprune = seq(2, 100, length.out = 10) %>% floor()
)

head(hyper_grid)
install.packages("caret")
library(caret)

cv_mars1 <- train(
        x = subset(train_data, select = -RCPC),
        y = train_data$RCPC,
        method = "earth",
        metric = "RMSE",
        trControl = trainControl(method = "cv", number = 10),
        tuneGrid = hyper_grid
)

# View results
cv_mars1$bestTune
ggplot(cv_mars1)

mars.predicttrain <- predict(mars1,train_data,type="response")
plot(train_data$RCPC, mars.predicttrain, pch="o", col='black',lty=5,  main="MARS(RESE): Actual vs Predicted(Train Data)",
     xlab = "Actual", 
     ylab="Predicted")
mars.msetrain <-mean((mars.predicttrain - train_data$RCPC)^2)
mars.rmsetrain <- sqrt(mars.msetrain)
residuals.mars <- (train_data$RCPC-mars.predicttrain)
install.packages("car")
library(car)
par(mfrow=c(1,1))
qqPlot(residuals.mars, main = "MARS(RESE): Residual Plot(Train Data)") 

mars.predict <- predict(mars1,test_data,type="response")
plot(test_data$RCPC, mars.predict, pch="o", col='black',lty=5,  main="MARS(RESE): Actual vs Fitted(Test Data)",
     xlab = "Actual", 
     ylab="Predicted")
mars.mse <-mean((mars.predict - test_data$RCPC)^2)
mars.rmse <- sqrt(mars.mse)
residuals.mars1 <- (test_data$RCPC-mars.predict)
install.packages("car")
library(car)
par(mfrow=c(1,1))
qqPlot(residuals.mars1, main = "MARS(RESE): Residual Plot(Test Data)") 

mars.imp1 = evimp(mars1, trim = FALSE)
print(mars.imp1)
plot(mars.imp1)
plot(mars1)

#Tree Model
library(tree)
set.seed(123)
tree=tree(RCPC~., data=train_data)
summary(tree)
plot(tree)
text(tree,pretty=0)

#Determining optimal size of tree
cv.tree=cv.tree(tree)
plot(cv.tree$size,cv.tree$dev,type='b') 
#Prune the tree on the best 6 terminal nodes)
prune.tree=prune.tree(tree,best=6) 
plot(prune.tree)
text(prune.tree,pretty=0)

#Random Forest and Bagging 
library(randomForest)
set.seed(123)
tree.random <- tree(RCPC~., train_data)

# Helper function for calculating RMSE
rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
        #Calculates rmse for a regression decision tree
        yhat <- predict(model_obj, newdata = testing)
        actual <- testing[[target]]
        sqrt(mean((yhat-actual)^2))
}
rmse_reg(tree.random, test_data, "RCPC")

#Bagging
#Check how many trees to fit
set.seed(123)
bag <- randomForest(RCPC ~ ., data=train_data, mtry = ncol(train_data) - 1, importance = TRUE, ntree=400)
plot(bag, type='l', main='MSE by ntree for Bagging')

#Bagging by using optimal number of trees
bag1 <- randomForest(RCPC~ ., data=train_data, mtry = ncol(train_data) - 1, importance = TRUE, ntree=60)
rmse_reg(bag1, train_data, "RCPC")
rmse_reg(bag1, test_data, "RCPC")

bag.predict <- predict(bag1,test_data,type="response")
plot(test_data$RCPC, bag.predict, pch="o", col='red',lty=5,  main="Bagging: Actual vs Fitted(Test Data)",
     xlab = "Actual", 
     ylab="Predicted")
bag.mse <-mean((bag.predict - test_data$RCPC)^2)
bag.rmse <- sqrt(bag.mse)
residuals.bag <- (test_data$RCPC-bag.predict)
install.packages("car")
library(car)
par(mfrow=c(1,1))
qqPlot(residuals.bag, main = "Bagging: Residual Plot(Test Data)") 

bag.predicttrain <- predict(bag1,train_data,type="response")
plot(train_data$RCPC, bag.predicttrain, pch="o", col='red',lty=5,  main="Bagging: Actual vs Fitted(train Data)",
     xlab = "Actual", 
     ylab="Predicted")
bag.msetrain <-mean((bag.predicttrain - train_data$RCPC)^2)
bag.rmsetrain <- sqrt(bag.msetrain)
residuals.bagtrain <- (train_data$RCPC-bag.predicttrain)
install.packages("car")
library(car)
par(mfrow=c(1,1))
qqPlot(residuals.bagtrain, main = "Bagging: Residual Plot(Train Data)") 

# Random Forest using optimal number of trees
set.seed(123)
rf.mse <- c()
for(i in 1:(ncol(train_data)-1)){
        rf.RESE <- randomForest(RCPC~., data=train_data, mtry=i, importance=TRUE, ntree=60)
        rf.mse[i] <- rf.RESE$mse[60]
}
plot(rf.mse, main='Training Error by m', xlab='Number of Predictors', ylab='MSE')
#Select final model, 18 predictors per tree.
rf.RESE <- randomForest(RCPC~., data=train_data, mtry=18, importance=TRUE, ntree=60)
#Out of bag error
rmse_reg(rf.RESE, test_data, "RCPC")
rmse_reg(rf.RESE, train_data, "RCPC")

#Variable importance for bagging
importance(bag1)
varImpPlot(bag1)
# Variable importance for Random Forest
importance(rf.RESE)
varImpPlot(rf.RESE)
summary(rf.RESE)
par(mfrow=c(2,2))
plot(rf.RESE)

random.predict <- predict(rf.RESE,test_data,type="response")
plot(test_data$RCPC, random.predict, pch="o", col='red',lty=5,  main="Random Forest: Actual vs Fitted(Test Data)",
     xlab = "Actual", 
     ylab="Predicted")
random.mse <-mean((random.predict - test_data$RCPC)^2)
random.rmse <- sqrt(random.mse)
residuals.random <- (test_data$RCPC-random.predict)
install.packages("car")
library(car)
par(mfrow=c(1,1))
qqPlot(residuals.random, main = "Random Forest: Residual Plot(Test Data)") 

random.predicttrain <- predict(rf.RESE,train_data,type="response")
plot(train_data$RCPC, random.predicttrain, pch="o", col='red',lty=5,  main="Random Forest: Actual vs Fitted(train Data)",
     xlab = "Actual", 
     ylab="Predicted")
random.msetrain <-mean((random.predicttrain - train_data$RCPC)^2)
random.rmsetrain <- sqrt(random.msetrain)
residuals.randomtrain <- (train_data$RCPC-random.predicttrain)
install.packages("car")
library(car)
par(mfrow=c(1,1))
qqPlot(residuals.randomtrain, main = "Random Forest: Residual Plot(Train Data)") 

# Construct partial dependence plots for final model(BAGGING)
install.packages("pdp")
library(pdp)
p1 <- partial(bag1, pred.var = "HMX65", grid.resolution = 10) %>% 
        autoplot()
p2 <- partial(bag1, pred.var = "MMXT", grid.resolution = 10) %>% 
        autoplot()
p3 <- partial(bag1, pred.var = "PRICE",grid.resolution = 10) %>%
        autoplot()
p4 <- partial(bag1, pred.var ="GDP",grid.resolution = 10) %>%
        autoplot()
p5 <- partial(bag1, pred.var = "VISIB", grid.resolution = 10) %>% 
        autoplot()
p6 <- partial(bag1, pred.var = "CMN65", grid.resolution = 10) %>% 
        autoplot()
p7 <- partial(bag1, pred.var = "DSND",grid.resolution = 10) %>%
        autoplot()
p8 <- partial(bag1, pred.var ="EMNT",grid.resolution = 10) %>%
        autoplot()
p9 <- partial(bag1, pred.var = "EMXT", grid.resolution = 10) %>% 
        autoplot()
p10 <- partial(bag1, pred.var = "MMNT",grid.resolution = 10) %>%
        autoplot()
p11 <- partial(bag1, pred.var ="UNEMPRATE",grid.resolution = 10) %>%
        autoplot()
# Display plots side by side
gridExtra::grid.arrange(p1, p2, ncol = 2)
gridExtra::grid.arrange(p3, p4, ncol = 2)
gridExtra::grid.arrange(p5, p6, ncol = 2)
gridExtra::grid.arrange(p7, p8, ncol = 2)
gridExtra::grid.arrange(p9, p10, ncol = 2)
gridExtra::grid.arrange(p11, ncol = 2)

save(list = ls(all=T),file =  "./Group26_RData(RESE)_FinalData.RData")
