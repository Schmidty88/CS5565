## Problem 1 
## 1.a
library(ISLR)
library(MASS)
attach(Default)
set.seed(1)
lr<-glm(default~income+balance,family = binomial,data=Default)
summary(lr)
## This is were I will predict at
p<-predict(lr,Default$default,type="response")
p.class<-ifelse(p>0.5,"Yes","No")
round(mean(Default$default!=p.class),4)
## we obtain an answer of 0.0263

## 1.b
sub<-sample(nrow(Default),nrow(Default)*0.9)
def.train<-Default[sub,]
def.test<-Default[-sub,]
## Prediction
lr.90<-glm(def~income+balance,family = binomial,data=def.train)
predict.90<-predict(lr.90,def.test,type="response")
class.90<-ifelse(predict.90>0.5,"Yes","No")
# Validation set error
table(def.test$def,class.90,dnn=c("Actual","Predicted"))

##1.C
set.seed(1)
subset<-sample(nrow(Default),nrow(Default)*0.8)
default.train<-Default[sub,]
default.test<-Default[-sub,]
lr.80<-glm(default~income+balance,family = binomial,data=default.train)
predict.80<-predict(lr.80,default.test,type="response")
class.80<-ifelse(predict.80>0.5,"Yes","No")
mr80<-round(mean(class.80!=default.test$default),4)
sub<-sample(nrow(Default),nrow(Default)*0.7)
default.train<-Default[sub,]
default.test<-Default[-sub,]
lr.70<-glm(default~income+balance,family = binomial,data=default.train)
predict.70<-predict(lr.70,default.test,type="response")
class.70<-ifelse(predict.70>0.5,"Yes","No")
mr70<-round(mean(class.70!=default.test$default),4)
sub<-sample(nrow(Default),nrow(Default)*0.5)
default.train<-Default[sub,]
default.test<-Default[-sub,]
#iii
lr.50<-glm(default~income+balance,family = binomial,data=default.train)
predict.50<-predict(lr.50,default.test,type="response")
class.50<-ifelse(predict.50>0.5,"Yes","No")
#iv
mr50<-round(mean(class.50!=default.test$default),4)

##1.D
set.seed(1)
subset<-sample(nrow(Default),nrow(Default)*0.7)
default.train<-Default[subset,]
default.test<-Default[-subset,]
lr.70<-glm(default~income+balance+student,family = binomial,data=default.train)
summary(lr.70)
predict.70<-predict(lr.70,default.test,type="response")
class.70<-ifelse(predict.70>0.7,"Yes","No")
table(default.test$default,class.70,dnn=c("Actual","Predicted"))
round(mean(class.70!=default.test$default),4)
## using a dummy give missclassificaion of 0.0444 and doesnt led to reduciton 
## in test error

## Problem 2
## 2.a
set.seed(1)
attach(Default)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)
## 2.b
boot.fn <- function(data, index) {
    fit <- glm(default ~ income + balance, data = data, family = "binomial", subset = index)
    return (coef(fit))
}
##2.c
library(boot)
boot(Default, boot.fn, 1000)

##2.d
## All of the error that we got are similiar. 

## Problem 3
set.seed(1)
x <- rnorm(100)
e <- rnorm(100)
## 3.b
b0 <- 2
b1 <- 3
b2 <- -1
b3 <- 0.5
y <- b0 + b1 * x + b2 * x^2 + b3 * x^3 + e
## 3.c
library(leaps)
data.full <- data.frame(y = y, x = x)
regfit.full <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10)
reg.summary <- summary(regfit.full)
par(mfrow = c(2, 2))
plot(reg.summary$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "red", cex = 2, pch = 20)
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "red", cex = 2, pch = 20)
coef(regfit.full, which.max(reg.summary$adjr2))

## 3.d
## forward stepwise
regfit.fwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10, method = "forward")
reg.summary.fwd <- summary(regfit.fwd)
par(mfrow = c(2, 2))
plot(reg.summary.fwd$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary.fwd$cp), reg.summary.fwd$cp[which.min(reg.summary.fwd$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary.fwd$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary.fwd$bic), reg.summary.fwd$bic[which.min(reg.summary.fwd$bic)], col = "red", cex = 2, pch = 20)
plot(reg.summary.fwd$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(reg.summary.fwd$adjr2), reg.summary.fwd$adjr2[which.max(reg.summary.fwd$adjr2)], col = "red", cex = 2, pch = 20)
mtext("Plots of C_p, BIC and adjusted R^2 for forward stepwise selection", side = 3, line = -2, outer = TRUE)
coef(regfit.fwd, which.max(reg.summary.fwd$adjr2))
## backword stepwise
regfit.bwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10, method = "backward")
reg.summary.bwd <- summary(regfit.bwd)
par(mfrow = c(2, 2))
plot(reg.summary.bwd$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary.bwd$cp), reg.summary.bwd$cp[which.min(reg.summary.bwd$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary.bwd$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary.bwd$bic), reg.summary.bwd$bic[which.min(reg.summary.bwd$bic)], col = "red", cex = 2, pch = 20)
plot(reg.summary.bwd$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(reg.summary.bwd$adjr2), reg.summary.bwd$adjr2[which.max(reg.summary.bwd$adjr2)], col = "red", cex = 2, pch = 20)
mtext("Plots of C_p, BIC and adjusted R^2 for backward stepwise selection", side = 3, line = -2, outer = TRUE)
coef(regfit.bwd, which.max(reg.summary.bwd$adjr2))
##3.e
library(glmnet)
xmat <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full)[, -1]
cv.lasso <- cv.glmnet(xmat, y, alpha = 1)
plot(cv.lasso)
best <- cv.lasso$lambda.min
best
fit.lasso <- glmnet(xmat, y, alpha = 1)
predict(fit.lasso, s = best, type = "coefficients")[1:11, ]
##3.f
b7 <- 7
y <- b0 + b7 * x^7 + eps
data.full <- data.frame(y = y, x = x)
regfit.full <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10)
reg.summary <- summary(regfit.full)
par(mfrow = c(2, 2))
plot(reg.summary$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "red", cex = 2, pch = 20)
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "red", cex = 2, pch = 20)
coef(regfit.full, 1)
coef(regfit.full, 2)
coef(regfit.full, 4)
## lasso
xmat <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full)[, -1]
cv.lasso <- cv.glmnet(xmat, y, alpha = 1)
best<- cv.lasso$lambda.min
best
fit.lasso <- glmnet(xmat, y, alpha = 1)
predict(fit.lasso, s = best, type = "coefficients")[1:11, ]

##Problem 4
library(ISLR)
data(College)
set.seed(11)
train = sample(1:dim(College)[1], dim(College)[1] / 2)
test <- -train
College.train <- College[train, ]
College.test <- College[test, ]
##4.b
fit.lm <- lm(Apps ~ ., data = College.train)
pred.lm <- predict(fit.lm, College.test)
mean((pred.lm - College.test$Apps)^2)
##4.c
train.mat <- model.matrix(Apps ~ ., data = College.train)
test.mat <- model.matrix(Apps ~ ., data = College.test)
grid <- 10 ^ seq(4, -2, length = 100)
fit.ridge <- glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <- cv.glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
best.ridge <- cv.ridge$lambda.min
best.ridge
pred.ridge <- predict(fit.ridge, s = best.ridge, newx = test.mat)
mean((pred.ridge - College.test$Apps)^2)

##4.d
fit.lasso <- glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
cv.lasso <- cv.glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
best.lasso <- cv.lasso$lambda.min
best.lasso
pred.lasso <- predict(fit.lasso, s = best.lasso, newx = test.mat)
mean((pred.lasso - College.test$Apps)^2)
predict(fit.lasso, s = best.lasso, type = "coefficients")

##4.e
library(pls)
fit.pcr <- pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, College.test, ncomp = 10)
mean((pred.pcr - College.test$Apps)^2)
##4.f
fit.pls <- plsr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")
pred.pls <- predict(fit.pls, College.test, ncomp = 10)
mean((pred.pls - College.test$Apps)^2)
##4.g
test.avg <- mean(College.test$Apps)
lm.r2 <- 1 - mean((pred.lm - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
ridge.r2 <- 1 - mean((pred.ridge - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
lasso.r2 <- 1 - mean((pred.lasso - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pcr.r2 <- 1 - mean((pred.pcr - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pls.r2 <- 1 - mean((pred.pls - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)