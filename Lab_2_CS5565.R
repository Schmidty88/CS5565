## problem 2
auto <- read.csv("Auto.csv", TRUE, ",")
fix(auto)
mpg = auto$mpg
horsepower = auto$horsepower
k <- lm(horsepower, mpg)
summary(k)
## When looking at the residuals I would say no since the numbers are so large
## The relationship isn't strong as stated above
## The response is negative
plot(horsepower, mpg)
## the predicted is about 20 when using the plot 
## p-value is 2.2e-16 and F-statistic is 599.7
## part b
plot(horsepower~mpg)
abline(reg=lm(horsepower~mpg))
## part c
plot(horsepower,mpg)
fit<-lm(mpg~horsepower)
abline(fit)
## this one issue i see with the line is that there is a lot of data below it 

## problem 3
mpg = auto$mpg
horsepower = auto$horsepower
displacement = auto$displacement
weight = auto$weight
acceleration = auto$acceleration
year = auto$year
cylinders = auto$cylinders
pairs(~mpg+horsepower+displacement+weight+acceleration+year+cylinders)
##part B
x <- auto[1:8]
cor(x)
##part C
fit2 <- lm(mpg ~ horsepower, data = auto)
summary(fit2)
## Yes becasue we can see that the P-value is corresponding with the F-statistic which tells us
## there is relationship between mpg and horsepower
## Which predictors appear to have a statistically significant relationship to the response?
## We can find this by calculating the residual error relative to the response which is 
summary(fit2)$sigma/mean(mpg)*100
## that gives us 20.923 
## Is the relationship between the predictor and the response positive or negative ?
## The more horsepower a car has will result in lower mpg according to the linear regression.
## Part D
par(mfrow = c(2, 2))
plot(fit2)
## The problems that I see with the fit is that it shows no linearity in the data. If we look at
## residuals Vs Leverage we can see that it has many outliers 
## Part E
fit3 <- lm(mpg ~ cylinders * displacement+displacement * weight, data = auto[, 1:8])
summary(fit3)
## if we look at the P-values for cylinders:displacement it is not significant but 
## the same can not be said for displacement:weight
## Part F
par(mfrow = c(2,2))
plot(log(horsepower), mpg)
plot(sqrt(horsepower),mpg)
plot((horsepower)^2, mpg)
## the squared function seems to be not close to linear but the log is the most linear
##
##
##Question 4
carseats <- read.csv("Carseats_Lab2.csv", TRUE, ",")
fit4 <- lm(Sales~Price + Urban + US, data = carseats)
summary(fit4)
## Part B
## for Price we can say 
abs(summary(fit4)$coef[2, 1]) * 1000
## for Urban we can say
abs(summary(fit4)$coef[3, 1]) * 1000
## for US we can say 
abs(summary(fit4)$coef[4, 1]) * 1000
## Part C
Sales = summary(fit4)$coef[1,1]+ summary(fit4)$coef[2,1] + summary(fit4)$coef[3,1] + summary(fit4)$coef[4,1]
## Part D
## We are able to reject the null hypothesis for the price and US
## Part E
fit5 <- lm(Sales ~ Price + US, data = carseats)
summary(fit5)
## Part F
summary(fit4)$r.sq * 100
summary(fit5)$r.sq * 100
## comparing those two we can see that the R-squared for the smaller data is better 
## Part G
confint(fit5)
## Part H
par(mfrow = c(2, 2))
plot(fit5)
## looking at the residuals vs leverage we can see that there are many outliers 
##
##
##
## Question 5
set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
## Part A
fit6 <- lm(y ~ x + 0)
summary(fit6)
## We have a standard error, t-statistic and p-valuse.
## The small p-value will allow us to reject H_0
##Part B
fit7 <- lm(x ~ y + 0)
summary(fit7)
## we have a standard error, t-statistic and p-value
## the small p-value will allow us to reject H_0
## Part C
## the t-statistic and p-value are the same and will create the same line
## Part D
n <- length(x)
t <- sqrt(n - 1)*(x %*% y)/sqrt(sum(x^2) * sum(y^2) - (x %*% y)^2)
as.numeric(t)
## this is the same t-value we have seen in the previou modles
## Part E
## if we replace x by y it will give the same result 
n <- length(y)
t2 <- sqrt(n - 1)*(y %*% x)/sqrt(sum(y^2) * sum(x^2) - (y %*% x)^2)
as.numeric(t2)
## Part F
fit8 <- lm(y ~ x)
summary(fit8)
fit9 <- lm(x ~ y)
summary(fit9)
summary(fit8)$coef[2, 3]
## looking the summary fit8 and fit9 we can see it matches the t-statistic for summary(fit8)$coef[2,3]