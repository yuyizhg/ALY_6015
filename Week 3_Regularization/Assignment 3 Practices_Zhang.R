# Exercise 1
library(lars)
data("diabetes")
library(glmnet)
attach(diabetes)


# Exercise 2
par(mfrow = c(2,5))
for (i in 1:10) {
  plot(x[,i],y)
  abline(lm(y ~ x[,i]))
}
layout(1)


# Exercise 3
model_ols <- lm(y ~ x)
summary(model_ols)


# Exercise 4
model_lasso <- glmnet(x, y)
plot.glmnet(model_lasso, xvar = "norm", label = TRUE)


# Exercise 5
cv_fit <- cv.glmnet(x = x, y = y, alpha = 1, nlambda = 1000)
plot.cv.glmnet(cv_fit)
cv_fit$lambda.min


# Exercise 6
fit <- glmnet(x = x, y = y, alpha = 1, lambda = cv_fit$lambda.min)
fit$beta


# Exercise 7
cv_fit$lambda.lse
fit <- glmnet(x = x, y = y, alpha = 1, lambda = cv_fit$lambda.lse)
fit$beta


# Exercise 8
model_ols2 <- lm(y~x2)
summary(model_ols2)


# Exercise 9
model_lasso1 <- glmnet(x2, y)
plot.glmnet(model_lasso1, xvar = "norm", label = TRUE)


# Exercise 10
cv_fit1 <- cv.glmnet(x = x2, y = y, alpha = 1, nlambda = 1000)
plot.cv.glmnet(cv_fit1)

fit1 <- glmnet(x = x2, y = y, alpha = 1, lambda=cv_fit1$lambda.min)
fit1$beta
