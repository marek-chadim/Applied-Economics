rm(list=ls())
gc()
setwd("C:/Users/chadi/Dropbox/Applied-Economics")

# Linear Models and Regularization Methods
d <- read.csv("growth/growthdata92_02.csv")
str(d)
names(d)
d<-d[,3:ncol(d)]
dim(d)
sum(is.na(d))

# Choosing Among Models Using the Validation-Set Approach and Cross-Validation
set.seed(42)
train <- sample(nrow(d), nrow(d)*0.8, replace = FALSE)
test <- d[-train, ]

## Best Subset Selection
library(leaps)

###
 predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
 }
###
d3<-d[train,]
k <- 10
n <- nrow(d3)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 27,
        dimnames = list(NULL, paste(1:27)))
###
for (j in 1:k) {
    best.fit <- regsubsets(growth ~ .,
             data = d3[folds != j, ],
             nvmax = 27)
    for (i in 1:27) {
        pred <- predict(best.fit, d3[folds == j, ], id = i)
        cv.errors[j, i] <-
                 mean((d3$growth[folds == j] - pred)^2)
     }
 }
###
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
coef(best.fit, 7)
par(mfrow = c(2, 1))
plot(mean.cv.errors, type = "b")
plot(mean.cv.errors[1:10], type = "b")

## Ridge Regression and the Lasso
library(glmnet)

x <- model.matrix(growth ~ ., d)[, -1]
y <- d$growth
test <- (-train)
y.test <- y[test]
###
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
        lambda = grid)
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
        lambda = grid)

### Ridge Regression

###
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
###
ridge.pred <- predict(ridge.mod, s = bestlam,
        newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]
ridge.coef<- predict(out, type = "coefficients", s = bestlam)[1:20, ]
ridge.coef

### The Lasso

###
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s = bestlam,
        newx = x[test, ])
mean((lasso.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
        s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]

## PCR and PLS Regression
library(pls)

### Principal Components Regression

###
pcr.fit <- pcr(growth ~ ., data = d, subset = train,
        scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
###
pcr.pred <- predict(pcr.fit, x[test, ], ncomp =1)
mean((pcr.pred - y.test)^2)
###
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp =1)
summary(pcr.fit)

### Partial Least Squares

###
set.seed(1)
pls.fit <- plsr(growth ~ ., data = d, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
###
pls.pred <- predict(pls.fit, x[test, ], ncomp = 1)
mean((pls.pred - y.test)^2)
###
pls.fit <- plsr(growth ~ ., data = d, scale = TRUE,
        ncomp = 1)
summary(pls.fit)
###

### Exercise 1: Within-time period prediction

# Naive
naive.fit <- lm(growth ~ 1, data = d, subset = train)
naive.pred <- predict(naive.fit, d[test, ])
mean((naive.pred - y.test)^2)

# Kitchen sink
ks.fit <- lm(growth ~ ., data = d, subset = train)
ks.pred <- predict(ks.fit, d[test, ])
mean((ks.pred - y.test)^2)

# Best subset
bs.fit <- regsubsets(growth ~ ., data = d[train, ], nvmax = 7)
bs.pred <- predict(best.fit, d[test, ], id = 7)
mean((bs.pred - y.test)^2)

# Ridge 
mean((ridge.pred - y.test)^2)

# Lasso
mean((lasso.pred - y.test)^2)

# PCR
mean((pcr.pred - y.test)^2)

# PLS
mean((pls.pred - y.test)^2)

#select the best model
models <- c("naive", "kitchen", "bs", "ridge", "lasso", "pcr", "pls")
errors <- c(mean((naive.pred - y.test)^2), mean((ks.pred - y.test)^2), mean((bs.pred - y.test)^2), mean((ridge.pred - y.test)^2), mean((lasso.pred - y.test)^2), mean((pcr.pred - y.test)^2), mean((pls.pred - y.test)^2))
best.model <- models[which.min(errors)]
best.model

### Exercise 2: Out of sample prediction

#Predict using the whole older dataset
naive.pred2 <- predict(naive.fit, d)
ks.pred2 <- predict(ks.fit, d)
bs.pred2 <- predict(best.fit, d, id = 7)
ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = x)
lasso.pred2 <- predict(lasso.mod, s = bestlam, newx = x)
pcr.pred2 <- predict(pcr.fit, x, ncomp = 1)
pls.pred2 <- predict(pls.fit, x, ncomp = 1)


#Load the test data
d2 <- read.csv("growth/growthdata02_11.csv")
y.test2 <- d2$growth

plot(y.test2, type = "l")
lines(naive.pred2, col = "red")
lines(ks.pred2, col = "blue")

plot(y.test2, type = "l")
lines(bs.pred2, col = "red")
lines(ridge.pred2, col = "blue")
lines(lasso.pred2, col = "green")
lines(pcr.pred2, col = "purple")
lines(pls.pred2, col = "orange")

#Naive
mean((naive.pred2 - y.test2)^2)

#Kitchen sink
mean((ks.pred2 - y.test2)^2)

#Best subset
mean((bs.pred2 - y.test2)^2)

#Ridge
mean((ridge.pred2 - y.test2)^2)

#Lasso
mean((lasso.pred2 - y.test2)^2)

#PCR
mean((pcr.pred2 - y.test2)^2)

#PLS
mean((pls.pred2 - y.test2)^2)

#select the best model
errors2 <- c(mean((naive.pred2 - y.test2)^2), mean((ks.pred2 - y.test2)^2), mean((bs.pred2 - y.test2)^2), mean((ridge.pred2 - y.test2)^2), mean((lasso.pred2 - y.test2)^2), mean((pcr.pred2 - y.test2)^2), mean((pls.pred2 - y.test2)^2))
best.model2 <- models[which.min(errors2)]
best.model2


### Exercise 3: Testing for changing data generating process
d3<-d2[,3:ncol(d2)]
dim(d3)

# Choosing Among Models Using the Validation-Set Approach and Cross-Validation

## Best Subset Selection
 predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
 }
###
dt3<-d3[train,]
k <- 10
n <- nrow(dt3)
folds <- sample(rep(1:k, length = n))
cv.errors3 <- matrix(NA, k, 27,
        dimnames = list(NULL, paste(1:27)))
###
for (j in 1:k) {
    best.fit3 <- regsubsets(growth ~ .,
             data = dt3[folds != j, ],
             nvmax = 27)
    for (i in 1:27) {
        pred3 <- predict(best.fit3, d3[folds == j, ], id = i)
        cv.errors3[j, i] <-
                 mean((dt3$growth[folds == j] - pred3)^2)
     }
 }
###
mean.cv.errors3 <- apply(cv.errors3, 2, mean)
mean.cv.errors3
which.min(mean.cv.errors3)
coef(best.fit3, 1)
par(mfrow = c(2, 1))
plot(mean.cv.errors3, type = "b")
plot(mean.cv.errors3[1:10], type = "b")

## Ridge Regression and the Lasso
x3 <- model.matrix(growth ~ ., d3)[, -1]
y3 <- d3$growth
test <- (-train)
y3.test <- y3[test]
###
grid <- 10^seq(10, -2, length = 100)
ridge.mod3 <- glmnet(x3[train, ], y3[train], alpha = 0,
        lambda = grid)
lasso.mod3 <- glmnet(x3[train, ], y3[train], alpha = 1,
        lambda = grid)

### Ridge Regression

###
cv.out3 <- cv.glmnet(x3[train, ], y3[train], alpha = 0)
plot(cv.out3)
bestlam3 <- cv.out3$lambda.min
bestlam3
###
ridge.pred3 <- predict(ridge.mod3, s = bestlam3,
        newx = x3[test, ])
mean((ridge.pred3 - y3.test)^2)
###
out3 <- glmnet(x3, y3, alpha = 0)
ridge.coef3 <- predict(out3, type = "coefficients", s = bestlam3)[1:20, ]
ridge.coef3

### The Lasso

###
cv.out3 <- cv.glmnet(x3[train, ], y3[train], alpha = 1)
plot(cv.out3)
bestlam3 <- cv.out3$lambda.min
bestlam3
lasso.pred3 <- predict(lasso.mod3, s = bestlam3,
        newx = x3[test, ])
mean((lasso.pred3 - y3.test)^2)
###
out3 <- glmnet(x3, y3, alpha = 1, lambda = grid)
lasso.coef3 <- predict(out3, type = "coefficients",
    s = bestlam3)[1:20, ]
lasso.coef3
lasso.coef3[lasso.coef3 != 0]

## PCR and PLS Regression

### Principal Components Regression

###
pcr.fit3 <- pcr(growth ~ ., data = d3, subset = train,
    scale = TRUE, validation = "CV")
validationplot(pcr.fit3, val.type = "MSEP")
###
pcr.pred3 <- predict(pcr.fit3, x3[test, ], ncomp =1)
mean((pcr.pred3 - y3.test)^2)
###
pcr.fit3 <- pcr(y3 ~ x3, scale = TRUE, ncomp =1)
summary(pcr.fit3)

### Partial Least Squares

###
set.seed(1)
pls.fit3 <- plsr(growth ~ ., data = d3, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit3)
validationplot(pls.fit3, val.type = "MSEP")
###
pls.pred3 <- predict(pls.fit3, x3[test, ], ncomp = 1)
mean((pls.pred3 - y3.test)^2)
###
pls.fit3 <- plsr(growth ~ ., data = d3, scale = TRUE,
    ncomp = 1)
summary(pls.fit3)
###

### Within-time period prediction

# Naive
naive.fit3 <- lm(growth ~ 1, data = d3, subset = train)
naive.pred3 <- predict(naive.fit3, d3[test, ])
mean((naive.pred3 - y3.test)^2)

# Kitchen sink
ks.fit3 <- lm(growth ~ ., data = d3, subset = train)
ks.pred3 <- predict(ks.fit3, d3[test, ])
mean((ks.pred3 - y3.test)^2)

# Best subset
bs.fit3 <- regsubsets(growth ~ ., data = d3[train, ], nvmax = 7)
bs.pred3 <- predict(bs.fit3, d3[test, ], id = 7)
mean((bs.pred3 - y3.test)^2)

# Ridge 
mean((ridge.pred3 - y3.test)^2)

# Lasso
mean((lasso.pred3 - y3.test)^2)

# PCR
mean((pcr.pred3 - y3.test)^2)

# PLS
mean((pls.pred3 - y3.test)^2)

#select the best model
models3 <- c("naive", "kitchen", "bs", "ridge", "lasso", "pcr", "pls")
errors3 <- c(mean((naive.pred3 - y3.test)^2), mean((ks.pred3 - y3.test)^2), mean((bs.pred3 - y3.test)^2), mean((ridge.pred3 - y3.test)^2), mean((lasso.pred3 - y3.test)^2), mean((pcr.pred3 - y3.test)^2), mean((pls.pred3 - y3.test)^2))
best.model3 <- models3[which.min(errors3)]
best.model3

#parameter (in-)stability
coef(best.fit, 10)
coef(best.fit3, 10)

ridge.coef[1:10]
ridge.coef3[1:10]

lasso.coef[lasso.coef != 0]
lasso.coef3[lasso.coef3 != 0]
