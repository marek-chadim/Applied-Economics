rm(list=ls())
gc()
setwd("C:/Users/chadi/Dropbox/Applied-Economics")

# Moving beyond linearity: tree based methods

d <- read.csv("task5/growthdata92_02.csv")
d <- d[, 3:ncol(d)] 
set.seed(42)
train <- sample(1:nrow(d), nrow(d) * .8)
d.test <- d[-train, "growth"]

# Fitting Regression Trees
library(tree)
tree.d <- tree(growth ~ ., d, subset = train)
cv.d <- cv.tree(tree.d)
plot(cv.d$size, cv.d$dev, type = "b")
size <- cv.d$size[which.min(cv.d$dev)]
prune.d <- prune.tree(tree.d, best = size)
yhat <- predict(prune.d, newdata = d[-train, ])
rmse_tree <- sqrt(mean((yhat - d.test)^2))
rmse_tree

# Bagging and Random Forests
library(randomForest)
mtry_values <- seq(1, ncol(d) - 1, by = 1)  
oob_error_values <- numeric(length(mtry_values))

bag.d <- randomForest(growth ~ ., data = d, mtry = length(mtry_values), subset = train, importance = TRUE)
yhat.bag <- predict(bag.d, newdata = d[-train, ])
rmse_bag <- sqrt(mean((yhat.bag - d.test)^2))
rmse_bag

for (i in 1:length(mtry_values)) {
  rf.d <- randomForest(growth ~ ., data = d, mtry = mtry_values[i], subset = train, importance = TRUE)
  oob_error_values[i] <- rf.d$mse[rf.d$ntree]
}
best_mtry <- mtry_values[which.min(oob_error_values)]
rf.d <- randomForest(growth ~ ., data = d, mtry = best_mtry, subset = train, importance = TRUE)
yhat.rf <- predict(rf.d, newdata = d[-train, ])
rmse_rf <- sqrt(mean((yhat.rf - d.test)^2))
rmse_rf

# Boosting
library(gbm)
boost.d <- gbm(growth ~ ., data = d[train, ], distribution = "laplace", n.trees = 10000, interaction.depth = 4, shrinkage = 0.001, verbose = FALSE)
yhat.boost <- predict(boost.d, newdata = d[-train, ], n.trees = 10000)
rmse_boost <- sqrt(mean((yhat.boost - d.test)^2))
rmse_boost

# Bayesian Additive Regression Trees
library(BART)
x <- d[, -2]
y <- d[, "growth"]
xtrain <- x[train, ]
ytrain <- y[train]
xtest <- x[-train, ]
ytest <- y[-train]
bartfit <- gbart(xtrain, ytrain, x.test = xtest)
ord <- order(bartfit$varcount.mean, decreasing = T)
yhat.bart <- bartfit$yhat.test.mean
rmse_bart <- sqrt(mean((ytest - yhat.bart)^2))
rmse_bart

# Model Comparison
models <- c("Regression Tree", "Bagging", "Random Forest", "Boosting", "BART")
rmse_values <- c(rmse_tree, rmse_bag, rmse_rf, rmse_boost, rmse_bart)
rmse_comparison <- data.frame(Model = models, RMSE = rmse_values)
barplot(rmse_comparison$RMSE, names.arg = rmse_comparison$Model, col = "skyblue", main = "Model Comparison", ylab = "RMSE")

# Exercise 2: Out-of-sample prediction
d_92_02 <- read.csv("task5/growthdata92_02.csv")
d_92_02 <- d_92_02[, 3:ncol(d_92_02)]
d_02_11 <- read.csv("task5/growthdata02_11.csv")
d_02_11 <- d_02_11[, 3:ncol(d_02_11)]
x_train <- model.matrix(growth ~ ., d_92_02)[, -1]
y_train <- d_92_02$growth
x_test <- model.matrix(growth ~ ., d_02_11)[, -1]
y_test <- d_02_11$growth

# Regression Tree
tree.d <- tree(growth ~ ., data = d_92_02)
cv.d <- cv.tree(tree.d)
plot(cv.d$size, cv.d$dev, type = "b")
size <- cv.d$size[which.min(cv.d$dev)]
prune.d <- prune.tree(tree.d, best = size)
yhat_tree <- predict(prune.d, newdata = d_02_11)
rmse_tree <- sqrt(mean((yhat_tree - y_test)^2))
rmse_tree

# Bagging
bag.d <- randomForest(growth ~ ., data = d_92_02, mtry = length(mtry_values), importance = TRUE)
yhat_bag <- predict(bag.d, newdata = d_02_11)
rmse_bag <- sqrt(mean((yhat_bag - y_test)^2))
rmse_bag

# Random Forest
oob_error_values2 <- numeric(length(mtry_values))
for (i in 1:length(mtry_values)) {
  rf.d <- randomForest(growth ~ ., data = d_92_02, mtry = mtry_values[i], subset = train, importance = TRUE)
  oob_error_values2[i] <- rf.d$mse[rf.d$ntree]
}
best_mtry2 <- mtry_values[which.min(oob_error_values2)]
rf.d <- randomForest(growth ~ ., data = d_92_02, mtry = best_mtry2, subset = train, importance = TRUE)
yhat_rf <- predict(rf.d, newdata = d_02_11)
rmse_rf <- sqrt(mean((yhat_rf - y_test)^2))
rmse_rf

# Boosting
boost.d <- gbm(growth ~ ., data = d_92_02, distribution = "gaussian", n.trees = 10000, shrinkage = 0.001, interaction.depth = 4)
yhat_boost <- predict(boost.d, newdata = d_02_11, n.trees = 10000)
rmse_boost <- sqrt(mean((yhat_boost - y_test)^2))
rmse_boost

# BART
x <- d_92_02[, -2]
y <- d_92_02[, "growth"]
bartfit <- gbart(x, y, x.test = d_02_11[, -2])
yhat_bart <- bartfit$yhat.test.mean
rmse_bart <- sqrt(mean((y_test - yhat_bart)^2))
rmse_bart

# Model Comparison
models <- c("Regression Tree", "Bagging", "Random Forest", "Boosting", "BART")
rmse_values <- c(rmse_tree, rmse_bag, rmse_rf, rmse_boost, rmse_bart)
rmse_comparison <- data.frame(Model = models, RMSE = rmse_values)
barplot(rmse_comparison$RMSE, names.arg = rmse_comparison$Model, col = "skyblue", main = "Out-of-Sample RMSE Comparison", ylab = "RMSE")

# Exercise 3: Testing for Changing Data Generating Process
d2 <- read.csv("task5/growthdata02_11.csv")
d2 <- d2[, 3:ncol(d2)]
train <- sample(1:nrow(d2), nrow(d2) * .8)
d2.test <- d2[-train, "growth"]

# Regression Tree
tree.d <- tree(growth ~ ., d2, subset = train)
cv.d <- cv.tree(tree.d)
plot(cv.d$size, cv.d$dev, type = "b")
size <- cv.d$size[which.min(cv.d$dev)]
prune.d2 <- prune.tree(tree.d, best = size)
yhat <- predict(prune.d2, newdata = d2[-train, ])
rmse_tree2 <- sqrt(mean((yhat - d2.test)^2))
rmse_tree2

# Bagging and Random Forests
bag.d <- randomForest(growth ~ ., data = d2, mtry = length(mtry_values), subset = train, importance = TRUE)
yhat.bag <- predict(bag.d, newdata = d2[-train, ])
rmse_bag2 <- sqrt(mean((yhat.bag - d2.test)^2))
rmse_bag2

oob_error_values3 <- numeric(length(mtry_values))
for (i in 1:length(mtry_values)) {
  rf.d <- randomForest(growth ~ ., data = d2, mtry = mtry_values[i], subset = train, importance = TRUE)
  oob_error_values3[i] <- rf.d$mse[rf.d$ntree]
}
best_mtry3 <- mtry_values[which.min(oob_error_values3)]
rf.d <- randomForest(growth ~ ., data = d2, mtry = best_mtry3, subset = train, importance = TRUE)
yhat.rf <- predict(rf.d, newdata = d2[-train, ])
rmse_rf2 <- sqrt(mean((yhat.rf - d2.test)^2))
rmse_rf2

# Boosting
boost.d2 <- gbm(growth ~ ., data = d2[train, ], distribution = "laplace", n.trees = 10000, interaction.depth = 4, shrinkage = 0.001, verbose = FALSE)
yhat.boost <- predict(boost.d2, newdata = d2[-train, ], n.trees = 10000)
rmse_boost2 <- sqrt(mean((yhat.boost - d2.test)^2))
rmse_boost2

# BART
x <- d2[, -2]
y <- d2[, "growth"]
xtrain <- x[train, ]
ytrain <- y[train]
xtest <- x[-train, ]
ytest <- y[-train]
bartfit2 <- gbart(xtrain, ytrain, x.test = xtest)
ord2 <- order(bartfit2$varcount.mean, decreasing = T)
yhat.bart <- bartfit$yhat.test.mean
rmse_bart2 <- sqrt(mean((ytest - yhat.bart)^2))
rmse_bart2

# Model Comparison
models <- c("Regression Tree", "Bagging", "Random Forest", "Boosting", "BART")
rmse_values2 <- c(rmse_tree2, rmse_bag2, rmse_rf2, rmse_boost2, rmse_bart2)
rmse_comparison2 <- data.frame(Model = models, RMSE = rmse_values2)
barplot(rmse_comparison2$RMSE, names.arg = rmse_comparison2$Model, col = "skyblue", main = "Model Comparison", ylab = "RMSE")

# Parameter Stability
plot(prune.d)
text(prune.d, pretty = 0)
plot(prune.d2)
text(prune.d2, pretty = 0)
importance(rf.d)
varImpPlot(rf.d)
importance(rf.d2)
varImpPlot(rf.d2)
summary(boost.d)
summary(boost.d2)
bartfit$varcount.mean[ord]
bartfit2$varcount.mean[ord2]
