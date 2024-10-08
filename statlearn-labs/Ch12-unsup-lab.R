
# Lab: Unsupervised Learning
library(MASS)
library(ISLR2)


## Principal Components Analysis
View(USArrests)
###
states <- row.names(USArrests)
states
###
names(USArrests)
###
apply(USArrests, 2, mean)
###
apply(USArrests, 2, var)
###
pr.out <- prcomp(USArrests, scale = TRUE)
###
names(pr.out)
###
pr.out$center
pr.out$scale
###
pr.out$rotation
###
dim(pr.out$x)
###
biplot(pr.out, scale = 0)
###
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)
###
pr.out$sdev
###
pr.var <- pr.out$sdev^2
pr.var
###
pve <- pr.var / sum(pr.var)
pve
###
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
    ylab = "Proportion of Variance Explained", ylim = c(0, 1),
    type = "b")
plot(cumsum(pve), xlab = "Principal Component",
    ylab = "Cumulative Proportion of Variance Explained",
    ylim = c(0, 1), type = "b")
###
a <- c(1, 2, 8, -3)
cumsum(a)

## Matrix Completion 

###
X <- data.matrix(scale(USArrests))
pcob <- prcomp(X)
summary(pcob)
###
sX <- svd(X)
names(sX)
round(sX$v, 3)
###
pcob$rotation
###
t(sX$d * t(sX$u))
pcob$x
###
nomit <- 20
set.seed(15)
ina <- sample(seq(50), nomit)
inb <- sample(1:4, nomit, replace = TRUE)
Xna <- X
index.na <- cbind(ina, inb)
Xna[index.na] <- NA
###
fit.svd <- function(X, M = 1) {
   svdob <- svd(X)
   with(svdob,
       u[, 1:M, drop = FALSE] %*%
       (d[1:M] * t(v[, 1:M, drop = FALSE]))
     )
}
###
###
Xhat <- Xna
xbar <- colMeans(Xna, na.rm = TRUE)
Xhat[index.na] <- xbar[inb]
###
thresh <- 1e-7
rel_err <- 1
iter <- 0
ismiss <- is.na(Xna)
mssold <- mean((scale(Xna, xbar, FALSE)[!ismiss])^2)
mss0 <- mean(Xna[!ismiss]^2)
###
while(rel_err > thresh) {
    iter <- iter + 1

    # Step 2(a)

    Xapp <- fit.svd(Xhat, M = 1)

    # Step 2(b)

    Xhat[ismiss] <- Xapp[ismiss]

    # Step 2(c)

    mss <- mean(((Xna - Xapp)[!ismiss])^2)
    rel_err <- (mssold - mss) / mss0
    mssold <- mss
    cat("Iter:", iter, "MSS:", mss,
      "Rel. Err:", rel_err, "\n")
    }
###
cor(Xapp[ismiss], X[ismiss])

## Clustering

### $K$-Means Clustering
###
set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
###
km.out <- kmeans(x, 2, nstart = 20)
###
km.out$cluster
###
par(mfrow = c(1, 2))
plot(x, col = (km.out$cluster + 1),
    main = "K-Means Clustering Results with K = 2",
    xlab = "", ylab = "", pch = 20, cex = 2)
###
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1),
    main = "K-Means Clustering Results with K = 3",
    xlab = "", ylab = "", pch = 20, cex = 2)
###
set.seed(4)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss

### Hierarchical Clustering

###
hc.complete <- hclust(dist(x), method = "complete")
###
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")
###
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage",
    xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage",
    xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage",
    xlab = "", sub = "", cex = .9)
###
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
###
cutree(hc.single, 4)
###
xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"),
    main = "Hierarchical Clustering with Scaled Features")
###
x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"),
    main = "Complete Linkage with Correlation-Based Distance",
    xlab = "", sub = "")

## NCI60 Data Example

###
library(ISLR2)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
###
dim(nci.data)
###
nci.labs[1:4]
table(nci.labs)

### PCA on the NCI60 Data

###
pr.out <- prcomp(nci.data, scale = TRUE)
###
Cols <- function(vec) {
   cols <- rainbow(length(unique(vec)))
   return(cols[as.numeric(as.factor(vec))])
 }
###
par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
    xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19,
    xlab = "Z1", ylab = "Z3")
###
summary(pr.out)
###
plot(pr.out)
###
pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
par(mfrow = c(1, 2))
plot(pve,  type = "o", ylab = "PVE",
    xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE",
    xlab = "Principal Component", col = "brown3")

### Clustering the Observations of the NCI60 Data

###
sd.data <- scale(nci.data)
###
par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
    labels = nci.labs, main = "Complete Linkage")
plot(hclust(data.dist, method = "average"),
    labels = nci.labs, main = "Average Linkage",
    xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"),
    labels = nci.labs,  main = "Single Linkage",
    xlab = "", sub = "", ylab = "")
###
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)
###
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")
###
hc.out
###
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
###
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs,
    main = "Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
###
