rm(list=ls())
gc()
setwd("C:/Users/chadi/Dropbox/Applied-Economics")
load("WVS/WV6_Data_R_v20201117.rdata")
d <- WV6_Data_R_v20201117

library(dplyr)
library(countrycode)

# Creating the data set
d <- d |>
  mutate(
    country = countrycode(V2, origin = "iso3n", destination = "country.name")
  )

View(d$country)

selected_vars <- c("V5", "V6", "V7", "V8", "V9", "V10", "V11", "V23", "V24", 
                   "V55", "V56", "V59", "V67", "V69", "V70", "V71", "V72", 
                   "V73", "V74", "V76", "V77", "V78", "V79", "V102", "V103", 
                   "V104", "V105", "V106", "V107", "V108", "V109", "V110", 
                   "V111", "V112", "V113", "V114", "V115", "V116", "V117", 
                   "V118", "V119", "V120", "V121", "V122", "V123", "V124", 
                   "V127", "V128", "V130", "V131", "V132", "V133", 
                   "V134", "V135", "V136", "V137", "V138", "V139", "V97", 
                   "V98", "V99", "V100", "V101", "V140", "V141", "V142", 
                   "V143", "V145", "V146", "V147", "V148", "V149", "V150", 
                   "V151", "V152", "V153", "V154", "V155", "V156", "V157", 
                   "V158", "V159", "V160", "V161", "V162", "V163", "V164", 
                   "V165", "V166", "V167", "V168", "V169", "V170")

d <- d|>
  select(country, all_of(selected_vars))

d <- d |>
  mutate(across(where(is.numeric), ~na_if(., -1))) |>
  mutate(across(where(is.numeric), ~na_if(., -2))) |>
  mutate(across(where(is.numeric), ~na_if(., -3))) |>
  mutate(across(where(is.numeric), ~na_if(., -4))) |>
  mutate(across(where(is.numeric), ~na_if(., -5)))

d <- d |>
  group_by(country) |>
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

d <- d[, colSums(is.na(d)) == 0]
colSums(is.na(d)) == 0

write.csv(d, "WVS/WVS.csv", row.names = FALSE)

## Principal Components Analysis
wvs <- read.csv("WVS/WVS.csv")
View(wvs)
row.names(wvs) <- wvs[,1]
wvs <- wvs[,-1]   
countries <- row.names(wvs)
countries
names(wvs)
apply(wvs, 2, mean)
apply(wvs, 2, var)
pr.out <- prcomp(wvs, scale = TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
par(mfrow = c(1, 1))
#save plot as image

biplot(pr.out, scale = 0)

pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var / sum(pr.var)
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

## Clustering

#Countries
wvs.data <- wvs
wvs.labs <- rownames(wvs)
sd.data <- scale(wvs.data)
dim(wvs.data)
wvs.labs[1:4]

par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
     labels = wvs.labs, main = "Complete Linkage")
plot(hclust(data.dist, method = "average"),
     labels = wvs.labs, main = "Average Linkage",
     xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"),
     labels = wvs.labs,  main = "Single Linkage",
     xlab = "", sub = "", ylab = "")
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, wvs.labs)
par(mfrow = c(1, 1))
plot(hc.out, labels = wvs.labs)
abline(h = 139, col = "red")
hc.out
set.seed(42)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = wvs.labs,
     main = "Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), wvs.labs)

#Variables
wvs.data <- t(wvs)
wvs.labs <- names(wvs)
dim(wvs.data)
wvs.labs[1:4]
table(wvs.labs)

sd.data <- scale(wvs.data)
par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
     labels = wvs.labs, main = "Complete Linkage")
plot(hclust(data.dist, method = "average"),
     labels = wvs.labs, main = "Average Linkage",
     xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"),
     labels = wvs.labs,  main = "Single Linkage",
     xlab = "", sub = "", ylab = "")
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, wvs.labs)
par(mfrow = c(1, 1))
plot(hc.out, labels = wvs.labs)
abline(h = 139, col = "red")
hc.out
set.seed(42)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
pr.out <- prcomp(wvs.data, scale = TRUE)
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = wvs.labs,
     main = "Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), wvs.labs)

