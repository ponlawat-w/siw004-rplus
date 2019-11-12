# Load Data
life <- source('./chap4lifeexp.dat')$value
life

# Hierarchical Clustering with 'single' linkage
## using euclidean distance (`dist` function)
out <- hclust(dist(life), method = 'single')

out

names(out)
# "merge" "height" "order" "labels" "method" "call" "dist.method"
## out$height: height of hierarchical of each cluster
## out$order: order to get in to particular cluster

# Draw dendogramme
plclust(out, labels = row.names(life), ylab='Distance')
dev.off()

# Draw three dendogrammes for comapring linkages
par(mfrow = c(1, 3))
hclust.single <- hclust(dist(life), method = 'single')
plclust(hclust.single, labels = row.names(life), ylab='Distance')
title('(a) Single Linkage')
hclust.complete <- hclust(dist(life), method = 'complete')
plclust(hclust.complete, labels = row.names(life), ylab='Distance')
title('(a) Complete Linkage')
hclust.average <- hclust(dist(life), method = 'average')
plclust(hclust.average, labels = row.names(life), ylab='Distance')
title('(a) Average Linkage')

dev.off()

# Cut dendogram at the specified step
four <- cutree(hclust.complete, h = 21)
four # showing the group of the 21st hierarchy

cutree(hclust.complete, h = 40)
## ↳ showing the froup of the 40th hierarchy

cutree(hclust.complete, h = 15)
## ↳ showing the froup of the 40th hierarchy

# Display clusters by group
country.clus <- lapply(1:max(four), function(nc) { row.names(life)[four == nc] })
country.mean <- lapply(1:max(four), function(nc) {
  apply(life[four == nc,], 2, mean)
})
country.clus # display country names in each cluster
country.mean

pairs(life, panel = function(x, y) text(x, y, four))

dev.off()

##----------##

pottery.data <- source('./chap6pottery.dat')$value

# Normalise data
rge <- apply(pottery.data, 2, max) - apply(pottery.data, 2, min)
pottery.dat <- sweep(pottery.data, 2, rge, FUN = '/')
## ↳ rge: range of data (max - min by column)
## ↳ overwrite pottery.dat
### devided by `rge`
## There are many other ways to do the normalisation.

n <- length(pottery.dat[, 1])
wss1 <- (n - 1) * sum(apply(pottery.dat, 2, var))
## apply(pottery.dat, 2, var): apply variances per columnn
## sum(↑): summation of variances
## (n - 1) * ↑

wss <- numeric(0)
for (i in 2:6) {
  W <- sum(kmeans(pottery.dat, i)$withinss)
  wss <- c(wss, W)
}
wss <- c(wss1, wss)

plot(1:6, wss, type = 'l',
  xlab = 'Number of groups',
  ylab = 'Within groups sum of squares',
  lwd = 2)

