# Import data
Tibet <- source('./chap7tibetskull.dat')$value

attach(Tibet)

Tibet
# ↳ data type is predefined by 1 or 2 (Tibet$Type)

library(MASS)
dis <- lda(Type ~
  Length + Breadth + Height + Fheight + Fbreadth,
  data = Tibet, prior = c(0.5, 0.5))
# Linear discriminant analysis
# ↳ prior (priority): new individual has same probability to attach to type (0.5, 0.5)
names(dis)
# names of lda result

dis$means
# means of each values in each types

dis$counts
# the number of members in each type

# create a new data to be classified
newdata <- rbind(
  c(171, 140.5, 127.0, 69.5, 137.0),
  c(179.0, 132.0, 140.0, 72.0, 138.5)
)
colnames(newdata) <- colnames(Tibet[, -6])
# ↳ to name columns of newdata to make it in the same format as original data
newdata <- data.frame(newdata)
newdata

predict(dis, newdata = newdata)
# ↳ posterior: probability to be in each type
