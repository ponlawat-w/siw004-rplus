##----------##

# Load data to variable
huswif <- source('./chap1huswif.dat')$value

# Show column names
names(huswif)

# View all rows from column 2 (index starts at 1)
huswif[,2]

# View first 5 rows from column 1
huswif[1:5, 1]

# View first 5 rows from column 2 to 3
huswif[1:5, 2:3]

# View 5th row
huswif[5,]

# Show dimension of variable
dim(huswif) # `[1] 10 5` => 10 rows, 5 columnss

##----------##

# Find mean of variable
## cbind first
## cbind: ColumnBind to put vector in to columns
huswif1 <- cbind(
  huswif[,1],
  huswif[,2],
  huswif[,3],
  huswif[,4],
  huswif[,5]
)

mean(huswif1) # overall means, regardless of columns
c(
  mean(huswif[,1]),
  mean(huswif[,2]),
  mean(huswif[,3]),
  mean(huswif[,4]),
  mean(huswif[,5])
) # finding mean of each column and put everything together in c()
# c() => combine values into array or list

# Variances of each column
c(
  var(huswif[,1]),
  var(huswif[,2]),
  var(huswif[,3]),
  var(huswif[,4]),
  var(huswif[,5])
)

# Invariance-Covariance matrix
var(huswif)
var(huswif1)
## ↳ symmetric matrix expected
## e.g. var([1,2]) is negative => correlation column 1 to 2 is negative (non-normalised)

# Correlation
## To compare correlation over columns, don't use var() use cor()
cor(huswif)
cor(huswif1)

# Distance
dis <- dist(huswif)
dis # dis contains *euclidean* distances between each row

# dist2full is not standard function: below is declaration
dist2full <- function(dis) {
  n <- attr(dis, 'Size')
  full <- matrix(0, n, n)
  full[lower.tri(full)] <- dis
  return(full + t(full))
}

dis.matrix <- dist2full(dis)
dis.matrix # dis.matrix is full-matrix which contains euclidean distances between each row

# 【Summary of Distance Functions】
## dis() => stair matrix of distances
## dist2full() => full matrix of distances with 0

# However, distances from dis() and dist2full() is distance that depends on original units (not-normalised)
# The way to normalise the values is to devide them by the standard variation

# Normalised Distances
## 1) find standard variation
std <- c(
  sd(huswif[,1]),
  sd(huswif[,2]),
  sd(huswif[,3]),
  sd(huswif[,4]),
  sd(huswif[,5])
)
std

## 2) perform sweep() to normalise values in matrix
huswif.std <- sweep(huswif, 2, std, FUN = '/')
### PARAMETERS
### x       = huswif  // array
### MARGIN  = 2       // a vector of indices giving the extent(s) of x which correspond to STATS.
### STATS   = std     // the summary statistic which is to be swept out.
### FUN     = '/'     // the function to be used to carry out the sweep.
huswif.std

## 3) Find the distance
dis <- dist(huswif.std)
dis.matrix <- dist2full(dis)
round(dis.matrix, digits = 2) # display matrix `dis.matrix` but round each value to 2 decimal points
### Now we get the matrix of normalised euclidean distances

##----------##

# Load MASS library
library(MASS)

X <- mvrnorm(         # mvrnorm() - Produces one or more samples from the specified multivariate normal distribution.
  200,                # n:      the number of samples required.
  mu = c(0,0),        # mu:     a vector giving the means of the variables.
  Sigma = matrix(     # Sigma:  a positive-definite symmetric matrix specifying the covariance matrix of the variables.
    c(1, 0.5, 0.5, 1.0),
    ncol=2
  )
)

X

mean(X[,1])
mean(X[,2])
var(X[,1])  # must be close to 1
var(X[,2])  # must be close to 1

# QQ Plot
par(mfrow = c(1, 2)) # par() - can be used to set or query graphical parameters.
# qqnorm(), qqline() - is a generic function the default method of which produces a normal QQ plot of the values in y.
## qqline adds a line to a “theoretical”, by default normal, quantile-quantile plot which passes through the probs quantiles,
## by default the first and third quartiles.
qqnorm(X[,1], ylab = 'Ordered observations')
qqline(X[,1])
qqnorm(X[,2], ylab = 'Ordered observations')
qqline(X[,2])

# QQ Plot of log(abs)
par(mfrow = c(1, 2))
qqnorm(log(abs(X[,1])), ylab = 'Ordered observations')
qqline(log(abs(X[,1])))
qqnorm(log(abs(X[,2])), ylab = 'Ordered observations')
qqline(log(abs(X[,2])))

# Plot 4 QQs for comparison
par(mfrow = c(1, 4))
qqnorm(X[,1], ylab = 'Ordered observations', main = 'QQ Plot of X[,1]')
qqline(X[,1])
qqnorm(X[,2], ylab = 'Ordered observations', main = 'QQ Plot of X[,2]')
qqline(X[,2])
qqnorm(log(abs(X[,1])), ylab = 'Ordered observations', main = 'QQ Plot of log(abs(X[,1]))')
qqline(log(abs(X[,1])))
qqnorm(log(abs(X[,2])), ylab = 'Ordered observations', main = 'QQ Plot of log(abs(X[,2]))')
qqline(log(abs(X[,2])))
