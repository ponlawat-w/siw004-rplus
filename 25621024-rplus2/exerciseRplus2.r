airpoll <- source('./chap2airpoll.dat')$value

airpoll

# Look on mortality data by column index
airpoll[,7]

# Get column names
names(airpoll)

# Look on mortality data by column name
airpoll$Mortality

# Error, because it will find `Mortality` as another variable
## Not in `airpoll`
Mortality

# Attach the variable `airpoll` to the system
## From now it will be able to look on data by typing only column name
attach(airpoll)
Mortality

# Detach (reverse of attach())
detach(airpoll)

# Plots of Mortality
## a) Scatterplots with x = SO₂, y = Mortality
attach(airpoll)
par(mfrow = c(2, 2))
par(pty = 's')
plot(SO2, Mortality, pch = 1, lwd = 2)
title('(a) Scatterplots', lwd = 2)

## b) Plot with linear model
lm(Mortality~SO2) # Get the linear model of Mortality to SO2
### e.g. result Intercept = 917.9035, SO2 = 0.4181
###   means y = 917.9035 + 0.4181x
plot(SO2, Mortality, pch = 1, lwd = 3)
abline(lm(Mortality~SO2), lwd = 2)
title('(b) Linear Model', lwd = 2)

## c) Jittering (Shaking: add some noise to data)
airpoll1 <- jitter(cbind(SO2, Mortality)) # Jittering will change the value of data
### so make sure that we use it only in graphical term
### ※ NEVER USE JITERRING IN MODELLING TERM
plot(airpoll1[,1], airpoll1[,2], xlab = 'SO2', ylab = 'Mortality', pch = 1, lwd = 2)
title('(c) Jittering', lwd = 2)

## d) Rug (with histogram, indicator on axises)
plot(SO2, Mortality, pch = 1, lwd = 2)
rug(jitter(SO2), side = 1)
rug(jitter(Mortality), side = 2)
title('(d) Rugging', lwd = 2)

##---------##

# Plot by names (use name as a point in plot)
names <- abbreviate(row.names(airpoll))
par(mfrow = c(1, 1))
plot(SO2, Mortality, lwd = 2, type = 'n') # n => nothing, to make points disappeared
text(SO2, Mortality, labels = names, lwd = 2) # replace points with text described in `names`

dev.off() # Turn off plot window

##----------##

# Plotting Linear/Non-Linear Models
par(fig = c(0, 0.7, 0, 0.7)) # Plot in 0vw, 0vh to 70vw, 70vh
plot(SO2, Mortality, lwd = 2)
abline(lm(Mortality~SO2), lwd = 2)      # Linear Model
lines(lowess(SO2, Mortality), lwd = 2)  # Local-Weighted Regression
## ↳ lowess() is to identify non-linear regression

## Note: non-linear model can be linearised (e.g. logarithm equation)

# Draw a histogram
par(fig = c(0, 0.7, 0.65, 1), new = TRUE)
hist(SO2, lwd = 2) 

# Draw a boxplot
par(fig = c(0.65, 1, 0, 0.7), new = TRUE)
boxplot(Mortality, lwd = 2) 
## lower-bound of box => Q[25%]
## middle line of box => Q[50%]
## upper-bound of box => Q[75%]
##   |---[--|--]--|
## Q=0   1  2  3  4

dev.off()

##----------##

# Convex Hull (Data Bounding)
## To identify extreme data
hull <- chull(SO2, Mortality) # return the index of extreme data
plot(SO2, Mortality, pch = 1)
polygon(SO2[hull], Mortality[hull], density = 15, angle = 30)

cor(SO2, Mortality)
cor(SO2[-hull], Mortality[-hull]) # Remove column at indices described in hull
## e.g. `airpoll[, -7]` => remove column 7
## e.g. `airpoll[, -c(1, 7)]` => remove column 1 and 7
## e.g. `airpoll[-1:-10, -c(1, 7)]` => remove rows 1 to 10, column 1 and 7
## Hence, `cor(SO2[-hull], Mortality[-hull])` is correlation of data whose extreme data are removed

dev.off()

##----------##

# Chiplot

## Function declaration
source('../functions/chiplot.r');

chiplot(SO2, Mortality, vlabs = c('SO2', 'Mortality'))
# If all of them are inside => two variables are clearly independent.

dev.off()

##----------##

# bvbox

# Function Declaration

source('../functions/biweight.r')
source('../functions/bvbox.r')

bvbox(cbind(SO2, Mortality), xlab = 'SO2', ylab = 'Mortality')
bvbox(cbind(SO2, Mortality), xlab = 'SO2', ylab = 'Mortality', method = 'O')

dev.off();

##----------##

# Bivariate Densities (bivden)

source('../functions/bivden.r')

par(mfrow = c(1, 2))

## 3D plot of bivariate densities
den1 <- bivden(SO2, Mortality)
persp(den1$seqx, den1$seqy, den1$den,
  xlab = 'SO2', ylab = 'Mortality', zlab = 'Density',
  lwd = 2)

## contour plot
plot(SO2, Mortality)
contour(den1$seqx, den1$seqy, den1$den,
  lwd = 2, nlevels = 20, add = TRUE)

# Check correlation between two columns
cor(cbind(SO2, Mortality))

dev.off()

##----------##

# Pairs

pairs(airpoll)
## pair each column to see their correlations

pairs(airpoll, panel = function(x, y) {
  abline(lsfit(x, y)$coef, lwd = 2) # linear line
  lines(lowess(x, y), lty = 2, lwd = 2) # non-linear line
  points(x, y) # scatter
})
## pair each column to see their correlations
## with linear model line and non-linear model line
## for comparing between linear and non-linear fit
### ※ constant line means non-relation

dev.off()

##----------##

# Conditional Plot

coplot(Mortality~SO2 | Popden)
## plot of relationship of Mortality to SO2, conditional to Popden

coplot(Mortality~SO2 | Popden, panel = function(x, y, col, pch) {
  panel.smooth(x, y, span = 1) # smoothing relation of two variables
})

coplot(Mortality~SO2 | Popden, panel = function(x, y, col, pch) {
  panel.smooth(x, y, span = 1) # smoothing relation of two variables
}, overlap = 0) # conditioning Popden without overlapping range

coplot(Mortality~SO2 | Popden, panel = function(x, y, col, pch) {
  panel.smooth(x, y, span = 1) # smoothing relation of two variables
}, overlap = 0.7) # conditioning Popden with 0.7 proportion of overlapping

coplot(Mortality~SO2 | Popden, panel = function(x, y, col, pch) {
  panel.smooth(x, y, span = 1) # smoothing relation of two variables
}, overlap = 0.7, number = 4) # conditioning Popden into 4 ranges

dev.off()

##----------##
