# PRINCIPAL COMPONENTS ANALYSIS (PCA)

# Load Data
usair.dat <- source('./chap3usair.dat')$value
attach(usair.dat)

head(usair.dat)

# Correlation of data without first column
cor(usair.dat[, -1])

# Find Principal Components Analysis (PCA)
## cor = TRUE => to  perform over the correlation rather than covariance
usair.pc <- princomp(usair.dat[, -1], cor = TRUE)

# To view summary of `usair.pc`
summary(usair.pc, loadings = TRUE)
## Comp.1~6 => y1~y6
## cumulativeProportion(comp.3) > 0.8
### means we will use dimension of only component 1 ~ 3

## Loadings
### Comp.1 => Neg.Temp(x[1]): 0.330, Manuf(x[2]): 0.612, ...
### means y1 = 0.33x[1] + 0.612x[2] + ...
### and we will use only y1 ~ y3

## What we have learned
### 1. We can reduce dimension to 3 from 6.
### 2. We know that y1, y2, y3 are uncorrelated.

## Then,
### SO2 = f(NT, Manuf, ..., Days) => TO BE =>
### 1) (y1, y2, y3) = f(NT, Manuf, ..., Days)
### 2) SO2 = f(y1, y2, y3)
#### ↳ we have reduced the number of dimension in the function
#### of parameteres that affects to SO2
#### Then, SO2 = β0 + β1y1 + β2y2 + β3y3 // Linear Regression of the step 2 above
#### Then, we rewrite the original equation.

# Create linear regression model on first 3 columns of PCA
out <- lm(SO2 ~ (usair.pc$scores[,1] + usair.pc$scores[,2] + usair.pc$scores[,3]))
out
# ↳【Result】
#    Call:
#    lm(formula = SO2 ~ (usair.pc$scores[, 1] + usair.pc$scores[,
#        2] + usair.pc$scores[, 3]))
#    Coefficients:
#             (Intercept)  usair.pc$scores[, 1]  usair.pc$scores[, 2]
#                  30.049                 9.942                 2.240
#    usair.pc$scores[, 3]
#                  -0.375
## ↳ SO2 = 30.049 + 9.942y[1] + 2.24y[2] - 0.375y[3]

# View summary information of linear model
summary(out)
## to see how accurate the model is.
## 【Result】
##  Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##  (Intercept)            30.049      2.907  10.336 1.85e-12 ***
##  usair.pc$scores[, 1]    9.942      1.962   5.068 1.14e-05 ***
##  usair.pc$scores[, 2]    2.240      2.374   0.943    0.352    
##  usair.pc$scores[, 3]   -0.375      2.462  -0.152    0.880  
### ↳ *** means the variable is significant to the model
### ↳ In other words, other variable (without ***) is no need to the model
### ↳ t value: to test the significance of the variable
#### if t value is large, the dimension is sagnificant.
#### if Pr(>|t|) is larger than 0.05 (5%) means the variable is not sagnificant
