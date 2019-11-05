# Create a new matrix with value 1 to 25 splited into 5 rows
a <- matrix(1:25, nrow = 5)
a

# Create a new matrix with value 26 to 50 splited into 5 rows
b <- matrix(26:50, nrow = 5)
b

##

dim(a)
dim(b)

# Summation of matrices
a + b

# Difference of matrices
a - b

# ※Multiplication of *each corresponding cell* in matrices
a * b

# ※Multiplication of matrices
a %*% b

# Inverse matrix
solve(a) # this will produce error, because matrix is singular
## singular matrix <-> matrix determinant is 0 <-> matrix does not have its inverse

# Create a new matrix
cv <- matrix(runif(25), nrow = 5)
cv
# inverse the matrix
solve(cv)

# Proofing inverse matrix
cv1 <- solve(cv)
cv %*% cv1 # get a identity matrix
