# Tests for rowMins_R(), rowMins_C()

X <- matrix(1:5, nrow = 5, ncol = 10)
print(X)

# Expected output added as comment after test

# TESTS FOR rowMins_R()
rowMins_R(X)
# Check: [1] 1 2 3 4 5 

rowMins_R(X, cols = 6:8)
# Check: [1] 26 27 28 29 30 

# Passing non matrix value as argument x
rowMins_R('string input', cols = 6:8)
# Check:
#Error in rowMins_R("string input", cols = 6:8) : 
#  Argument x must be a matrix

# Passing non vector value as argument cols 
rowMins_R(X, cols = matrix(1:20, nrow = 4, ncol = 5))
# Check:
#Error in rowMins_R(X, cols = matrix(1:20, nrow = 4, ncol = 5)) : 
#  Argument cols must be a vector

# Handling out of range subscript
rowMins_R(X, cols = 'c')
# Check:
#Error in rowMins_R(X, cols = "c") : Index out of bounds

# Warning for non numeric matrix values
charMatrix = matrix(1:4, nrow =2, ncol = 2)
charMatrix[1,1] = '1'
print(charMatrix)

rowMins_R(charMatrix, cols = 1:2)
# Check:
#[1] "1" "2"
#Warning message:
#  In rowMins_R(charMatrix, cols = 1:2) : Non numeric values present in x



# TESTS FOR rowMins_C()
rowMins_C(X)
# Check: [1] 1 2 3 4 5 

rowMins_C(X, cols = 6:8)
# Check: [1] 26 27 28 29 30 

# Passing non matrix value as argument x
rowMins_C('string input', cols = 6:8)
# Check:
#Error in rowMins_C("string input", cols = 6:8) : 
#  Argument x must be a matrix

# Passing non vector value as argument cols 
rowMins_C(X, cols = matrix(1:20, nrow = 4, ncol = 5))
# Check:
#Error in rowMins_C(X, cols = matrix(1:20, nrow = 4, ncol = 5)) : 
#  Argument cols must be a vector

# Handling out of range subscript
rowMins_C(X, cols = 'c')
# Check:
#Error in rowMins_C(X, cols = "c") : Index out of bounds

# For non numeric matrix values gives numeric result
# if coerce is successful else gives NA
print(charMatrix)

rowMins_C(charMatrix, cols = 1:2)
# Check:
#[1] 1 2

charMatrix[2,1] = 'string'
rowMins_C(charMatrix, cols = 1:2)
# Check:
#[1]  1 NA
#Warning message:
#  In rowMins_C(charMatrix, cols = 1:2) : NAs introduced by coercion



# TESTING ROW MINS FOR COLUMN GROUPS
X <- matrix(1:50, nrow = 5, ncol = 10)
X
factors <- as.factor(c(3,1,1,2,2,1,1,2,3,1))
groups <- split(seq(along=factors), factors)
Y <- sapply(groups, FUN=function(idxs) rowMins_R(X, cols=idxs))
for(i in 1:3){
  cat('\nColumns in consideration\n')
  print(X[, groups[[i]]])
  print('Corresponding rowMins_R')
  print(Y[,i])
}
Z <- sapply(groups, FUN=function(idxs) rowMins_C(X, cols=idxs))
for(i in 1:3){
  cat('\nColumns in consideration\n')
  print(X[, groups[[i]]])
  print('Corresponding rowMins_C')
  print(Z[,i])
}

# Time comparison for large matrices

# Matrix of size 10^4 * 10^4 of ~750Mb, here is the time comparison:
#[1] "Matrix size: 10000 * 10000"
#[1] "rowMins_R:"
#user  system elapsed 
#116.891   0.000 115.666 
#[1] "rowMins_C:"
#user  system elapsed 
#0.000   0.000   0.456 

# increase range of i with caution, creates very large matrix i=4 onwards
# rowMins_R() will consume lot of time from i=4 onwards
print('Performance comparison of rowMins_R() and rowMins_C()')
for (i in 1:3){
  print(paste('Matrix size:',10^i,'*',10^i,sep=' '))
  B = matrix(rnorm(10^(2*i)), ncol = 10^i, nrow = 10^i)
  print("rowMins_R:")
  ptm = proc.time()
  Temp = rowMins_R(B)
  print(proc.time() - ptm)
  print("rowMins_C:")
  ptm = proc.time()
  Temp = rowMins_C(B)
  print(proc.time() - ptm)
}