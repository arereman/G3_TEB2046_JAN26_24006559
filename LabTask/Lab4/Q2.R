# assign values to V1
V1 <- c(2, 3, 1, 5, 4, 6, 8, 7, 9)

# create 3x3 Matrix_1 using values from V1
Matrix_1 <- matrix(V1, nrow = 3, ncol = 3)

# rename row and column on Matrix_1
rownames(Matrix_1) <- c("Row1", "Row2", "Row3")
colnames(Matrix_1) <- c("Col1", "Col2", "Col3")

# print Matrix_1 
cat("----- Matrix_1 -----\n")
print(Matrix_1)

# transpose Matrix_1 to create Matrix_2
Matrix_2 <- t(Matrix_1)

# print Matrix_2
cat("----- Matrix_2 -----\n")
print(Matrix_2)

# perform arithmatic operations
# addition
cat("\n----- Addition -----")
print(Matrix_1 + Matrix_2)

# subtraction
cat("\n----- Subtraction -----")
print(Matrix_1 - Matrix_2)

# multiplication
cat("\n----- Multiplication -----")
print(Matrix_1 * Matrix_2)

# division
cat("\n----- Division -----")
print(Matrix_1 / Matrix_2)