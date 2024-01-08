# Simple and easy dynamic programming solution.

with open('0081_matrix.txt') as file:
    mat = [[int(n) for n in line.rstrip().split(',')] for line in file]

# First row.
for j in range(1, 80):
    mat[0][j] += mat[0][j-1]

# First column.
for i in range(1, 80):
    mat[i][0] += mat[i-1][0]

# Rest of rows and columns.
for i in range(1, 80):
    for j in range(1, 80):
        mat[i][j] += min(mat[i][j-1], mat[i-1][j])

# Final answer is in the bottom right cell.
print(mat[79][79])
