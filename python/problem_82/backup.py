import numpy as np

# Simple and easy dynamic programming solution.

N = 5

with open('small_matrix.txt') as file:
    mat = [[int(n) for n in line.rstrip().split(',')] for line in file]

mat = np.array(mat)

print(mat)

original = mat.copy()

# For each column from left-to-right (skipping the first column).
for j in range(1, N):
    # print(mat[:,j])
    # col = mat[:,j]
    # print(np.array_equal(col, mat[:,j]))
    col = np.zeros([N, 1])
    while (not np.array_equal(col, mat[:,j])):
        col = mat[:,j]
        mat[0, j] = original[0, j] + min(mat[0, j-1], mat[1, j])
        for i in range(1, N-1):
           mat[i, j] = original[i, j] + min(mat[i, j-1], mat[i-1, j], mat[i+1, j])
        mat[N-1, j] = original[N-1, j] + min(mat[0, j-1], mat[N-1, j])

print(mat)
