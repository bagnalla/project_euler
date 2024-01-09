# Similar to the dynamic programming solution for problem 81 except we
# repeat the update step until reaching a fixed point.

from copy import deepcopy

with open('0082_matrix.txt') as file:
    data = [[int(n) for n in line.rstrip().split(',')] for line in file]

N: int = len(data)

bignum = 100000000

# Initial approximation of the solution matrix.
old: list[list[int]] = [[bignum]*N for _ in range(N)]

# Initialize the first column.
for i in range(N):
    old[i][0] = data[i][0]

# Update step.
def step(old: list[list[int]]) -> list[list[int]]:
    new = deepcopy(old)
    
    for j in range(1, N):
        new[0][j] = data[0][j] + min(new[0][j-1], new[1][j])
        for i in range(1, N-1):
            new[i][j] = data[i][j] + min(new[i][j-1], new[i-1][j], new[i+1][j])
        new[N-1][j] = data[N-1][j] + min(new[N-1][j-1], new[N-2][j])
    
    return new

# Iterate update step to fixed point.
new = step(old)
while (old != new):
    old = new
    new = step(old)

# The answer is the minimum value from the rightmost column in the
# fixed point matrix.
min_sum = bignum
for i in range(N):
    min_sum = min(min_sum, new[i][N-1])
print(min_sum)
