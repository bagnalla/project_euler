# Iterate to a fixed point solution matrix.

from copy import deepcopy

with open('0083_matrix.txt') as file:
    data = [[int(n) for n in line.rstrip().split(',')] for line in file]

N: int = len(data)

# Initial approximation of the solution matrix.
old: list[list[int]] = [[100000000]*N for _ in range(N)]

# Initialize the top left cell.
old[0][0] = data[0][0]

# Update step.
def step(old: list[list[int]]) -> list[list[int]]:
    new = deepcopy(old)

    for i in range(N):
        for j in range(N):
            if (i, j) == (0, 0):
                continue
            new[i][j] = data[i][j] + min(*(([new[i-1][j]] if i > 0 else []) +
                                           ([new[i+1][j]] if i < N-1 else []) +
                                           ([new[i][j-1]] if j > 0 else []) +
                                           ([new[i][j+1]] if j < N-1 else [])))
            
    return new

# Iterate update step to fixed point.
new = step(old)
while (old != new):
    old = new
    new = step(old)

print(new[N-1][N-1])
