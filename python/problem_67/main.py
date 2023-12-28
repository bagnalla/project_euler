# Bottom-up dynamic programming solution. 

with open('triangle.txt') as file:
    lines = [[int(n) for n in line.rstrip().split()] for line in file]

for i in range(len(lines) - 2, -1, -1):
    for j in range(len(lines[i])):
        lines[i][j] += max(lines[i+1][j], lines[i+1][j+1])

print(lines[0][0])
