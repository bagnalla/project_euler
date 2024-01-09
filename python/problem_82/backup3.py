# Similar to the dynamic programming solution for problem 81 except we
# repeat the update step until reaching a fixed point.

from __future__ import annotations
from copy import deepcopy
from dataclasses import dataclass
from typing import Optional

with open('0082_matrix.txt') as file:
    data = [[int(n) for n in line.rstrip().split(',')] for line in file]

N: int = len(data)

# [print(row) for row in data]
# print('')

bignum = 100000000

@dataclass
class Cell:
    from_left:  int
    from_above: Optional[int]
    from_below: Optional[int]

    def min(self: Cell, include_above=True, include_below=True) -> int:
        return min(self.from_left,
                   self.from_above if include_above and self.from_above else bignum,
                   self.from_below if include_below and self.from_below else bignum)
    
    def __str__(self: Cell) -> str:
        return "(%d, %d, %d)" % (self.from_left,
                                 self.from_above or -1,
                                 self.from_below or -1)

old: list[list[Cell]] = [[Cell(from_left  = bignum,
                               from_above = None,
                               from_below = None) for j in range(N)]
                         for i in range(N)]

# Trivial initialization of first column.
for i in range(N):
    old[i][0] = Cell(from_left  = data[i][0],
                     from_above = data[i][0],
                     from_below = data[i][0])

def step(old: list[list[Cell]]) -> list[list[Cell]]:
    new = deepcopy(old)
    
    # Left to right.
    for j in range(1, N):
        for i in range(N):
            new[i][j].from_left = data[i][j] + new[i][j-1].min()

    # Top to bottom.
    for j in range(1, N):
        for i in range(1, N):
            new[i][j].from_above = data[i][j] + new[i-1][j].min(include_below=True)
    
    # Bottom to top
    for j in range(1, N):
        for i in range(N-2, -1, -1):
            new[i][j].from_below = data[i][j] + new[i+1][j].min(include_above=True)
    
    return new

new = step(old)

while (old != new):
    old = new
    new = step(old)

def print_solution(sol: list[list[Cell]]) -> None:
    for row in sol:
        for cell in row:
            print(cell, end=', ')
        print('')

# print_solution(old)
# print('')
# print_solution(new)

min_sum = bignum
for i in range(N):
    min_sum = min(min_sum, new[i][N-1].min())
print(min_sum)
