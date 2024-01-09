from __future__ import annotations
from dataclasses import dataclass
from typing import Optional

N = 5

with open('small_matrix.txt') as file:
    data = [[int(n) for n in line.rstrip().split(',')] for line in file]

# # For each column from left-to-right (skipping the first column).
# for j in range(1, N):
#     # print(mat[:,j])
#     # col = mat[:,j]
#     # print(np.array_equal(col, mat[:,j]))
#     col = np.zeros([N, 1])
#     while (not np.array_equal(col, mat[:,j])):
#         col = mat[:,j]
#         mat[0, j] = original[0, j] + min(mat[0, j-1], mat[1, j])
#         for i in range(1, N-1):
#            mat[i, j] = original[i, j] + min(mat[i, j-1], mat[i-1, j], mat[i+1, j])
#         mat[N-1, j] = original[N-1, j] + min(mat[0, j-1], mat[N-1, j])

@dataclass
class Cell:
    from_left:  int
    from_above: Optional[int]
    from_below: Optional[int]
    def __str__(self: Cell) -> str:
        return "(%d, %d, %d)" % (self.from_left,
                                 self.from_above or -1,
                                 self.from_below or -1)

bignum = 1000000

# TODO: need to change the algorithm at the top level so it proceeds
# one column at a time and iterates each one to a fixed point (rather
# than iterating on the whole matrix).

def initial_approximation(mat: list[list[int]]) -> list[list[Cell]]:
    out: list[list[Cell]] = [[Cell(from_left  = bignum,
                                   from_above = None,
                                   from_below = None) for j in range(N)] for i in range(N)]

    # Trivial initialization of first column.
    for i in range(N):
        out[i][0] = Cell(from_left  = mat[i][0],
                         from_above = mat[i][0],
                         from_below = mat[i][0])

    # # Compute initial values for the rest of the columns.
    # for j in range(1, N):
    #     out[0][j] = Cell(from_left  = mat[0][j] + mat[0][j-1],
    #                      from_above = None,
    #                      from_below = mat[0][j] + mat[1][j])
    #     for i in range(1, N-1):
    #         out[i][j] = Cell(from_left  = mat[i][j] + mat[i][j-1],
    #                          from_above = mat[i][j] + mat[i-1][j],
    #                          from_below = mat[i][j] + mat[i+1][j])
    #     out[N-1][j] = Cell(from_left  = mat[N-1][j] + mat[N-1][j-1],
    #                        from_above = mat[N-1][j] + mat[N-2][j],
    #                        from_below = None)

    # # Compute initial values for the rest of the columns.
    # for j in range(1, N):
    #     out[0][j] = Cell(from_left  = out[0][j] + out[0][j-1],
    #                      from_above = None,
    #                      from_below = out[0][j] + out[1][j])
    #     for i in range(1, N-1):
    #         out[i][j] = Cell(from_left  = out[i][j] + out[i][j-1],
    #                          from_above = out[i][j] + out[i-1][j],
    #                          from_below = out[i][j] + out[i+1][j])
    #     out[N-1][j] = Cell(from_left  = out[N-1][j] + out[N-1][j-1],
    #                        from_above = out[N-1][j] + out[N-2][j],
    #                        from_below = None)
    
    for j in range(1, N):
        out[0][j] = Cell(from_left = mat[0][j] + min(out[0][j-1].from_left,
                                                     out[0][j-1].from_above or bignum,
                                                     out[0][j-1].from_below or bignum),
                         from_above = None,
                         from_below = mat[0][j] + min(out[1][j].from_left,
                                                      out[1][j].from_below or bignum))
        for i in range(N-1):
            out[i][j] = Cell(from_left = mat[i][j] + min(out[i][j-1].from_left,
                                                         out[i][j-1].from_above or bignum,
                                                         out[i][j-1].from_below or bignum),
                             from_above = mat[i][j] + min(out[i-1][j].from_left,
                                                          out[i-1][j].from_above or bignum),
                             from_below = mat[i][j] + min(out[i+1][j].from_left,
                                                          out[i+1][j].from_below or bignum))
        out[N-1][j] = Cell(from_left = mat[N-1][j] + min(out[N-1][j-1].from_left,
                                                         out[N-1][j-1].from_above or bignum,
                                                         out[N-1][j-1].from_below or bignum),
                           from_above = mat[N-1][j] + min(out[N-2][j].from_left,
                                                          out[N-2][j].from_above or bignum),
                           from_below = None)
        
    return out

# def step(mat: list[list[int]], sol: list[list[Cell]]) -> list[list[Cell]]:
#     out = sol.copy()
    
#     # For all columns (skipping the first).
#     for j in range(1, N):
#         out[0][j] = Cell(from_left = mat[0][j] + min(out[0][j-1].from_left,
#                                                      out[0][j-1].from_above or bignum,
#                                                      out[0][j-1].from_below or bignum),
#                          from_above = None,
#                          from_below = mat[0][j] + min(out[1][j].from_left,
#                                                       out[1][j].from_below or bignum))
#         for i in range(N-1):
#             out[i][j] = Cell(from_left = mat[i][j] + min(out[i][j-1].from_left,
#                                                          out[i][j-1].from_above or bignum,
#                                                          out[i][j-1].from_below or bignum),
#                              from_above = mat[i][j] + min(out[i-1][j].from_left,
#                                                           out[i-1][j].from_above or bignum),
#                              from_below = mat[i][j] + min(out[i+1][j].from_left,
#                                                           out[i+1][j].from_below or bignum))
        
#         out[N-1][j] = Cell(from_left = mat[N-1][j] + min(out[N-1][j-1].from_left,
#                                                          out[N-1][j-1].from_above or bignum,
#                                                          out[N-1][j-1].from_below or bignum),
#                            from_above = mat[N-1][j] + min(out[N-2][j].from_left,
#                                                           out[N-2][j].from_above or bignum),
#                            from_below = None)
#     return out

# def step(mat: list[list[int]], sol: list[list[Cell]]) -> list[list[Cell]]:
#     out = sol.copy()
    
#     # For all columns (skipping the first one).
#     for j in range(1, N):
#         out[0][j] = Cell(from_left = mat[0][j] + min(sol[0][j-1].from_left,
#                                                      sol[0][j-1].from_above or bignum,
#                                                      sol[0][j-1].from_below or bignum),
#                          from_above = None,
#                          from_below = mat[0][j] + min(sol[1][j].from_left,
#                                                       sol[1][j].from_below or bignum))
#         for i in range(N-1):
#             out[i][j] = Cell(from_left = mat[i][j] + min(sol[i][j-1].from_left,
#                                                          sol[i][j-1].from_above or bignum,
#                                                          sol[i][j-1].from_below or bignum),
#                              from_above = mat[i][j] + min(sol[i-1][j].from_left,
#                                                           sol[i-1][j].from_above or bignum),
#                              from_below = mat[i][j] + min(sol[i+1][j].from_left,
#                                                           sol[i+1][j].from_below or bignum))
        
#         out[N-1][j] = Cell(from_left = mat[N-1][j] + min(sol[N-1][j-1].from_left,
#                                                          sol[N-1][j-1].from_above or bignum,
#                                                          sol[N-1][j-1].from_below or bignum),
#                            from_above = mat[N-1][j] + min(sol[N-2][j].from_left,
#                                                           sol[N-2][j].from_above or bignum),
#                            from_below = None)
#     return out

# def solve(mat: list[list[int]], init: list[list[Cell]]) -> list[list[Cell]]:
#     prev = init
#     new = step(mat, init)
#     while (prev != new):
#         print(new)
#         prev = new
#         new = step(mat, prev)
#     return new

# print(data)

x = initial_approximation(data)

print([[str(cell) for cell in row] for row in x])

# y = solve(data, x)

# print([[str(cell) for cell in row] for row in y])

# for i in range(N):
#     print(y[i][N-1])
