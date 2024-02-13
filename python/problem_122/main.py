# I first tried solving this via BFS in OCaml but it was taking too
# much time/memory, so I decided to try using an SMT solver and that
# worked! To encode the problem as an SMT instance do the following:

# 1) Choose a guess for the number of multiplications (start at 1 and
# if that doesn't work try 2 and so on until it succeeds).

# 2) Create a vector of integer variables x₁, x₂, ..., one for each
# power obtained via the multiplications.

# 3) Assert:
#      x₀ = 1
#      x₁ = 2
#      x₂ = x₀ + x₀ ∨ x₂ = x₀ + x₁ ∨ x₂ = x₁ + x₁
#      x₃ = x₀ + x₀ ∨ x₃ = x₀ + x₁ ∨ x₃ = x₀ + x₂
#                   ∨ x₃ = x₁ + x₁ ∨ x₃ = x₁ + x₂ ∨ x₃ = x₂ + x₂
#      etc.

# In other words, each variable xᵢ should be equal to the sum of two
# variables xⱼ and xₖ for j < i and k < i.

# And finally, assert that the last variable is equal to the target
# exponent value.

# Runs in about 30s in my Linux VM.

from dataclasses import dataclass
from typing import Optional
from yices import Config, Context, Model, Status, Types, Terms
Term = int # Make typechecker happy.

# Try to solve for the number of ways to raise a base to the [target]
# power using [n] multiplications.
def try_to_solve(target: int, n: int) -> Optional[list[int]]:
    if n < 1:
        return None
    
    yices_ctx: Context = Context(Config()) # Yices context.
    env: dict[str, Term] = {}              # Map names to Yices terms.

    for i in range(n):
        name = "x%d" % i
        env[name] = Terms.new_uninterpreted_term(Types.int_type(), name)

    yices_ctx.assert_formula(Terms.eq(env["x0"], Terms.integer(1)))
    if n > 1: # Small optimization.
        yices_ctx.assert_formula(Terms.eq(env["x1"], Terms.integer(2)))

    for i in range(2, n):
        l = []
        for j in range(0, i):
            for k in range(j, i):
                l.append(Terms.eq(env["x%d" % i],
                                  Terms.add(env["x%d" % j], env["x%d" % k])))
        yices_ctx.assert_formula(Terms.yor(l))

    yices_ctx.assert_formula(Terms.eq(env["x%d" % (n-1)], Terms.integer(target)))

    if yices_ctx.check_context() == Status.SAT:
        model = Model.from_context(yices_ctx, 1)
        return [model.get_integer_value(term) for term in env.values()]
    else:
        return None

# Keep trying to solve with increasing values of n until succeeding.
def solve(target: int) -> list[int]:
    n = 1
    l = try_to_solve(target, n)
    while l is None:
        n += 1
        l = try_to_solve(target, n)
    return l

sum = 0
for i in range(1, 201):
    sum += len(solve(i)) - 1
print(sum)
