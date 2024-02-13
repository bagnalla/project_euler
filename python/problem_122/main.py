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
# exponent value. If the resulting context is satisfiable then we're
# done, otherwise we have to undo the last assertion and add another
# variable and try again.

# Runs in about 30s in my Linux VM.

from dataclasses import dataclass
from typing import Optional
from yices import Config, Context, Model, Status, Types, Terms
Term = int # Make typechecker happy.

# Find m(k).
def solve(k: int) -> list[int]:
    if k == 1:
        return [1]

    cfg = Config()
    yices_ctx: Context = Context(cfg) # Yices context.
    env: dict[str, Term] = {}         # Map names to Yices terms.

    env["x0"] = Terms.new_uninterpreted_term(Types.int_type(), "x0")
    yices_ctx.assert_formula(Terms.eq(env["x0"], Terms.integer(1)))

    n = 1
    while True:
        name = "x%d" % n
        env[name] = Terms.new_uninterpreted_term(Types.int_type(), name)
        
        l = []
        for i in range(0, n):
            for j in range(i, n):
                l.append(Terms.eq(env["x%d" % n],
                                  Terms.add(env["x%d" % i], env["x%d" % j])))
        yices_ctx.assert_formula(Terms.yor(l))

        yices_ctx.push()
        yices_ctx.assert_formula(Terms.eq(env["x%d" % n], Terms.integer(k)))

        if yices_ctx.check_context() == Status.SAT:
            model = Model.from_context(yices_ctx, 1)
            return [model.get_integer_value(term) for term in env.values()]
        else:
            yices_ctx.pop()
            n += 1

sum = 0
for i in range(1, 201):
    sum += len(solve(i)) - 1
print(sum)
