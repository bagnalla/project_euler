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

from yices import Config, Context, Model, Status, Types, Terms
Term = int # Make typechecker happy.

# Solve for the powers of n appearing in a minimal (not necessarily
# unique) calculation of n^k. m(k) = len(solve(k))-1.
def solve(k: int) -> list[int]:
    if k == 1:
        return [1]

    cfg = Config()
    yices_ctx: Context = Context(cfg) # Yices context.
    env: dict[str, Term] = {}         # Map names to Yices terms.

    env["x0"] = Terms.new_uninterpreted_term(Types.int_type(), "x0")
    yices_ctx.assert_formula(Terms.eq(env["x0"], Terms.integer(1)))

    i = 1
    while True:
        name = "x%d" % i
        env[name] = Terms.new_uninterpreted_term(Types.int_type(), name)
        
        l = []
        for a in range(0, i):
            for b in range(a, i):
                l.append(Terms.eq(env["x%d" % i],
                                  Terms.add(env["x%d" % a], env["x%d" % b])))
        yices_ctx.assert_formula(Terms.yor(l))

        yices_ctx.push()
        yices_ctx.assert_formula(Terms.eq(env["x%d" % i], Terms.integer(k)))

        if yices_ctx.check_context() == Status.SAT:
            model = Model.from_context(yices_ctx, 1)
            return [model.get_integer_value(term) for term in env.values()]
        else:
            yices_ctx.pop()
            i += 1

print(sum(len(solve(k)) - 1 for k in range(1, 201)))
