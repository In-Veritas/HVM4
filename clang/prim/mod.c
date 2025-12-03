// @@mod(a, b) = a % b (returns 0 on div by zero)
fn Term prim_mod(Term a, Term b) {
  return prim_op2(PRIM_MOD, a, b);
}
