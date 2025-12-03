// @@lt(a, b) = a < b ? 1 : 0
fn Term prim_lt(Term a, Term b) {
  return prim_op2(PRIM_LT, a, b);
}
