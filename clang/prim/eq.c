// @@eq(a, b) = a == b ? 1 : 0
fn Term prim_eq(Term a, Term b) {
  return prim_op2(PRIM_EQ, a, b);
}
