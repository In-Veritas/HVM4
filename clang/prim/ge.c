// @@ge(a, b) = a >= b ? 1 : 0
fn Term prim_ge(Term a, Term b) {
  return prim_op2(PRIM_GE, a, b);
}
