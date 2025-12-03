// @@and(a, b) = a & b
fn Term prim_and(Term a, Term b) {
  return prim_op2(PRIM_AND, a, b);
}
