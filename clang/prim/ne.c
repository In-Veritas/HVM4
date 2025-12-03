// @@ne(a, b) = a != b ? 1 : 0
fn Term prim_ne(Term a, Term b) {
  return prim_op2(PRIM_NE, a, b);
}
