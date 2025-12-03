// @@add(a, b) = a + b
fn Term prim_add(Term a, Term b) {
  return prim_op2(PRIM_ADD, a, b);
}
