// @@sub(a, b) = a - b
fn Term prim_sub(Term a, Term b) {
  return prim_op2(PRIM_SUB, a, b);
}
