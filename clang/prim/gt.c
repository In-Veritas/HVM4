// @@gt(a, b) = a > b ? 1 : 0
fn Term prim_gt(Term a, Term b) {
  return prim_op2(PRIM_GT, a, b);
}
