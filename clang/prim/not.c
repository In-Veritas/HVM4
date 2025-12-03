// @@not(a) = ~a
fn Term prim_not(Term a) {
  return prim_op1(PRIM_NOT, a);
}
