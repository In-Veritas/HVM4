// @@le(a, b) = a <= b ? 1 : 0
fn Term prim_le(Term a, Term b) {
  return prim_op2(PRIM_LE, a, b);
}
