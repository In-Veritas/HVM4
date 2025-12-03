// @@lsh(a, b) = a << (b & 31)
fn Term prim_lsh(Term a, Term b) {
  return prim_op2(PRIM_LSH, a, b);
}
