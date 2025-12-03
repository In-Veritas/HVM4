// @@rsh(a, b) = a >> (b & 31)
fn Term prim_rsh(Term a, Term b) {
  return prim_op2(PRIM_RSH, a, b);
}
