// @@or(a, b) = a | b
fn Term prim_or(Term a, Term b) {
  return prim_op2(PRIM_OR, a, b);
}
