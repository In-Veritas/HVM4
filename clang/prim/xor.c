// @@xor(a, b) = a ^ b
fn Term prim_xor(Term a, Term b) {
  return prim_op2(PRIM_XOR, a, b);
}
