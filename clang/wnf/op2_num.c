// @@opr(#n, y)
// ------------ op2-num
// @@opr'(#n, y)
fn Term wnf_op2_num(u32 opr, Term x, Term y) {
  ITRS++;
  return term_new_op1(opr, x, y);
}
