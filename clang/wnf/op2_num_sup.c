// (x op &L{a,b}) where x is NUM
// ------------------------- OP2-NUM-SUP
// ! X &L = x
// &L{(X₀ op a), (X₁ op b)}
fn Term wnf_op2_num_sup(u32 opr, Term x, Term sup) {
  ITRS_INC("OP2-NUM-SUP");
  u32  lab     = term_ext(sup);
  u64  sup_loc = term_val(sup);
  Term op0     = term_new_op2(opr, x, heap_read(sup_loc + 0));
  Term op1     = term_new_op2(opr, x, heap_read(sup_loc + 1));
  return term_new_sup_at(sup_loc, lab, op0, op1);
}
