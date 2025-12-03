// @@opr'(x, &L{a,b})
// ------------------------- op1-sup
// ! X &L = x
// &L{@@opr'(X₀,a), @@opr'(X₁,b)}
fn Term wnf_op1_sup(u32 opr, Term x, Term sup) {
  ITRS++;
  u32  lab     = term_ext(sup);
  u32  sup_loc = term_val(sup);
  Copy X       = term_clone(lab, x);
  Term op0     = term_new_op1(opr, X.k0, HEAP[sup_loc + 0]);
  Term op1     = term_new_op1(opr, X.k1, HEAP[sup_loc + 1]);
  return term_new_sup(lab, op0, op1);
}
