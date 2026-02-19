// (&L{a0,a1} .|. b)
// -------------------------- OR-SUP
// ! B &L = b
// &L{(a0 .|. B₀), (a1 .|. B₁)}
fn Term wnf_or_sup(u64 or_loc, Term sup, Term b) {
  ITRS_INC("OR-SUP");
  u64  sup_loc = term_val(sup);
  u32  lab = term_ext(sup);
  Term a0  = heap_read(sup_loc + 0);
  Term a1  = heap_read(sup_loc + 1);
  Copy B   = term_clone(lab, b);
  Term r0 = term_new_or_at(or_loc, a0, B.k0);
  Term r1 = term_new_or(a1, B.k1);
  return term_new_sup_at(sup_loc, lab, r0, r1);
}
