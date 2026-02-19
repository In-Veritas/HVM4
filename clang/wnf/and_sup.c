// (&L{a0,a1} .&. b)
// -------------------------- AND-SUP
// ! B &L = b
// &L{(a0 .&. B₀), (a1 .&. B₁)}
fn Term wnf_and_sup(u64 and_loc, Term sup, Term b) {
  ITRS_INC("AND-SUP");
  u32  sup_lab = term_ext(sup);
  u64  sup_loc = term_val(sup);
  Copy  B = term_clone(sup_lab, b);
  Term a0 = heap_read(sup_loc + 0);
  Term a1 = heap_read(sup_loc + 1);
  Term r0 = term_new_and_at(and_loc, a0, B.k0);
  Term r1 = term_new_and(a1, B.k1);
  return term_new_sup_at(sup_loc, sup_lab, r0, r1);
}
