// ! X &(#n) = v; b
// ---------------- DDU-NUM
// ! X &n = v
// b(X₀, X₁)
fn Term wnf_ddu_num(Term lab_num, Term val, Term bod) {
  ITRS_INC("DDU-NUM");
  u32 lab   = term_val(lab_num);
  Copy V    = term_clone(lab, val);
  Term app0 = term_new_app(bod, V.k0);
  Term app1 = term_new_app(app0, V.k1);
  return app1;
}
