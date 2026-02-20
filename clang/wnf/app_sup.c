// (&L{f,g} a)
// ----------------- APP-SUP
// ! A &L = a
// &L{(f A₀),(g A₁)}
fn Term wnf_app_sup(u64 app_loc, Term sup, Term arg) {
  ITRS_INC("APP-SUP");
  u64  sup_loc = term_val(sup);
  u32  lab     = term_ext(sup);
  Term tm1     = heap_read(sup_loc + 1);
  Copy D = term_clone(lab, arg);
  heap_set(sup_loc + 1, D.k0);
  Term ap0 = term_new(0, APP, 0, sup_loc);
  Term ap1 = term_new_app(tm1, D.k1);
  return term_new_sup_at(app_loc, lab, ap0, ap1);
}
