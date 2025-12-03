// (λ{n:f;g} &L{a,b})
// --------------------- swi-sup
// ! F &L = f
// ! G &L = g
// &L{(λ{n:F₀;G₀} a), (λ{n:F₁;G₁} b)}
fn Term wnf_app_swi_sup(Term swi, Term sup) {
  ITRS++;
  u32  num     = term_ext(swi);
  u32  swi_loc = term_val(swi);
  Term f       = HEAP[swi_loc + 0];
  Term g       = HEAP[swi_loc + 1];
  u32  lab     = term_ext(sup);
  u32  sup_loc = term_val(sup);
  Copy F       = term_clone(lab, f);
  Copy G       = term_clone(lab, g);
  Term swi0    = term_new_swi(num, F.k0, G.k0);
  Term swi1    = term_new_swi(num, F.k1, G.k1);
  Term app0    = term_new_app(swi0, HEAP[sup_loc + 0]);
  Term app1    = term_new_app(swi1, HEAP[sup_loc + 1]);
  return term_new_sup(lab, app0, app1);
}
