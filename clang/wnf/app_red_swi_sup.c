// ((f ~> λ{n:z;s}) &L{a,b})
// ------------------------- app-red-swi-sup
// ! F &L = f
// ! Z &L = z
// ! S &L = s
// &L{((F₀ ~> λ{n:Z₀;S₀}) a)
//   ,((F₁ ~> λ{n:Z₁;S₁}) b)}
fn Term wnf_app_red_swi_sup(Term f, Term swi, Term sup) {
  ITRS++;
  u32  swi_loc = term_val(swi);
  u32  swi_num = term_ext(swi);
  u32  sup_loc = term_val(sup);
  u32  lab     = term_ext(sup);
  Term z       = HEAP[swi_loc + 0];
  Term s       = HEAP[swi_loc + 1];
  Term a       = HEAP[sup_loc + 0];
  Term b       = HEAP[sup_loc + 1];
  Copy F       = term_clone(lab, f);
  Copy Z       = term_clone(lab, z);
  Copy S       = term_clone(lab, s);
  Term swi0    = term_new_swi(swi_num, Z.k0, S.k0);
  Term swi1    = term_new_swi(swi_num, Z.k1, S.k1);
  Term r0      = term_new_app(term_new_red(F.k0, swi0), a);
  Term r1      = term_new_app(term_new_red(F.k1, swi1), b);
  return term_new_sup(lab, r0, r1);
}
