fn Term term_sup_at(u32 loc, u32 lab, Term tm0, Term tm1) {
  return term_make_at(loc, SUP, lab, 2, (Term[]){tm0, tm1});
}
