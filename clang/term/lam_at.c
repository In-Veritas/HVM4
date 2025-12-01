fn Term term_lam_at(u32 loc, Term bod) {
  return term_make_at(loc, LAM, 0, 1, (Term[]){bod});
}
