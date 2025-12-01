fn Term term_dup_at(u32 loc, u32 lab, Term val, Term bod) {
  return term_make_at(loc, DUP, lab, 2, (Term[]){val, bod});
}
