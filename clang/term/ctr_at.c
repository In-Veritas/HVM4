fn Term term_ctr_at(u32 loc, u32 nam, u32 ari, Term *args) {
  return term_make_at(loc, C00 + ari, nam, ari, args);
}
