fn Term term_mat_at(u32 loc, u32 nam, Term val, Term nxt) {
  return term_make_at(loc, MAT, nam, 2, (Term[]){val, nxt});
}
