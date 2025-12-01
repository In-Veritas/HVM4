fn Term term_app_at(u32 loc, Term fun, Term arg) {
  return term_make_at(loc, APP, 0, 2, (Term[]){fun, arg});
}
