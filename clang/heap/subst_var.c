fn void heap_subst_var(u64 loc, Term val) {
  heap_set(loc, term_sub_set(val, 1));
}
