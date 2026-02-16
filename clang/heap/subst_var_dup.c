fn void heap_subst_var_dup(u64 loc, Term val) {
  heap_set_rel(loc, term_sub_set(val, 1));
}
