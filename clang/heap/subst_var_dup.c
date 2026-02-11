fn void heap_subst_var_dup(u32 loc, Term val) {
  // Not a heap_set since we need release ordering
  __atomic_store_n(&HEAP[loc], term_sub_set(val, 1), __ATOMIC_RELEASE);
}
