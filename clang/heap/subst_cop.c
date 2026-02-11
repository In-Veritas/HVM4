fn Term heap_subst_cop(u8 side, u32 loc, Term r0, Term r1) {
  // Not a heap_set since we need release ordering
  __atomic_store_n(&HEAP[loc], term_sub_set(side == 0 ? r1 : r0, 1), __ATOMIC_RELEASE);
  return side == 0 ? r0 : r1;
}
