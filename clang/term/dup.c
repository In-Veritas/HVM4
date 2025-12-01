fn Term term_dup(u32 lab, Term val, Term bod) {
  return term_dup_at(heap_alloc(2), lab, val, bod);
}
