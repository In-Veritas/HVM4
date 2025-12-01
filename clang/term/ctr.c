fn Term term_ctr(u32 nam, u32 ari, Term *args) {
  return term_ctr_at(heap_alloc(ari), nam, ari, args);
}
