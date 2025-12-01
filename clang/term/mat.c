fn Term term_mat(u32 nam, Term val, Term nxt) {
  return term_mat_at(heap_alloc(2), nam, val, nxt);
}
