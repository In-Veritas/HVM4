fn Term term_lam(Term bod) {
  return term_lam_at(heap_alloc(1), bod);
}
