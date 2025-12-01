fn Term term_app(Term fun, Term arg) {
  return term_app_at(heap_alloc(2), fun, arg);
}
