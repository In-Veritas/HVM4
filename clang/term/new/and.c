fn Term term_new_and_at(u64 loc, Term a, Term b) {
  heap_set(loc + 0, a);
  heap_set(loc + 1, b);
  return term_new(0, AND, 0, loc);
}

fn Term term_new_and(Term a, Term b) {
  return term_new_and_at(heap_alloc(2), a, b);
}
