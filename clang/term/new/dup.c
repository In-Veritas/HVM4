fn Term term_new_dup_at(u64 loc, u16 lab, Term val, Term bod) {
  return term_new_at(loc, DUP, lab, 2, (Term[]){val, bod});
}

fn Term term_new_dup(u16 lab, Term val, Term bod) {
  return term_new_dup_at(heap_alloc(2), lab, val, bod);
}
