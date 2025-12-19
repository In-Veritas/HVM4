fn Term term_new_clo_at(u32 loc, u32 lab, Term val, Term bod) {
  return term_new_at(loc, CLO, lab, 2, (Term[]){val, bod});
}

fn Term term_new_clo(u32 lab, Term val, Term bod) {
  return term_new_clo_at(heap_alloc(2), lab, val, bod);
}
