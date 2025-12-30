fn Term term_new_mov_at(u32 loc, Term val, Term bod) {
  return term_new_at(loc, MOV, 0, 2, (Term[]){val, bod});
}

fn Term term_new_mov(Term val, Term bod) {
  return term_new_mov_at(heap_alloc(2), val, bod);
}
