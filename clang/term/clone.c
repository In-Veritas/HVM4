fn Copy term_clone(u32 lab, Term val) {
  u64 loc   = heap_alloc(1);
  HEAP[loc] = val;
  return term_clone_at(loc, lab);
}
