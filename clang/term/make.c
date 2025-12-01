fn Term term_make(u8 tag, u32 ext, u32 ari, Term *args) {
  return term_make_at(heap_alloc(ari), tag, ext, ari, args);
}
