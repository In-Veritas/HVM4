fn Term term_new_alo(u32 ls_loc, u32 len, u32 tm_loc) {
  if (len == 0) {
    return term_new(0, ALO, 0, tm_loc);
  }
  u32 alo_loc = heap_alloc(1);
  heap_set(alo_loc, ((u64)ls_loc << 32) | tm_loc);
  return term_new(0, ALO, len, alo_loc);
}
