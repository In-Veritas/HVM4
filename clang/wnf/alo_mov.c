// @{s} % x = v; t
// ------------------ ALO-MOV
// x' ← fresh
// % x' = @{s} v
// @{x',s} t
fn Term wnf_alo_mov(u32 ls_loc, u32 len, u32 book_loc) {
  u64 mov_term_val = heap_alloc(1);
  u64 bind_ent     = heap_alloc(1);
  heap_set(bind_ent, ((u64)(u32)mov_term_val << 32) | ls_loc);
  u64 alo0 = heap_alloc(1);
  heap_set(alo0, ((u64)ls_loc << 32) | (book_loc + 0));
  heap_set(mov_term_val, term_new(0, ALO, len, alo0));
  u64 alo1 = heap_alloc(1);
  heap_set(alo1, ((u64)ls_loc << 32) | (book_loc + 0));
  u64 alo2 = heap_alloc(1);
  heap_set(alo2, ((u64)(u32)bind_ent << 32) | (book_loc + 1));
  return term_new_mov(term_new(0, ALO, len, alo1), term_new(0, ALO, len + 1, alo2));
}
