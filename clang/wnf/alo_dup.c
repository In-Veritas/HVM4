// @{s} ! x &L = v; t
// ------------------ ALO-DUP
// x' ← fresh
// ! x' &L = @{s} v
// @{x',s} t
fn Term wnf_alo_dup(u32 ls_loc, u32 len, u32 book_loc, u32 lab) {
  u64 bind_ent = heap_alloc(2);
  Term alo_v;
  if (len == 0) {
    alo_v = term_new(0, ALO, 0, book_loc + 0);
  } else {
    u64 alo0 = heap_alloc(1);
    heap_set(alo0, ((u64)ls_loc << 32) | (book_loc + 0));
    alo_v = term_new(0, ALO, len, alo0);
  }
  heap_set(bind_ent + 0, alo_v);
  heap_set(bind_ent + 1, term_new_num(ls_loc));
  u64 alo2 = heap_alloc(1);
  heap_set(alo2, ((u64)(u32)bind_ent << 32) | (book_loc + 1));
  return term_new_dup(lab, alo_v, term_new(0, ALO, len + 1, alo2));
}
