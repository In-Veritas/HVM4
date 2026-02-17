// @{s} λx.f
// ------------ ALO-LAM
// x' ← fresh
// λx'.@{x',s}f
fn Term wnf_alo_lam(u32 ls_loc, u32 len, u32 lam_ext, u32 book_body_loc) {
  u64 bind_ent = heap_alloc(2);
  heap_set(bind_ent + 1, term_new_num(ls_loc));
  u64 alo_loc = heap_alloc(1);
  heap_set(alo_loc, ((u64)(u32)bind_ent << 32) | book_body_loc);
  heap_set(bind_ent + 0, term_new(0, ALO, len + 1, alo_loc));
  return term_new(0, LAM, lam_ext, bind_ent + 0);
}
