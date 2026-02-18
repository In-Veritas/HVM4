// @{s} λx.f
// ------------ ALO-LAM
// x' ← fresh
// λx'.@{x',s}f
fn Term wnf_alo_lam(u32 ls_loc, u32 len, Term book) {
  u32 lam_ext  = term_ext(book);
  u32 lam_body = term_val(book);
  u64 bind_ent = heap_alloc(2);
  heap_set(bind_ent + 0, term_new_alo((u32)bind_ent, len + 1, lam_body));
  heap_set(bind_ent + 1, term_new_num(ls_loc));
  return term_new(0, LAM, lam_ext, bind_ent + 0);
}
