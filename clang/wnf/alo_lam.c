// @{s} λx.f
// ------------ ALO-LAM
// x' ← fresh
// λx'.@{x',s}f
fn Term wnf_alo_lam(u64 alo_loc, u64 ls_loc, u16 len, Term book) {
  u16 lam_ext  = term_ext(book);
  u64 lam_body = term_val(book);
  u64 bind_ent = heap_alloc(2);
  u64 t_loc    = (len > 0) ? alo_loc : heap_alloc(1);
  heap_set(t_loc, ((bind_ent & ALO_LS_MASK) << ALO_TM_BITS) | (lam_body & ALO_TM_MASK));
  heap_set(bind_ent + 0, term_new(0, ALO, len + 1, t_loc));
  // Keep full heap location width; term_new_num(u32) would truncate on T>1 slices.
  heap_set(bind_ent + 1, term_new(0, NUM, 0, ls_loc));
  return term_new(0, LAM, lam_ext, bind_ent + 0);
}
