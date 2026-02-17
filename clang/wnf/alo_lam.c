// @{s} λx.f
// ------------ ALO-LAM
// x' ← fresh
// λx'.@{x',s}f
fn Term wnf_alo_lam(u64 alo_loc, u64 ls_loc, u16 len, Term book) {
  u16 lam_ext  = term_ext(book);
  u64 lam_body = term_val(book);
  u64 bind_ent = heap_alloc(2);
  u64 t_loc    = (len > 0) ? alo_loc : heap_alloc(1);
  heap_set(t_loc, alo_pair_pack(bind_ent, lam_body));
  heap_set(bind_ent + 0, term_new(0, ALO, len + 1, t_loc));
  heap_set(bind_ent + 1, term_new_num(ls_loc));
  return term_new(0, LAM, lam_ext, bind_ent + 0);
}
