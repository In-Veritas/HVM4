// @{s} ! x &L = v; t
// ------------------ ALO-DUP
// x' ← fresh
// ! x' &L = @{s} v
// @{x',s} t
fn Term wnf_alo_dup(u32 alo_loc, u32 ls_loc, u32 len, Term book) {
  u32 book_loc = term_val(book);
  u64 bind_ent = heap_alloc(2);
  Term alo_v = term_new_alo(ls_loc, len, book_loc + 0);
  heap_set(bind_ent + 0, alo_v);
  heap_set(bind_ent + 1, term_new_num(ls_loc));
  u32 t_loc = (len > 0) ? alo_loc : heap_alloc(1);
  heap_set(t_loc, ((u64)bind_ent << 32) | (book_loc + 1));
  return term_new(0, ALO, len + 1, t_loc);
}
