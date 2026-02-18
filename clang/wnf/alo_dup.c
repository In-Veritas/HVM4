// @{s} ! x &L = v; t
// ------------------ ALO-DUP
// x' ← fresh
// ! x' &L = @{s} v
// @{x',s} t
fn Term wnf_alo_dup(u32 ls_loc, u32 len, Term book) {
  u32 book_loc = term_val(book);
  u32 lab      = term_ext(book);
  u64 bind_ent = heap_alloc(2);
  Term alo_v = term_new_alo(ls_loc, len, book_loc + 0);
  heap_set(bind_ent + 0, alo_v);
  heap_set(bind_ent + 1, term_new_num(ls_loc));
  return term_new_dup(lab, alo_v, term_new_alo((u32)bind_ent, len + 1, book_loc + 1));
}
