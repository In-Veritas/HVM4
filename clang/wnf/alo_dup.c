// @{s} ! x &L = v; t
// ------------------ ALO-DUP
// x' ← fresh
// ! x' &L = @{s} v
// @{x',s} t
fn Term wnf_alo_dup(u64 alo_loc, u64 ls_loc, u16 len, Term book) {
  u64 book_loc = term_val(book);
  u64 bind_ent = heap_alloc(2);
  Term alo_v = term_new_alo_at(alo_loc, ls_loc, len, book_loc + 0);
  heap_set(bind_ent + 0, alo_v);
  heap_set(bind_ent + 1, term_new(0, NUM, 0, ls_loc));
  return term_new_alo(bind_ent, len + 1, book_loc + 1);
}