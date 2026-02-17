// @{s} ! x &L = v; t
// ------------------ ALO-DUP
// x' ← fresh
// ! x' &L = @{s} v
// @{x',s} t
fn Term wnf_alo_dup(u64 alo_loc, u64 ls_loc, u16 len, Term book) {
  u64 book_loc = term_val(book);
  u64 bind_ent = heap_alloc(2);
  Term alo_v;
  if (len == 0) {
    alo_v = term_new(0, ALO, 0, book_loc + 0);
  } else {
    u64 alo0 = heap_alloc(1);
    heap_set(alo0, alo_pair_pack(ls_loc, book_loc + 0));
    alo_v = term_new(0, ALO, len, alo0);
  }
  heap_set(bind_ent + 0, alo_v);
  heap_set(bind_ent + 1, term_new_num(ls_loc));
  u64 t_loc = (len > 0) ? alo_loc : heap_alloc(1);
  heap_set(t_loc, alo_pair_pack(bind_ent, book_loc + 1));
  return term_new(0, ALO, len + 1, t_loc);
}
