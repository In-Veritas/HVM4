// @{s} T{a,b,...}
// ---------------- ALO-NOD
// T{@{s}a, @{s}b, ...}
fn Term wnf_alo_nod(u64 ls_loc, u16 len, Term book) {
  u64 loc = term_val(book);
  u8  tag = term_tag(book);
  u16 ext = term_ext(book);
  u32 ari = term_arity(book);
  Term args[16];
  if (len == 0) {
    for (u32 i = 0; i < ari; i++) {
      args[i] = term_new(0, ALO, 0, loc + i);
    }
    return term_new_(tag, ext, ari, args);
  }
  for (u32 i = 0; i < ari; i++) {
    u64 alo_loc = heap_alloc(1);
    heap_set(alo_loc, ((ls_loc & ALO_LS_MASK) << ALO_TM_BITS) | ((loc + i) & ALO_TM_MASK));
    args[i] = term_new(0, ALO, len, alo_loc);
  }
  return term_new_(tag, ext, ari, args);
}
