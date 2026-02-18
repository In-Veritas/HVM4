// @{s} T{a,b,...}
// ---------------- ALO-NOD
// T{@{s}a, @{s}b, ...}
fn Term wnf_alo_nod(u32 ls_loc, u32 len, Term book) {
  u32 loc = term_val(book);
  u8  tag = term_tag(book);
  u32 ext = term_ext(book);
  u32 ari = term_arity(book);
  Term args[16];
  for (u32 i = 0; i < ari; i++) {
    args[i] = term_new_alo(ls_loc, len, loc + i);
  }
  return term_new_(tag, ext, ari, args);
}
