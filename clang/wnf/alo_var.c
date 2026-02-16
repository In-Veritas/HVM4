// @{s} n
// ------ ALO-VAR
// s[n] or n when substitution missing (n is a de Bruijn level)
fn Term wnf_alo_var(u64 ls, u16 len, Term book) {
  u16 lvl = (u16)term_val(book);
  u8  tag = term_tag(book);
  u16 ext = term_ext(book);
  if (lvl == 0 || lvl > len) {
    return term_new(0, tag, ext, lvl);
  }
  u16 idx = len - lvl;
  u64 it  = ls;
  for (u16 i = 0; i < idx && it != 0; i++) {
    it = term_val(heap_read(it + 1));
  }
  return it != 0 ? term_new_var(it) : term_new(0, tag, ext, lvl);
}
