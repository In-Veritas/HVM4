// @{s} n
// ------ ALO-VAR
// s[n] or n when substitution missing (n is a de Bruijn level)
fn Term wnf_alo_var(u32 ls, u32 len, u32 lvl, u8 tag) {
  if (lvl == 0 || lvl > len) {
    return term_new(0, tag, 0, lvl);
  }
  u32 idx = len - lvl;
  u32 it  = ls;
  for (u32 i = 0; i < idx && it != 0; i++) {
    it = term_val(heap_read(it + 1));
  }
  return it != 0 ? term_new_var(it) : term_new(0, tag, 0, lvl);
}
