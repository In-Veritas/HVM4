// @{s} n
// ------ ALO-GOT
// s[n] or n when substitution missing (n is a de Bruijn level)
fn Term wnf_alo_got(u32 ls, u32 len, u32 lvl) {
  if (lvl == 0 || lvl > len) {
    return term_new(0, BJM, 0, lvl);
  }
  u32 idx = len - lvl;
  u32 it  = ls;
  for (u32 i = 0; i < idx && it != 0; i++) {
    it = (u32)(heap_read(it) & 0xFFFFFFFF);
  }
  u32 bind = (it != 0) ? (u32)(heap_read(it) >> 32) : 0;
  return bind ? term_new_got(bind) : term_new(0, BJM, 0, lvl);
}
