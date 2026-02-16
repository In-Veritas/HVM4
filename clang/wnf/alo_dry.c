// @{s} ^(f x)
// -------------- ALO-DRY
// ^(@{s}f @{s}x)
fn Term wnf_alo_dry(u64 ls_loc, u16 len, u64 loc) {
  if (len == 0) {
    Term fun = term_new(0, ALO, 0, loc + 0);
    Term arg = term_new(0, ALO, 0, loc + 1);
    return term_new_dry(fun, arg);
  }
  u64 alo_loc0 = heap_alloc(1);
  u64 alo_loc1 = heap_alloc(1);
  heap_set(alo_loc0, ((u64)ls_loc << 32) | (loc + 0));
  heap_set(alo_loc1, ((u64)ls_loc << 32) | (loc + 1));
  Term fun = term_new(0, ALO, len, alo_loc0);
  Term arg = term_new(0, ALO, len, alo_loc1);
  return term_new_dry(fun, arg);
}
