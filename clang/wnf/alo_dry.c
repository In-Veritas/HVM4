// @{s} ^(f x)
// -------------- ALO-DRY
// ^(@{s}f @{s}x)
fn Term wnf_alo_dry(u64 ls_loc, u16 len, u64 loc) {
  if (len == 0) {
    Term fun = term_new(0, ALO, 0, loc + 0);
    Term arg = term_new(0, ALO, 0, loc + 1);
    return term_new_dry(fun, arg);
  }
  u64 alo0 = heap_alloc(1);
  heap_set(alo0, alo_pair_pack(ls_loc, loc + 0));
  Term fun = term_new(0, ALO, len, alo0);

  u64 alo1 = heap_alloc(1);
  heap_set(alo1, alo_pair_pack(ls_loc, loc + 1));
  Term arg = term_new(0, ALO, len, alo1);

  return term_new_dry(fun, arg);
}
