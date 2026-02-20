// (^(f x) a)
// ----------- APP-DRY
// ^(^(f x) a)
fn Term wnf_app_dry(u64 app_loc, Term dry) {
  heap_set(app_loc + 0, dry);
  return term_new(0, DRY, 0, app_loc);
}
