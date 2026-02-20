// (name a)
// --------- APP-NAM
// ^(name a)
fn Term wnf_app_nam(u64 app_loc, Term nam) {
  heap_set(app_loc + 0, nam);
  return term_new(0, DRY, 0, app_loc);
}
