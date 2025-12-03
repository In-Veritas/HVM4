// ((f ~> &{}) a)
// -------------- app-red-era
// &{}
fn Term wnf_app_red_era(void) {
  ITRS++;
  return term_new_era();
}
