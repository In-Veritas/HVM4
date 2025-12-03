// ((f ~> λ{n:z;s}) &{})
// --------------------- app-red-swi-era
// &{}
fn Term wnf_app_red_swi_era(void) {
  ITRS++;
  return term_new_era();
}
