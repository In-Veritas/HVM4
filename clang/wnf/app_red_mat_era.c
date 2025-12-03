// ((f ~> λ{#K:h; m}) &{})
// ----------------------- app-red-mat-era
// &{}
fn Term wnf_app_red_mat_era(void) {
  ITRS++;
  return term_new_era();
}
