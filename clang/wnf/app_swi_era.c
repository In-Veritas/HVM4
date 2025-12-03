// (λ{n:f;g} &{})
// --------------- swi-era
// &{}
fn Term wnf_app_swi_era() {
  ITRS++;
  return term_new_era();
}
