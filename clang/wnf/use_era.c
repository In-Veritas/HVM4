// (λ{f} &{})
// ---------- use-era
// &{}
fn Term wnf_use_era() {
  ITRS++;
  return term_new_era();
}
