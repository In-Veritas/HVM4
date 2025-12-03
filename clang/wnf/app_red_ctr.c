// ((f ~> #K{...}) a)
// ------------------ app-red-ctr
// ^((f ~> #K{...}) a)
fn Term wnf_app_red_ctr(Term f, Term ctr, Term arg) {
  ITRS++;
  return term_new_dry(term_new_red(f, ctr), arg);
}
