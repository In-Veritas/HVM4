// ! X &(↑x) = v; b
// ---------------- DDU-INC
// ↑(! X &(x) = v; b)
fn Term wnf_ddu_inc(Term inc, Term val, Term bod) {
  ITRS_INC("DDU-INC");
  u64  inc_loc = term_val(inc);
  Term x       = heap_read(inc_loc);
  Term new_ddu = term_new_ddu(x, val, bod);
  heap_set(inc_loc, new_ddu);
  return inc;
}
