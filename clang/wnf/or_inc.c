// (↑a | b)
// --------- OR-INC
// ↑(a | b)
fn Term wnf_or_inc(u64 loc, Term inc, Term b) {
  ITRS_INC("OR-INC");
  u64  inc_loc = term_val(inc);
  Term a       = heap_read(inc_loc);
  Term or_tm   = term_new_or_at(loc, a, b);
  heap_set(inc_loc, or_tm);
  return inc;
}
