// (↑a & b)
// --------- AND-INC
// ↑(a & b)
fn Term wnf_and_inc(u64 and_loc, Term inc, Term b) {
  ITRS_INC("AND-INC");
  u64  inc_loc = term_val(inc);
  Term a       = heap_read(inc_loc);
  Term and_tm  = term_new_and_at(and_loc, a, b);
  heap_set(inc_loc, and_tm);
  return inc;
}
