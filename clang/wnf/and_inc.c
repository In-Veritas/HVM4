// (↑a & b)
// --------- AND-INC
// ↑(a & b)
fn Term wnf_and_inc(u64 and_loc, Term inc, Term b) {
  ITRS_INC("AND-INC");
  u64  inc_loc = term_val(inc);
  Term a       = heap_read(inc_loc);
  heap_set(and_loc + 0, a);
  heap_set(inc_loc, term_new(0, AND, 0, and_loc));
  return inc;
}
