// (↑a === b)
// ----------- EQL-INC-L
// ↑(a === b)
fn Term wnf_eql_inc_l(u64 loc, Term inc, Term b) {
  ITRS_INC("EQL-INC-L");
  u64  inc_loc = term_val(inc);
  Term a       = heap_read(inc_loc);
  Term eql     = term_new_eql_at(loc, a, b);
  heap_set(inc_loc, eql);
  return inc;
}

// (a === ↑b)
// ----------- EQL-INC-R
// ↑(a === b)
fn Term wnf_eql_inc_r(u64 loc, Term a, Term inc) {
  ITRS_INC("EQL-INC-R");
  u64  inc_loc = term_val(inc);
  Term b       = heap_read(inc_loc);
  Term eql     = term_new_eql_at(loc, a, b);
  heap_set(inc_loc, eql);
  return inc;
}
