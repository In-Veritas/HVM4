// (λ{af} === λ{bf})
// ----------------- EQL-USE
// af === bf
fn Term wnf_eql_use(Term a, Term b) {
  ITRS_INC("EQL-USE");
  u64  a_loc = term_val(a);
  u64  b_loc = term_val(b);
  Term af    = heap_read(a_loc);
  Term bf    = heap_read(b_loc);
  return term_new_eql(af, bf);
}
