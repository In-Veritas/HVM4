// (↑x op y)
// ---------- OP2-INC-X
// ↑(x op y)
fn Term wnf_op2_inc_x(u32 opr, Term inc, Term y) {
  ITRS_INC("OP2-INC-X");
  u64  inc_loc = term_val(inc);
  Term x       = heap_read(inc_loc);
  Term op      = term_new_op2(opr, x, y);
  heap_set(inc_loc, op);
  return inc;
}

// (#n op ↑y)
// ---------- OP2-INC-Y
// ↑(#n op y)
fn Term wnf_op2_inc_y(u32 opr, Term x, Term inc) {
  ITRS_INC("OP2-INC-Y");
  u64  inc_loc = term_val(inc);
  Term y       = heap_read(inc_loc);
  Term op      = term_new_op2(opr, x, y);
  heap_set(inc_loc, op);
  return inc;
}
