// Parse INC: ↑x
fn Term parse_term_inc(PState *s, u32 depth) {
  Term x = parse_term_atom(s, depth);
  return term_new_inc(x);
}
