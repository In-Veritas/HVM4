// (λ{n:f;g} #m)
// -------------- swi-num
// if n == m: f
// else: (g #m)
fn Term wnf_app_swi_num(Term swi, Term num) {
  ITRS++;
  u32  n   = term_ext(swi);
  u32  loc = term_val(swi);
  Term f   = HEAP[loc + 0];
  Term g   = HEAP[loc + 1];
  if (term_val(num) == n) {
    return f;
  } else {
    return term_new_app(g, num);
  }
}
