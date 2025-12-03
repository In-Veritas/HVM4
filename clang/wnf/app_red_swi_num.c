// ((f ~> λ{n:z;s}) #n)
// -------------------- app-red-swi-match
// (f #n) ~> z
fn Term wnf_app_red_swi_match(Term f, Term swi, Term num) {
  ITRS++;
  u32  swi_loc = term_val(swi);
  Term z       = HEAP[swi_loc + 0];
  return term_new_red(term_new_app(f, num), z);
}

// ((f ~> λ{n:z;s}) #m) where m != n
// ---------------------------------- app-red-swi-miss
// ((λp.(f (1+p)) ~> s) #(m-1))
fn Term wnf_app_red_swi_miss(Term f, Term swi, Term num) {
  ITRS++;
  u32  swi_loc = term_val(swi);
  u32  num_val = term_val(num);
  Term s       = HEAP[swi_loc + 1];

  // Build λp.(f (1+p))
  u64 lam_loc  = heap_alloc(1);
  Term var_p   = term_new(0, VAR, 0, lam_loc);
  Term succ_p  = term_new_op2(OP_ADD, term_new_num(1), var_p);
  Term body    = term_new_app(f, succ_p);
  HEAP[lam_loc] = body;
  Term lam     = term_new(0, LAM, 0, lam_loc);

  // (lam ~> s) #(m-1)
  Term pred_num = term_new_num(num_val - 1);
  return term_new_app(term_new_red(lam, s), pred_num);
}
