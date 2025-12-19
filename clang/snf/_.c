fn Term snf(Term term, u32 depth, u8 quoted) {
  term = wnf(term);
  u32 ari = term_arity(term);
  if (ari == 0) {
    return term;
  }
  u64 loc = term_val(term);
  if (term_tag(term) == LAM) {
    Term body = HEAP[loc];
    if (quoted) {
      // ^(depth+1) for stuck variable
      heap_subst_var(loc, term_new_nam(depth + 1));
      HEAP[loc] = snf(body, depth + 1, quoted);
      term = term_new(0, LAM, depth + 1, loc);
    } else {
      HEAP[loc] = snf(body, depth + 1, quoted);
    }
  } else if (term_tag(term) == DRY) {
    HEAP[loc + 0] = snf(HEAP[loc + 0], depth, quoted);
    HEAP[loc + 1] = snf(HEAP[loc + 1], depth, quoted);
  } else {
    for (u32 i = 0; i < ari; i++) {
      HEAP[loc + i] = snf(HEAP[loc + i], depth, quoted);
    }
  }
  return term;
}
