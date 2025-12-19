fn Term snf(Term term, u32 depth, u8 quoted) {
  term = wnf(term);
  if (quoted && term_tag(term) == DUP) {
    u32 loc = term_val(term);
    u32 lab = term_ext(term);
    Term val = HEAP[loc + 0];
    Term bod = HEAP[loc + 1];
    u32 level = depth + 1;
    Term bj0 = term_new(0, BJ0, lab, level);
    Term bj1 = term_new(0, BJ1, lab, level);
    HEAP[loc + 0] = term_new_sup(lab, bj0, bj1);
    Term val_q = snf(val, depth, quoted);
    Term bod_q = snf(bod, depth + 1, quoted);
    u64 out_loc = heap_alloc(2);
    HEAP[out_loc + 0] = val_q;
    HEAP[out_loc + 1] = bod_q;
    return term_new(0, DUP, lab, out_loc);
  }
  if (quoted) {
    u8 tag = term_tag(term);
    if (tag == BJV || tag == BJ0 || tag == BJ1) {
      return term;
    }
    if (tag == VAR) {
      return term_new(0, BJV, 0, depth);
    }
    if (tag == CO0 || tag == CO1) {
      u8  bj_tag = tag == CO0 ? BJ0 : BJ1;
      u32 lab    = term_ext(term);
      return term_new(0, bj_tag, lab, depth);
    }
  }

  u32 ari = term_arity(term);
  if (ari == 0) {
    return term;
  }
  u64 loc = term_val(term);
  if (term_tag(term) == LAM) {
    Term body = HEAP[loc];
    if (quoted) {
      u32 level = depth + 1;
      heap_subst_var(loc, term_new(0, BJV, 0, level));
      HEAP[loc] = snf(body, depth + 1, quoted);
      term = term_new(0, LAM, level, loc);
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
