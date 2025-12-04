// Auto-dup: wraps a term with N uses of a variable in N-1 dups.
// Example: [x,x,x] becomes !d0&=x; !d1&=d0₁; [d0₀,d1₀,d1₁]
//
// Works for both VAR refs (let/lambda bindings) and CO refs (dup bindings).
// - Target is identified by tag + idx (and ext for CO mode)
// - Outer refs (val >= dep) are shifted by n to account for new DUPs

fn void auto_dup_go(u64 loc, u32 idx, u32 dep, u32 *use, u32 n, u32 lab, u8 tgt, u32 ext) {
  Term t = HEAP[loc];
  u8  tg = term_tag(t);
  u32 vl = term_val(t);

  // Replace target ref with CO0/CO1 chain
  if (tg == tgt && vl == idx && (tgt == VAR || term_ext(t) == ext)) {
    u32 i = (*use)++;
    HEAP[loc] = (i < n)
      ? term_new(0, CO0, lab + i, idx + n - 1 - i)
      : term_new(0, CO1, lab + n - 1, idx);
    return;
  }

  // Shift outer refs
  if ((tg == VAR || tg == CO0 || tg == CO1) && vl >= dep) {
    HEAP[loc] = term_new(0, tg, term_ext(t), vl + n);
    return;
  }

  // Recurse into children
  switch (tg) {
    case LAM: {
      auto_dup_go(vl, idx + 1, dep + 1, use, n, lab, tgt, ext);
      return;
    }
    case DUP: {
      auto_dup_go(vl + 0, idx + 0, dep + 0, use, n, lab, tgt, ext);
      auto_dup_go(vl + 1, idx + 1, dep + 1, use, n, lab, tgt, ext);
      return;
    }
    default: {
      u32 ari = term_arity(t);
      for (u32 i = 0; i < ari; i++) {
        auto_dup_go(vl + i, idx, dep, use, n, lab, tgt, ext);
      }
    }
  }
}

fn Term parse_auto_dup(Term body, u32 idx, u32 uses, u8 tgt, u32 ext) {
  if (uses <= 1) {
    return body;
  }

  u32 n   = uses - 1;
  u32 lab = PARSE_FRESH_LAB;
  PARSE_FRESH_LAB += n;

  // Walk body's children
  u8  tg  = term_tag(body);
  u32 vl  = term_val(body);
  u32 use = 0;

  switch (tg) {
    case LAM: {
      auto_dup_go(vl, idx + 1, 1, &use, n, lab, tgt, ext);
      break;
    }
    case DUP: {
      auto_dup_go(vl + 0, idx + 0, 0, &use, n, lab, tgt, ext);
      auto_dup_go(vl + 1, idx + 1, 1, &use, n, lab, tgt, ext);
      break;
    }
    default: {
      u32 ari = term_arity(body);
      for (u32 i = 0; i < ari; i++) {
        auto_dup_go(vl + i, idx, 0, &use, n, lab, tgt, ext);
      }
    }
  }

  // Build dup chain: !d0&=x; !d1&=d0₁; ... body
  Term result = body;
  for (int i = n - 1; i >= 0; i--) {
    Term v   = (i == 0) ? term_new(0, tgt, ext, idx) : term_new(0, CO1, lab + i - 1, 0);
    u64  loc = heap_alloc(2);
    HEAP[loc + 0] = v;
    HEAP[loc + 1] = result;
    result = term_new(0, DUP, lab + i, loc);
  }

  return result;
}
