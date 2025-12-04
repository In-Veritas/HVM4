// Auto-dup: wraps a term with N uses of a variable in N-1 dups.
// Example: [x,x,x] becomes !d0&=x; !d1&=d0₁; [d0₀,d1₀,d1₁]

fn void parse_auto_dup_go(u64 loc, u32 idx, u32 *use, u32 n, u32 lab);
fn Term parse_auto_dup(Term body, u32 idx, u32 uses);

// Auto-dup for CO0/CO1: wraps a term with N uses of a co-reference in N-1 dups.
// Example: [X₀,X₀,X₀] becomes !d0&=X₀; !d1&=d0₁; [d0₀,d1₀,d1₁]
// Parameters:
//   body: the term to transform
//   idx: De Bruijn index of the original dup binding
//   uses: number of times the CO reference is used
//   orig_lab: label of the original dup binding
//   co_tag: CO0 or CO1 (which side we're duplicating)
fn void parse_auto_dup_co_go(u64 loc, u32 idx, u32 *use, u32 n, u32 new_lab, u32 orig_lab, u8 co_tag) {
  Term t   = HEAP[loc];
  u8  tag  = term_tag(t);
  u32 val  = term_val(t);
  u32 ext  = term_ext(t);

  // Replace target CO0/CO1 with new CO0/CO1
  if (tag == co_tag && ext == orig_lab && val == idx) {
    u32 i = (*use)++;
    if (i < n) {
      HEAP[loc] = term_new(0, CO0, new_lab + i, idx + n - 1 - i);
    } else {
      HEAP[loc] = term_new(0, CO1, new_lab + n - 1, idx);
    }
    return;
  }

  // Shift outer VARs/COs by n (variables bound outside the dup chain)
  // Note: we only shift if val > idx, so the "other side" CO references to the same
  // dup are NOT shifted here. They will be found at the correct depth when the
  // DUP chain is traversed (each DUP adds 1 to idx when recursing into body).
  if ((tag == VAR || tag == CO0 || tag == CO1) && val > idx) {
    HEAP[loc] = term_new(0, tag, ext, val + n);
    return;
  }

  // Recurse into children
  u32 ari = 0;
  u32 bnd = 0;
  switch (tag) {
    case LAM: {
      ari = 1;
      bnd = 1;
      break;
    }
    case USE: {
      ari = 1;
      break;
    }
    case APP:
    case SUP:
    case MAT:
    case SWI:
    case DRY: {
      ari = 2;
      break;
    }
    case DUP: {
      parse_auto_dup_co_go(val + 0, idx + 0, use, n, new_lab, orig_lab, co_tag);
      parse_auto_dup_co_go(val + 1, idx + 1, use, n, new_lab, orig_lab, co_tag);
      return;
    }
    case C00 ... C16: {
      ari = tag - C00;
      break;
    }
    case OP2:
    case EQL: {
      ari = 2;
      break;
    }
    case DSU:
    case DDU: {
      ari = 3;
      break;
    }
    default: {
      return;
    }
  }
  for (u32 i = 0; i < ari; i++) {
    parse_auto_dup_co_go(val + i, idx + bnd, use, n, new_lab, orig_lab, co_tag);
  }
}

fn Term parse_auto_dup_co(Term body, u32 idx, u32 uses, u32 orig_lab, u8 co_tag) {
  if (uses <= 1) {
    return body;
  }
  u32 n   = uses - 1;
  u32 new_lab = PARSE_FRESH_LAB;
  PARSE_FRESH_LAB += n;

  // Walk body's children
  u8  tag = term_tag(body);
  u32 val = term_val(body);
  u32 use = 0;
  u32 ari = 0;
  u32 bnd = 0;
  switch (tag) {
    case LAM: {
      ari = 1;
      bnd = 1;
      break;
    }
    case USE: {
      ari = 1;
      break;
    }
    case APP:
    case SUP:
    case MAT:
    case SWI:
    case DRY: {
      ari = 2;
      break;
    }
    case DUP: {
      parse_auto_dup_co_go(val + 0, idx + 0, &use, n, new_lab, orig_lab, co_tag);
      parse_auto_dup_co_go(val + 1, idx + 1, &use, n, new_lab, orig_lab, co_tag);
      ari = 0;
      break;
    }
    case C00 ... C16: {
      ari = tag - C00;
      break;
    }
    case OP2:
    case EQL: {
      ari = 2;
      break;
    }
    case DSU:
    case DDU: {
      ari = 3;
      break;
    }
    default: {
      break;
    }
  }
  for (u32 i = 0; i < ari; i++) {
    parse_auto_dup_co_go(val + i, idx + bnd, &use, n, new_lab, orig_lab, co_tag);
  }

  // Build dup chain: !d0&=X₀; !d1&=d0₁; ... body
  // The first dup references the original CO0/CO1
  Term result = body;
  for (int i = n - 1; i >= 0; i--) {
    Term v;
    if (i == 0) {
      v = term_new(0, co_tag, orig_lab, idx);  // Reference original CO0 or CO1
    } else {
      v = term_new(0, CO1, new_lab + i - 1, 0);
    }
    u64 loc = heap_alloc(2);
    HEAP[loc + 0] = v;
    HEAP[loc + 1] = result;
    result = term_new(0, DUP, new_lab + i, loc);
  }
  return result;
}

fn void parse_auto_dup_go(u64 loc, u32 idx, u32 *use, u32 n, u32 lab) {
  Term t   = HEAP[loc];
  u8  tag  = term_tag(t);
  u32 val  = term_val(t);

  // Replace target VAR with CO0/CO1
  if (tag == VAR && val == idx) {
    u32 i = (*use)++;
    if (i < n) {
      HEAP[loc] = term_new(0, CO0, lab + i, idx + n - 1 - i);
    } else {
      HEAP[loc] = term_new(0, CO1, lab + n - 1, idx);
    }
    return;
  }

  // Shift variables by n to account for the inserted DUP chain
  // - VARs with val != idx: shift (target VARs are handled above)
  // - All COs: always shift (they reference different bindings)
  if (tag == VAR && val != idx) {
    HEAP[loc] = term_new(0, tag, term_ext(t), val + n);
    return;
  }
  if (tag == CO0 || tag == CO1) {
    HEAP[loc] = term_new(0, tag, term_ext(t), val + n);
    return;
  }

  // Recurse into children
  u32 ari = 0;
  u32 bnd = 0;
  switch (tag) {
    case LAM: {
      ari = 1;
      bnd = 1;
      break;
    }
    case USE: {
      ari = 1;
      break;
    }
    case APP:
    case SUP:
    case MAT:
    case SWI:
    case DRY: {
      ari = 2;
      break;
    }
    case DUP: {
      parse_auto_dup_go(val + 0, idx + 0, use, n, lab);
      parse_auto_dup_go(val + 1, idx + 1, use, n, lab);
      return;
    }
    case C00 ... C16: {
      ari = tag - C00;
      break;
    }
    case OP2:
    case EQL: {
      ari = 2;
      break;
    }
    case DSU:
    case DDU: {
      ari = 3;
      break;
    }
    default: {
      return;
    }
  }
  for (u32 i = 0; i < ari; i++) {
    parse_auto_dup_go(val + i, idx + bnd, use, n, lab);
  }
}

fn Term parse_auto_dup(Term body, u32 idx, u32 uses) {
  if (uses <= 1) {
    return body;
  }
  u32 n   = uses - 1;
  u32 lab = PARSE_FRESH_LAB;
  PARSE_FRESH_LAB += n;

  // Walk body's children
  u8  tag = term_tag(body);
  u32 val = term_val(body);
  u32 use = 0;
  u32 ari = 0;
  u32 bnd = 0;
  switch (tag) {
    case LAM: {
      ari = 1;
      bnd = 1;
      break;
    }
    case USE: {
      ari = 1;
      break;
    }
    case APP:
    case SUP:
    case MAT:
    case SWI:
    case DRY: {
      ari = 2;
      break;
    }
    case DUP: {
      parse_auto_dup_go(val + 0, idx + 0, &use, n, lab);
      parse_auto_dup_go(val + 1, idx + 1, &use, n, lab);
      ari = 0;
      break;
    }
    case C00 ... C16: {
      ari = tag - C00;
      break;
    }
    case OP2:
    case EQL: {
      ari = 2;
      break;
    }
    case DSU:
    case DDU: {
      ari = 3;
      break;
    }
    default: {
      break;
    }
  }
  for (u32 i = 0; i < ari; i++) {
    parse_auto_dup_go(val + i, idx + bnd, &use, n, lab);
  }

  // Build dup chain: !d0&=x; !d1&=d0₁; ... body
  Term result = body;
  for (int i = n - 1; i >= 0; i--) {
    Term v;
    if (i == 0) {
      v = term_new(0, VAR, 0, idx);
    } else {
      v = term_new(0, CO1, lab + i - 1, 0);
    }
    u64 loc = heap_alloc(2);
    HEAP[loc + 0] = v;
    HEAP[loc + 1] = result;
    result = term_new(0, DUP, lab + i, loc);
  }
  return result;
}
