fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_dup(PState *s, u32 depth) {
  parse_skip(s);
  // Check for !!x = val or !!&x = val (strict let, optionally cloned)
  int strict = parse_match(s, "!");
  parse_skip(s);
  // Check for cloned: '&' BEFORE name
  u32 cloned = 0;
  if (parse_peek(s) == '&') {
    parse_advance(s);  // consume &
    parse_skip(s);
    cloned = 1;
  }
  u32 nam = parse_name(s);
  parse_skip(s);

  // Let sugar: ! x = val; body  →  (λx.body)(val)
  // Or cloned let: ! &x = val; body
  // Or unscoped lambda: ! f = λ x ; body
  if (parse_peek(s) == '=') {
    parse_advance(s);
    parse_skip(s);
    // Check for unscoped lambda: ! f = λ x ; body
    // Lookahead: save state, try λ name ;, restore if not matched
    PState save = *s;
    if (parse_match(s, "λ")) {
      parse_skip(s);
      u32 nam_v = parse_name(s);
      parse_skip(s);
      if (parse_match(s, ";")) {
        // Confirmed unscoped lambda
        parse_skip(s);
        parse_bind_push(nam, depth, 0, 0, 0);
        parse_bind_push(nam_v, depth + 1, 0, 0, 0);
        u64 loc_f = heap_alloc(1);
        u64 loc_v = heap_alloc(1);
        Term body = parse_term(s, depth + 2);
        parse_bind_pop();
        parse_bind_pop();
        HEAP[loc_v] = body;
        Term lam_v = term_new(0, LAM, depth + 2, loc_v);
        HEAP[loc_f] = lam_v;
        Term lam_f = term_new(0, LAM, depth + 1, loc_f);
        return term_new_uns(lam_f);
      }
      // Not unscoped lambda, restore position
      *s = save;
    }
    Term val = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ";");
    PBind* bind = parse_bind_push(nam, depth, 0, 0, cloned);
    u64  loc  = heap_alloc(1);
    Term body = parse_term(s, depth + 1);
    if (cloned) {
      body = parse_auto_dup(body, depth + 1, depth + 1, BJV, 0, bind->uses);
    }
    parse_bind_pop();
    HEAP[loc] = body;
    Term lam = term_new(0, LAM, depth + 1, loc);
    if (strict) {
      // !!x = val; body  →  (λ{λx.body(x)})(val)
      lam = term_new_use(lam);
    }
    return term_new_app(lam, val);
  }

  // Regular DUP term: !x&label = val; body  or  !x& = val; body (auto-label)
  // Cloned DUP term: !&X &label = val; body  or  !&X & = val; body (auto-label)
  // Dynamic DUP term: !x&(lab) = val; body  (lab is an expression)
  // Cloned Dynamic DUP term: !&X &(lab) = val; body
  parse_consume(s, "&");
  parse_skip(s);
  // Dynamic label: (expr)
  if (parse_peek(s) == '(') {
    parse_consume(s, "(");
    Term lab_term = parse_term(s, depth);
    parse_consume(s, ")");
    parse_consume(s, "=");
    Term val = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ";");
    parse_skip(s);
    PBind* bind = parse_bind_push(nam, depth, 0xFFFFFF, 0, cloned);
    Term body = parse_term(s, depth + 2);
    if (cloned) {
      body = parse_auto_dup(body, depth + 1, depth + 2, BJV, 0, bind->uses0);
      body = parse_auto_dup(body, depth + 2, depth + 2, BJV, 0, bind->uses1);
    }
    parse_bind_pop();
    u64 loc0   = heap_alloc(1);
    u64 loc1   = heap_alloc(1);
    HEAP[loc1] = body;
    Term lam1  = term_new(0, LAM, depth + 2, loc1);
    HEAP[loc0] = lam1;
    Term lam0  = term_new(0, LAM, depth + 1, loc0);
    return term_new_ddu(lab_term, val, lam0);
  }
  // Static label (or auto if next is =)
  u32 lab;
  if (parse_peek(s) == '=') {
    lab = PARSE_FRESH_LAB++;
  } else {
    lab = parse_name(s);
  }
  u64 loc = heap_alloc(2);
  parse_consume(s, "=");
  HEAP[loc] = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ";");
  parse_skip(s);
  PBind* bind = parse_bind_push(nam, depth, lab, 0, cloned);
  Term body   = parse_term(s, depth + 1);
  if (cloned) {
    body = parse_auto_dup(body, depth + 1, depth + 1, BJ1, lab, bind->uses1);
    body = parse_auto_dup(body, depth + 1, depth + 1, BJ0, lab, bind->uses0);
  }
  parse_bind_pop();
  HEAP[loc + 1] = body;
  return term_new(0, DUP, lab, loc);
}
