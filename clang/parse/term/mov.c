fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_mov(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  parse_skip(s);
  parse_consume(s, "=");
  Term val = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ";");
  parse_skip(s);
  parse_bind_push(nam, depth, 0, PBIND_MOV, 0);
  Term body = parse_term(s, depth + 1);
  parse_bind_pop();
  u64 loc = heap_alloc(2);
  HEAP[loc + 0] = val;
  HEAP[loc + 1] = body;
  return term_new(0, MOV, 0, loc);
}
