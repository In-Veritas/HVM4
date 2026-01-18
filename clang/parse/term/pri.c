fn Term parse_term(PState *s, u32 depth);
fn u32 prim_arity(u32 id);

fn Term parse_term_pri(PState *s, u32 depth) {
  u32 id = parse_name_ref(s);
  u32 arity = prim_arity(id);

  if (arity == 0) {
    parse_error(s, "known primitive", parse_peek(s));
  }

  parse_skip(s);
  if (parse_peek(s) != '(') {
    parse_error(s, "primitive application", parse_peek(s));
  }
  parse_consume(s, "(");

  Term f = term_new_pri(id);

  for (u32 i = 0; i < arity; i++) {
    parse_skip(s);
    if (parse_peek(s) == ')') {
      parse_error(s, "argument", parse_peek(s));
    }
    Term arg = parse_term(s, depth);
    f = term_new_app(f, arg);
    parse_skip(s);
    parse_match(s, ",");  // optional comma
  }

  parse_skip(s);
  if (parse_peek(s) != ')') {
    parse_error(s, "end of argument list", parse_peek(s));
  }
  parse_consume(s, ")");
  return f;
}
