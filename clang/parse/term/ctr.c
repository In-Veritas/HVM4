fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_ctr(PState *s, u32 depth) {
  u32  nam = parse_name_ref(s);
  Term args[16];
  u32  cnt = 0;
  parse_skip(s);
  if (parse_match(s, "{")) {
    parse_skip(s);
    while (parse_peek(s) != '}') {
      if (cnt >= 16) {
        parse_error(s, "at most 16 constructor fields", parse_peek(s));
      }
      args[cnt++] = parse_term(s, depth);
      parse_skip(s);
      parse_match(s, ",");  // optional comma
      parse_skip(s);
    }
    parse_consume(s, "}");
  }
  return term_new_ctr(nam, cnt, args);
}
