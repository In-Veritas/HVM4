fn u32 parse_char_lit(PState *s) {
  parse_advance(s);
  u32 c;
  if (parse_peek(s) == '\\') {
    parse_advance(s);
    c = (u32)(u8)parse_peek(s);
    parse_advance(s);
  } else {
    c = parse_utf8(s);
  }
  if (parse_peek(s) != '\'') {
    parse_error(s, "'", parse_peek(s));
  }
  parse_advance(s);
  return c;
}

fn Term parse_term_chr(PState *s) {
  u32 c = parse_char_lit(s);
  Term n = term_new_num(c);
  return term_new_ctr(NAM_CHR, 1, &n);
}
