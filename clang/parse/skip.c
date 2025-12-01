fn void parse_skip(PState *s) {
  while (!parse_at_end(s)) {
    if (parse_is_space(parse_peek(s))) {
      parse_advance(s);
      continue;
    }
    if (parse_starts_with(s, "//")) {
      parse_skip_comment(s);
      continue;
    }
    break;
  }
}
