fn void parse_skip_comment(PState *s) {
  while (!parse_at_end(s) && parse_peek(s) != '\n') {
    parse_advance(s);
  }
}
