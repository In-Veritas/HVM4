fn int parse_match(PState *s, const char *str) {
  if (!parse_starts_with(s, str)) {
    return 0;
  }
  while (*str) {
    parse_advance(s);
    str++;
  }
  return 1;
}
