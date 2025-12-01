fn int parse_starts_with(PState *s, const char *str) {
  u32 i = 0;
  while (str[i]) {
    if (parse_peek_at(s, i) != str[i]) {
      return 0;
    }
    i++;
  }
  return 1;
}
