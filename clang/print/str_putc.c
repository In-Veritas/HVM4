fn void print_str_putc(char c) {
  if (STR_MODE == STR_LOG) {
    putchar(c);
  } else {
    if (TERM_BUF_POS + 1 >= TERM_BUF_CAP) {
      TERM_BUF_CAP *= 2;
      TERM_BUF = realloc(TERM_BUF, TERM_BUF_CAP);
    }
    TERM_BUF[TERM_BUF_POS++] = c;
    TERM_BUF[TERM_BUF_POS]   = 0;
  }
}
