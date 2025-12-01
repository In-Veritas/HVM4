fn void print_term_buf_init(void) {
  TERM_BUF_CAP = 65536;
  TERM_BUF     = malloc(TERM_BUF_CAP);
  TERM_BUF_POS = 0;
}
