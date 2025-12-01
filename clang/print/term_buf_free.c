fn void print_term_buf_free(void) {
  free(TERM_BUF);
  TERM_BUF     = NULL;
  TERM_BUF_POS = 0;
  TERM_BUF_CAP = 0;
}
