fn char *print_term_to_str(Term term) {
  STR_MODE     = STR_BUF;
  TERM_BUF_POS = 0;
  print_str_term_go(term, 0);
  return TERM_BUF;
}
