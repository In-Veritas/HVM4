fn void print_str_puts(const char *s) {
  while (*s) {
    print_str_putc(*s++);
  }
}
