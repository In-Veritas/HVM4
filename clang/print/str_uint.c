fn void print_str_uint(u32 n) {
  char buf[32];
  int  len = 0;
  if (n == 0) {
    print_str_putc('0');
    return;
  }
  while (n > 0 && len < 31) {
    buf[len++] = (char)('0' + (n % 10));
    n /= 10;
  }
  for (int i = len - 1; i >= 0; i--) {
    print_str_putc(buf[i]);
  }
}
