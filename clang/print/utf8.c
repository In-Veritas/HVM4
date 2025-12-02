// Print a Unicode codepoint as UTF-8
fn void print_utf8(FILE *f, u32 c) {
  if (c < 0x80) {
    fputc(c, f);
  } else if (c < 0x800) {
    fputc(0xC0 | (c >> 6), f);
    fputc(0x80 | (c & 0x3F), f);
  } else if (c < 0x10000) {
    fputc(0xE0 | (c >> 12), f);
    fputc(0x80 | ((c >> 6) & 0x3F), f);
    fputc(0x80 | (c & 0x3F), f);
  } else {
    fputc(0xF0 | (c >> 18), f);
    fputc(0x80 | ((c >> 12) & 0x3F), f);
    fputc(0x80 | ((c >> 6) & 0x3F), f);
    fputc(0x80 | (c & 0x3F), f);
  }
}
