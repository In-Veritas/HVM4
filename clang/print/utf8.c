// Print a Unicode codepoint as UTF-8.
// Emits a codepoint as UTF-8 without validation; escaping helpers gate usage.
enum {
  PRINT_ESC_CHAR = 0,
  PRINT_ESC_STR  = 1,
};

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

// Returns true when the codepoint can be printed with escapes.
fn int print_utf8_can_escape(u32 c, u8 mode) {
  if (c > 0x10FFFF) {
    return 0;
  }
  if (c >= 0xD800 && c <= 0xDFFF) {
    return 0;
  }
  switch (c) {
    case '\n': {
      return 1;
    }
    case '\t': {
      return 1;
    }
    case '\r': {
      return 1;
    }
    case '\0': {
      return 1;
    }
    case '\\': {
      return 1;
    }
    case '\'': {
      if (mode == PRINT_ESC_CHAR) {
        return 1;
      }
      break;
    }
    case '"': {
      if (mode == PRINT_ESC_STR) {
        return 1;
      }
      break;
    }
  }
  if (c < 0x20) {
    return 0;
  }
  if (c == 0x7F) {
    return 0;
  }
  if (c >= 0x80 && c <= 0x9F) {
    return 0;
  }
  return 1;
}

// Prints an escaped codepoint when possible, returning 1 on success.
fn int print_utf8_escape(FILE *f, u32 c, u8 mode) {
  if (!print_utf8_can_escape(c, mode)) {
    return 0;
  }
  switch (c) {
    case '\n': {
      fputs("\\n", f);
      return 1;
    }
    case '\t': {
      fputs("\\t", f);
      return 1;
    }
    case '\r': {
      fputs("\\r", f);
      return 1;
    }
    case '\0': {
      fputs("\\0", f);
      return 1;
    }
    case '\\': {
      fputs("\\\\", f);
      return 1;
    }
    case '\'': {
      if (mode == PRINT_ESC_CHAR) {
        fputs("\\'", f);
        return 1;
      }
      break;
    }
    case '"': {
      if (mode == PRINT_ESC_STR) {
        fputs("\\\"", f);
        return 1;
      }
      break;
    }
  }
  print_utf8(f, c);
  return 1;
}
