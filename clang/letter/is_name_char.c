fn int letter_is_name_init(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

fn int letter_is_name_char(char c) {
  return letter_to_b64(c) >= 0;
}
