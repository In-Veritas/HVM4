// Decode UTF-8 codepoint at current position, advance past it
fn u32 parse_utf8(PState *s) {
  u8 b0 = (u8)s->src[s->pos];
  parse_advance(s);
  if (b0 < 0x80) return b0;
  if ((b0 & 0xE0) == 0xC0 && s->pos < s->len) {
    u8 b1 = (u8)s->src[s->pos];
    parse_advance(s);
    return ((b0 & 0x1F) << 6) | (b1 & 0x3F);
  }
  if ((b0 & 0xF0) == 0xE0 && s->pos + 1 < s->len) {
    u8 b1 = (u8)s->src[s->pos];
    parse_advance(s);
    u8 b2 = (u8)s->src[s->pos];
    parse_advance(s);
    return ((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F);
  }
  if ((b0 & 0xF8) == 0xF0 && s->pos + 2 < s->len) {
    u8 b1 = (u8)s->src[s->pos];
    parse_advance(s);
    u8 b2 = (u8)s->src[s->pos];
    parse_advance(s);
    u8 b3 = (u8)s->src[s->pos];
    parse_advance(s);
    return ((b0 & 0x07) << 18) | ((b1 & 0x3F) << 12)
         | ((b2 & 0x3F) << 6) | (b3 & 0x3F);
  }
  return b0;
}
