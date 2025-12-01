fn void parse_bind_lookup(u32 name, u32 depth, int *idx, u32 *lab) {
  for (int i = PARSE_BINDS_LEN - 1; i >= 0; i--) {
    if (PARSE_BINDS[i].name == name) {
      *idx = depth - 1 - PARSE_BINDS[i].depth;
      *lab = PARSE_BINDS[i].lab;
      return;
    }
  }
  *idx = -1;
  *lab = 0;
}
