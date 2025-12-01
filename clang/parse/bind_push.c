fn void parse_bind_push(u32 name, u32 depth, u32 lab) {
  PARSE_BINDS[PARSE_BINDS_LEN++] = (PBind){name, depth, lab};
}
