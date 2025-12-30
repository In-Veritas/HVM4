fn void parse_bind_push(u32 name, u32 depth, u32 lab, u32 kind, u32 cloned) {
  PARSE_BINDS[PARSE_BINDS_LEN++] = (PBind){name, depth, lab, kind, cloned, 0, 0, 0};
}
