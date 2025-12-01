fn void sys_error(const char *msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  exit(1);
}
