fn void sys_path_join(char *out, int size, const char *base, const char *rel) {
  if (rel[0] == '/') {
    snprintf(out, size, "%s", rel);
    return;
  }
  const char *slash = strrchr(base, '/');
  if (slash) {
    snprintf(out, size, "%.*s/%s", (int)(slash - base), base, rel);
  } else {
    snprintf(out, size, "%s", rel);
  }
}
