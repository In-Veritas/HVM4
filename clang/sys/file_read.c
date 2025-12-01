fn char *sys_file_read(const char *path) {
  FILE *fp = fopen(path, "rb");
  if (!fp) {
    return NULL;
  }
  fseek(fp, 0, SEEK_END);
  long len = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  char *src = malloc(len + 1);
  if (!src) {
    sys_error("OOM");
  }
  fread(src, 1, len, fp);
  src[len] = 0;
  fclose(fp);
  return src;
}
