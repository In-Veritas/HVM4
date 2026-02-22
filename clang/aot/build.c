// AOT Module: Build Orchestrator
// ------------------------------
// Emits standalone AOT C programs, supports one-shot `--as-c` execution,
// and supports writing a native executable with `-o/--output`.

#include <limits.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

fn void  aot_emit(const char *c_path, const char *runtime_path, const char *src_path, const char *src_text, const AotBuildCfg *cfg);
fn void  aot_emit_stdout(const char *runtime_path, const char *src_path, const char *src_text, const AotBuildCfg *cfg);
fn void  sys_error(const char *msg);

// Resolves argv0 to an absolute executable path.
fn char *aot_build_resolve_argv0(const char *argv0) {
  if (argv0 == NULL || argv0[0] == '\0') {
    return NULL;
  }

  if (strchr(argv0, '/') != NULL) {
    return realpath(argv0, NULL);
  }

  const char *path_env = getenv("PATH");
  if (path_env == NULL || path_env[0] == '\0') {
    return NULL;
  }

  char *buf = strdup(path_env);
  if (buf == NULL) {
    return NULL;
  }

  char *save = NULL;
  for (char *dir = strtok_r(buf, ":", &save); dir != NULL; dir = strtok_r(NULL, ":", &save)) {
    if (dir[0] == '\0') {
      continue;
    }

    char cand[PATH_MAX];
    int n = snprintf(cand, sizeof(cand), "%s/%s", dir, argv0);
    if (n < 0 || n >= (int)sizeof(cand)) {
      continue;
    }

    if (access(cand, X_OK) == 0) {
      char *abs = realpath(cand, NULL);
      free(buf);
      return abs;
    }
  }

  free(buf);
  return NULL;
}

// Resolves absolute runtime path (`.../clang/hvm.c`) from argv0.
fn void aot_build_runtime_path(char *out, u32 out_len, const char *argv0) {
  char *exe = aot_build_resolve_argv0(argv0);
  if (exe == NULL) {
    sys_error("failed to resolve executable path from argv[0]");
  }

  char *slash = strrchr(exe, '/');
  if (slash == NULL) {
    free(exe);
    sys_error("invalid executable path");
  }

  *slash = '\0';
  int n = snprintf(out, out_len, "%s/hvm.c", exe);
  free(exe);

  if (n < 0 || n >= (int)out_len) {
    sys_error("runtime path too long");
  }

  char *abs = realpath(out, NULL);
  if (abs == NULL) {
    fprintf(stderr, "ERROR: failed to resolve runtime file '%s'\n", out);
    exit(1);
  }

  n = snprintf(out, out_len, "%s", abs);
  free(abs);
  if (n < 0 || n >= (int)out_len) {
    sys_error("runtime path too long");
  }
}

// Runs one process and returns its exit code.
fn int aot_build_spawn(char *const argv[]) {
  pid_t pid = fork();
  if (pid < 0) {
    sys_error("fork failed");
  }

  if (pid == 0) {
    execvp(argv[0], argv);
    perror("execvp");
    _exit(127);
  }

  int status = 0;
  if (waitpid(pid, &status, 0) < 0) {
    sys_error("waitpid failed");
  }

  if (WIFEXITED(status)) {
    return WEXITSTATUS(status);
  }

  return 1;
}

// Compiles one generated C program into an executable.
fn int aot_build_compile(const char *c_path, const char *out_path) {
  char *const cmd[] = {
    "clang",
    "-O3",
    "-o",
    (char *)out_path,
    (char *)c_path,
    NULL,
  };

  return aot_build_spawn(cmd);
}

// Writes one AOT C file with an absolute runtime include path.
fn void aot_write_c_file(const char *c_path, const char *argv0, const char *src_path, const char *src_text, const AotBuildCfg *cfg) {
  char runtime_path[PATH_MAX];
  aot_build_runtime_path(runtime_path, sizeof(runtime_path), argv0);
  aot_emit(c_path, runtime_path, src_path, src_text, cfg);
}

// Builds one mkdtemp template path using TMPDIR or /tmp.
fn void aot_build_temp_template(char *out, u32 out_len) {
  const char *tmp = getenv("TMPDIR");
  if (tmp == NULL || tmp[0] == '\0') {
    tmp = "/tmp";
  }

  int n = snprintf(out, out_len, "%s/hvm-aot.XXXXXX", tmp);
  if (n < 0 || n >= (int)out_len) {
    sys_error("AOT temp directory path too long");
  }
}

// Creates one temporary build directory and writes it to `out`.
fn void aot_build_temp_dir(char *out, u32 out_len) {
  aot_build_temp_template(out, out_len);
  if (mkdtemp(out) == NULL) {
    sys_error("failed to create AOT temp directory");
  }
}

// Joins one directory + filename into `out`.
fn void aot_build_join(char *out, u32 out_len, const char *dir, const char *file) {
  int n = snprintf(out, out_len, "%s/%s", dir, file);
  if (n < 0 || n >= (int)out_len) {
    sys_error("AOT temp path too long");
  }
}

// Removes temporary C/executable files and then removes the temp directory.
fn void aot_build_cleanup(const char *tmp_dir, const char *c_path, const char *x_path) {
  if (x_path != NULL && x_path[0] != '\0') {
    unlink(x_path);
  }
  if (c_path != NULL && c_path[0] != '\0') {
    unlink(c_path);
  }
  if (tmp_dir != NULL && tmp_dir[0] != '\0') {
    rmdir(tmp_dir);
  }
}

// Emits only C code to stdout.
fn void aot_build_to_c(const char *argv0, const char *src_path, const char *src_text, const AotBuildCfg *cfg) {
  char runtime_path[PATH_MAX];
  aot_build_runtime_path(runtime_path, sizeof(runtime_path), argv0);
  aot_emit_stdout(runtime_path, src_path, src_text, cfg);
}

// Emits + compiles + runs once, then removes all temporary files.
fn int aot_build_as_c_once(const char *argv0, const char *src_path, const char *src_text, const AotBuildCfg *cfg) {
  int  rc = 1;
  char tmp_dir[PATH_MAX];
  char c_path[PATH_MAX];
  char x_path[PATH_MAX];

  aot_build_temp_dir(tmp_dir, sizeof(tmp_dir));
  aot_build_join(c_path, sizeof(c_path), tmp_dir, "main.c");
  aot_build_join(x_path, sizeof(x_path), tmp_dir, "main.bin");

  aot_write_c_file(c_path, argv0, src_path, src_text, cfg);
  rc = aot_build_compile(c_path, x_path);
  if (rc != 0) {
    fprintf(stderr, "ERROR: failed to compile AOT program '%s'\n", c_path);
    aot_build_cleanup(tmp_dir, c_path, x_path);
    return rc;
  }

  char *const run[] = {
    x_path,
    NULL,
  };

  rc = aot_build_spawn(run);
  aot_build_cleanup(tmp_dir, c_path, x_path);
  return rc;
}

// Emits + compiles a native executable to `out_path`.
fn int aot_build_to_output(const char *argv0, const char *src_path, const char *src_text, const char *out_path, const AotBuildCfg *cfg) {
  int  rc = 1;
  char tmp_dir[PATH_MAX];
  char c_path[PATH_MAX];

  aot_build_temp_dir(tmp_dir, sizeof(tmp_dir));
  aot_build_join(c_path, sizeof(c_path), tmp_dir, "main.c");
  aot_write_c_file(c_path, argv0, src_path, src_text, cfg);

  rc = aot_build_compile(c_path, out_path);
  if (rc != 0) {
    fprintf(stderr, "ERROR: failed to compile AOT executable '%s'\n", out_path);
  }

  aot_build_cleanup(tmp_dir, c_path, NULL);
  return rc;
}
