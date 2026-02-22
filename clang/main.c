// HVM4 CLI Entry Point
// ====================
//
// This file provides the command-line interface for the HVM4 runtime,
// mirroring the structure of main.hs for the Haskell implementation.
//
// Usage: ./main <file.hvm4> [options]
//   -s, --stats: Show statistics (interactions, time, performance)
//   -S, --silent: Silent output (omit term printing)
//   -D, --step-by-step: Step-by-step reduction trace
//   -d, --debug: Debug mode
//   -C[N], --collapse[=N]: Collapse and flatten (optional limit N)
//   -T<N>, --threads N: Use N threads (e.g. -T4, --threads=4)
//   --to-c: Emit standalone AOT C program to stdout
//   --as-c: Emit + compile + run standalone AOT executable once

#include "hvm4.c"

// CLI
// ===

typedef struct {
  int   stats;
  int   silent;
  int   do_collapse;
  int   collapse_limit;  // -1 means no limit
  int   debug;
  int   step_by_step;
  int   threads;
  int   help;
  int   as_c;
  int   to_c;
  u32            ffi_loads_len;
  RuntimeFfiLoad ffi_loads[RUNTIME_FFI_MAX];
  char *file;
} CliOpts;

// Returns the executable basename for help text.
fn const char *cli_prog_name(const char *argv0) {
  if (argv0 == NULL || argv0[0] == '\0') {
    return "hvm4";
  }

  const char *slash = strrchr(argv0, '/');
  if (slash == NULL) {
    return argv0;
  }

  return slash + 1;
}

// Returns 1 if `text` is a non-empty decimal unsigned integer.
fn int cli_is_uint(const char *text) {
  if (text == NULL || text[0] == '\0') {
    return 0;
  }

  for (u32 i = 0; text[i] != '\0'; i++) {
    if (text[i] < '0' || text[i] > '9') {
      return 0;
    }
  }

  return 1;
}

// Parses option values from either `--opt=value` or `--opt value`.
fn const char *cli_parse_opt_value(int argc, char **argv, int *idx, const char *opt, u32 prefix_len) {
  if (argv[*idx][prefix_len - 1] == '=') {
    return argv[*idx] + prefix_len;
  }

  if (*idx + 1 >= argc) {
    fprintf(stderr, "Error: missing value after %s\n", opt);
    exit(1);
  }

  return argv[++(*idx)];
}

// Prints one aligned row in the help options table.
fn void cli_print_help_opt(const char *sht, const char *lng, const char *desc) {
  char opt[64];

  if (sht == NULL || sht[0] == '\0') {
    snprintf(opt, sizeof(opt), "%s", lng);
  } else {
    snprintf(opt, sizeof(opt), "%s, %s", sht, lng);
  }

  fprintf(stdout, "  %-30s %s\n", opt, desc);
}

// Prints full command-line help.
fn void cli_print_help(const char *argv0) {
  const char *prog = cli_prog_name(argv0);

  fprintf(stdout, "HVM4 CLI\n");
  fprintf(stdout, "========\n\n");
  fprintf(stdout, "Usage:\n");
  fprintf(stdout, "  %s [options] <file.hvm4>\n", prog);
  fprintf(stdout, "  %s --help\n\n", prog);

  fprintf(stdout, "Options:\n");
  cli_print_help_opt("-h", "--help", "Show this help message and exit");
  cli_print_help_opt("-s", "--stats", "Show stats (interactions, time, perf)");
  cli_print_help_opt("-S", "--silent", "Suppress term output");
  cli_print_help_opt("-D", "--step-by-step", "Print each reduction step");
  cli_print_help_opt("-d", "--debug", "Enable debug mode");
  cli_print_help_opt("-C", "--collapse[=N]", "Collapse superpositions, optional limit N");
  cli_print_help_opt("-T", "--threads <N>", "Use N threads");
  cli_print_help_opt("", "--to-c", "Emit standalone AOT C to stdout");
  cli_print_help_opt("", "--as-c", "Compile and run standalone AOT executable once");
  cli_print_help_opt("", "--ffi <path>", "Load one FFI shared library before parsing");
  cli_print_help_opt("", "--ffi-dir <path>", "Load all FFI shared libraries in a directory");
  fprintf(stdout, "\nExamples:\n");
  fprintf(stdout, "  %s test/fib.hvm4 -s\n", prog);
  fprintf(stdout, "  %s test/fib.hvm4 --as-c\n", prog);
  fprintf(stdout, "  %s test/enum_nat.hvm4 --collapse=10\n", prog);
}

fn CliOpts parse_opts(int argc, char **argv) {
  CliOpts opts = {
    .stats = 0,
    .silent = 0,
    .do_collapse = 0,
    .collapse_limit = -1,
    .debug = 0,
    .step_by_step = 0,
    .threads = 0,
    .help = 0,
    .as_c = 0,
    .to_c = 0,
    .ffi_loads_len = 0,
    .file = NULL
  };

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      opts.help = 1;
    } else if (strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--stats") == 0) {
      opts.stats = 1;
    } else if (strcmp(argv[i], "-S") == 0 || strcmp(argv[i], "--silent") == 0) {
      opts.silent = 1;
    } else if (strcmp(argv[i], "-D") == 0 || strcmp(argv[i], "--step-by-step") == 0) {
      opts.step_by_step = 1;
    } else if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--debug") == 0) {
      opts.debug = 1;
    } else if (strncmp(argv[i], "-C", 2) == 0) {
      opts.do_collapse = 1;
      if (argv[i][2] != '\0') {
        opts.collapse_limit = atoi(&argv[i][2]);
      }
    } else if (strcmp(argv[i], "--collapse") == 0 || strncmp(argv[i], "--collapse=", 11) == 0) {
      opts.do_collapse = 1;
      if (argv[i][10] == '=') {
        const char *num = argv[i] + 11;
        if (!cli_is_uint(num)) {
          fprintf(stderr, "Error: invalid collapse limit '%s'\n", num);
          exit(1);
        }
        opts.collapse_limit = atoi(num);
      } else if (i + 1 < argc && cli_is_uint(argv[i + 1])) {
        opts.collapse_limit = atoi(argv[++i]);
      }
    } else if (strncmp(argv[i], "-T", 2) == 0) {
      const char *num = argv[i] + 2;
      if (num[0] == '\0') {
        if (i + 1 >= argc) {
          fprintf(stderr, "Error: missing thread count after -T\n");
          exit(1);
        }
        num = argv[++i];
      }
      opts.threads = atoi(num);
      if (opts.threads > MAX_THREADS) {
        fprintf(stderr, "Error: -T value (%d) exceeds MAX_THREADS (%d)\n", opts.threads, MAX_THREADS);
        exit(1);
      }
    } else if (strcmp(argv[i], "--threads") == 0 || strncmp(argv[i], "--threads=", 10) == 0) {
      const char *num = cli_parse_opt_value(argc, argv, &i, "--threads", 10);
      if (!cli_is_uint(num)) {
        fprintf(stderr, "Error: invalid thread count '%s'\n", num);
        exit(1);
      }
      opts.threads = atoi(num);
      if (opts.threads > MAX_THREADS) {
        fprintf(stderr, "Error: --threads value (%d) exceeds MAX_THREADS (%d)\n", opts.threads, MAX_THREADS);
        exit(1);
      }
    } else if (strcmp(argv[i], "--to-c") == 0) {
      opts.to_c = 1;
    } else if (strncmp(argv[i], "--to-c=", 7) == 0) {
      fprintf(stderr, "Error: --to-c does not take a path; it writes C to stdout\n");
      exit(1);
    } else if (strcmp(argv[i], "--as-c") == 0) {
      opts.as_c = 1;
    } else if (strcmp(argv[i], "--jit") == 0) {
      fprintf(stderr, "Error: --jit was renamed to --as-c\n");
      exit(1);
    } else if (strcmp(argv[i], "--compile") == 0 || strncmp(argv[i], "--compile=", 10) == 0 || strcmp(argv[i], "-o") == 0) {
      fprintf(stderr, "Error: --compile/-o was removed; use --as-c or --to-c\n");
      exit(1);
    } else if (strcmp(argv[i], "--ffi") == 0 || strncmp(argv[i], "--ffi=", 6) == 0) {
      const char *path = NULL;
      if (argv[i][5] == '=') {
        path = argv[i] + 6;
      } else {
        if (i + 1 >= argc) {
          fprintf(stderr, "Error: missing path after --ffi\n");
          exit(1);
        }
        path = argv[++i];
      }
      if (opts.ffi_loads_len >= RUNTIME_FFI_MAX) {
        fprintf(stderr, "Error: too many --ffi arguments (max %d)\n", RUNTIME_FFI_MAX);
        exit(1);
      }
      opts.ffi_loads[opts.ffi_loads_len++] = (RuntimeFfiLoad){.is_dir = 0, .path = path};
    } else if (strcmp(argv[i], "--ffi-dir") == 0 || strncmp(argv[i], "--ffi-dir=", 10) == 0) {
      const char *path = NULL;
      if (argv[i][9] == '=') {
        path = argv[i] + 10;
      } else {
        if (i + 1 >= argc) {
          fprintf(stderr, "Error: missing path after --ffi-dir\n");
          exit(1);
        }
        path = argv[++i];
      }
      if (opts.ffi_loads_len >= RUNTIME_FFI_MAX) {
        fprintf(stderr, "Error: too many FFI loads (max %d)\n", RUNTIME_FFI_MAX);
        exit(1);
      }
      opts.ffi_loads[opts.ffi_loads_len++] = (RuntimeFfiLoad){.is_dir = 1, .path = path};
    } else if (argv[i][0] != '-') {
      if (opts.file == NULL) {
        opts.file = argv[i];
      } else {
        fprintf(stderr, "Error: multiple input files specified\n");
        exit(1);
      }
    } else {
      fprintf(stderr, "Unknown option: %s\n", argv[i]);
      exit(1);
    }
  }

  return opts;
}

// Main
// ====

int main(int argc, char **argv) {
  if (argc <= 1) {
    cli_print_help(argv[0]);
    return 0;
  }

  // Parse command line
  CliOpts opts = parse_opts(argc, argv);

  if (opts.help) {
    cli_print_help(argv[0]);
    return 0;
  }

  if (opts.file == NULL) {
    fprintf(stderr, "Error: missing input file\n");
    fprintf(stderr, "Run '%s --help' for usage.\n", cli_prog_name(argv[0]));
    return 1;
  }

  if (opts.step_by_step && opts.do_collapse) {
    fprintf(stderr, "Error: -D is not supported with -C\n");
    return 1;
  }

  int build_modes = (opts.as_c ? 1 : 0) + (opts.to_c ? 1 : 0);
  if (build_modes > 1) {
    fprintf(stderr, "Error: choose only one build mode: --as-c or --to-c\n");
    return 1;
  }

  // Configure threads (default: 1)
  u32 threads = opts.threads > 0 ? (u32)opts.threads : 1;
  if (opts.step_by_step && threads > 1) {
    fprintf(stderr, "Warning: -D forces single-threaded mode\n");
    threads = 1;
  }
  runtime_init(threads, opts.debug, opts.silent, opts.step_by_step);

  // Load FFI libraries before parsing (needed for arity checks and overrides).
  int suppress_build_warnings = opts.as_c || opts.to_c;
  runtime_load_ffi(opts.ffi_loads, opts.ffi_loads_len, suppress_build_warnings);

  // Read and parse user file
  char *src = sys_file_read(opts.file);
  if (!src) {
    fprintf(stderr, "Error: could not open '%s'\n", opts.file);
    return 1;
  }

  // Parse and validate source, then resolve @main.
  char *abs_path = realpath(opts.file, NULL);
  const char *src_path = abs_path ? abs_path : opts.file;
  u32 main_id = 0;
  if (!runtime_prepare(&main_id, src_path, src)) {
    free(src);
    free(abs_path);
    runtime_free();
    return 1;
  }

  // Build-only modes (AOT emission/compilation) return early.
  AotBuildCfg aot_cfg = {
    .threads = threads,
    .debug   = opts.debug,
    .eval = {
      .do_collapse   = opts.do_collapse,
      .collapse_limit = opts.collapse_limit,
      .stats         = opts.stats,
      .silent        = opts.silent,
      .step_by_step  = opts.step_by_step,
    },
    .ffi_len = opts.ffi_loads_len,
  };
  for (u32 i = 0; i < opts.ffi_loads_len && i < RUNTIME_FFI_MAX; i++) {
    aot_cfg.ffi[i].is_dir = opts.ffi_loads[i].is_dir;
    aot_cfg.ffi[i].path   = opts.ffi_loads[i].path;
  }

  int build_done = 0;
  int build_rc   = 0;
  if (opts.to_c) {
    aot_build_to_c(argv[0], src_path, src, &aot_cfg);
    build_done = 1;
  } else if (opts.as_c) {
    build_rc = aot_build_as_c_once(argv[0], src_path, src, &aot_cfg);
    build_done = 1;
  }
  if (build_done) {
    free(src);
    free(abs_path);
    runtime_free();
    return build_rc;
  }

  free(src);

  // Evaluate and print stats using shared runtime behavior.
  RuntimeEvalCfg eval_cfg = {
    .do_collapse   = opts.do_collapse,
    .collapse_limit = opts.collapse_limit,
    .stats         = opts.stats,
    .silent        = opts.silent,
    .step_by_step  = opts.step_by_step,
  };
  runtime_eval_main(main_id, &eval_cfg);

  // Cleanup
  free(abs_path);
  runtime_free();

  return 0;
}
