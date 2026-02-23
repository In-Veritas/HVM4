// AOT Module: Program Emitter
// ---------------------------
// Emits standalone C with direct, monolithic per-definition compiled functions.
//
// Generated shape per definition:
// - F_<id>(stack, s_pos, base): WNF stack adapter
// - FH_<id>(argc, args, depth): hot Term path (monolithic nested control flow)
// - NH_<id>(argc, args, depth): numeric u32 path (monolithic nested control flow)
//
// There is no planner/state machine and no per-node helper function explosion.

fn char *table_get(u32 id);

// Emit Options
// ------------

// Controls whether emitted AOT code should include interaction counting calls.
static int AOT_EMIT_ITRS = 1;

// Returns 1 when this AOT build needs interaction counting.
fn int aot_emit_counting(const AotBuildCfg *cfg) {
  if (cfg == NULL) {
    return 1;
  }
  return cfg->eval.stats || cfg->eval.silent || cfg->eval.step_by_step;
}

// Emits one `aot_itrs_inc()` line if counting is enabled.
fn void aot_emit_itrs_inc(FILE *f, const char *pad) {
  if (!AOT_EMIT_ITRS) {
    return;
  }
  fprintf(f, "%saot_itrs_inc();\n", pad);
}

// Name Helpers
// ------------

// Builds a unique temporary identifier.
fn void aot_emit_tmp(char *out, u32 out_cap, const char *pre, u32 *next) {
  snprintf(out, out_cap, "%s_%u", pre, *next);
  *next = *next + 1;
}

// Builds one extra-indented padding string.
fn void aot_emit_pad_next(char *out, u32 out_cap, const char *pad) {
  snprintf(out, out_cap, "%s  ", pad);
}

// Escape Helpers
// --------------

// Writes one escaped byte as part of a C string literal.
fn void aot_emit_escaped_byte(FILE *f, u8 c) {
  switch (c) {
    case '\\': {
      fputs("\\\\", f);
      return;
    }
    case '"': {
      fputs("\\\"", f);
      return;
    }
    case '\n': {
      fputs("\\n", f);
      return;
    }
    case '\r': {
      fputs("\\r", f);
      return;
    }
    case '\t': {
      fputs("\\t", f);
      return;
    }
    default: {
      break;
    }
  }

  if (c >= 32 && c <= 126) {
    fputc((int)c, f);
    return;
  }

  fprintf(f, "\\%03o", (unsigned)c);
}

// Writes one C string literal token with escapes.
fn void aot_emit_c_string_token(FILE *f, const char *str) {
  fputc('"', f);
  for (u32 i = 0; str[i] != '\0'; i++) {
    aot_emit_escaped_byte(f, (u8)str[i]);
  }
  fputc('"', f);
}

// Writes one multi-line C string declaration from bytes.
fn void aot_emit_c_string_decl(FILE *f, const char *name, const char *text) {
  fprintf(f, "static const char *%s =\n", name);

  const u8 *ptr = (const u8 *)text;
  if (ptr[0] == '\0') {
    fprintf(f, "  \"\";\n\n");
    return;
  }

  u32 i = 0;
  while (ptr[i] != 0) {
    fprintf(f, "  \"");
    u32 chunk = 0;
    while (ptr[i] != 0 && chunk < 64) {
      aot_emit_escaped_byte(f, ptr[i]);
      i++;
      chunk++;
    }
    fprintf(f, "\"\n");
  }

  fprintf(f, "  ;\n\n");
}

// Deopt Emitters
// --------------

// Emits one hot deopt for unapplied remaining arguments.
fn void aot_emit_hot_fail_apply(FILE *f, u64 loc, u32 dep, const char *pad, const char *argc, const char *args, const char *i) {
  if (dep == 0) {
    fprintf(f, "%sreturn aot_hot_fail_apply_env(%lluULL, 0, NULL, %s, %s, %s);\n", pad, (unsigned long long)loc, argc, args, i);
    return;
  }

  fprintf(f, "%sreturn aot_hot_fail_apply_env(%lluULL, %u, env, %s, %s, %s);\n", pad, (unsigned long long)loc, dep, argc, args, i);
}

// Static APP-REF Collector
// ------------------------

// Collects APP arguments when `loc` is an APP-chain headed by REF.
// Returns 1 on success and stores args in call order.
fn int aot_emit_collect_app_ref(u64 loc, u16 *out_ref_id, u16 *out_arg_len, u64 out_args[AOT_HOT_ARG_CAP]) {
  u64 head_loc = loc;
  u64 rev[AOT_HOT_ARG_CAP];
  u16 rev_len = 0;

  for (;;) {
    Term head = heap_read(head_loc);
    if (term_tag(head) != APP) {
      break;
    }

    if (rev_len >= AOT_HOT_ARG_CAP) {
      return 0;
    }

    u64 app_loc = term_val(head);
    rev[rev_len] = app_loc + 1;
    rev_len++;
    head_loc = app_loc + 0;
  }

  Term head = heap_read(head_loc);
  if (term_tag(head) != REF) {
    return 0;
  }

  *out_ref_id  = term_ext(head);
  *out_arg_len = rev_len;

  for (u16 i = 0; i < rev_len; i++) {
    out_args[i] = rev[rev_len - 1 - i];
  }

  return 1;
}

// Hot Emitters
// ------------

// Forward declarations for recursive hot emitters.
fn void aot_emit_hot_apply(FILE *f, u32 def_id, u64 loc, u32 dep, const char *argc, const char *args, const char *i, const char *pad, u32 *tmp);

// Emits one source-map style comment for the strict WNF position.
fn void aot_emit_hot_wnf_comment(FILE *f, u64 loc, const char *pad) {
  fprintf(f, "%s// wnf ", pad);
  print_term_quoted_ex(f, heap_read(loc), 0);
  fprintf(f, "\n");
}

// Emits one apply evaluator fragment that returns from FH_<id>.
fn void aot_emit_hot_apply(FILE *f, u32 def_id, u64 loc, u32 dep, const char *argc, const char *args, const char *i, const char *pad, u32 *tmp) {
  Term term = heap_read(loc);
  u8   tag  = term_tag(term);

  char pad1[128];
  aot_emit_pad_next(pad1, sizeof(pad1), pad);
  aot_emit_hot_wnf_comment(f, loc, pad);

  switch (tag) {
    case LAM: {
      char i1[32];
      char x_name[32];
      aot_emit_tmp(i1, sizeof(i1), "i", tmp);
      snprintf(x_name, sizeof(x_name), "x%u", dep);

      if (dep >= AOT_HOT_ENV_CAP) {
        aot_emit_hot_fail_apply(f, loc, dep, pad, argc, args, i);
        return;
      }

      fprintf(f, "%sif (%s >= %s) {\n", pad, i, argc);
      aot_emit_hot_fail_apply(f, loc, dep, pad1, argc, args, i);
      fprintf(f, "%s}\n", pad);
      fprintf(f, "%sTerm %s = %s[%s];\n", pad, x_name, args, i);
      fprintf(f, "%senv[%u] = %s;\n", pad, dep, x_name);
      fprintf(f, "%su16 %s = %s + 1;\n", pad, i1, i);
      aot_emit_itrs_inc(f, pad);
      aot_emit_hot_apply(f, def_id, term_val(term), dep + 1, argc, args, i1, pad, tmp);
      return;
    }

    case SWI: {
      u64 mat_loc = term_val(term);
      char arg_name[32];
      char i1[32];
      aot_emit_tmp(arg_name, sizeof(arg_name), "arg", tmp);
      aot_emit_tmp(i1, sizeof(i1), "i", tmp);

      fprintf(f, "%sif (%s >= %s) {\n", pad, i, argc);
      aot_emit_hot_fail_apply(f, loc, dep, pad1, argc, args, i);
      fprintf(f, "%s}\n", pad);
      fprintf(f, "%sTerm %s = %s[%s];\n", pad, arg_name, args, i);
      fprintf(f, "%sif (term_tag(%s) != NUM) {\n", pad, arg_name);
      aot_emit_hot_fail_apply(f, loc, dep, pad1, argc, args, i);
      fprintf(f, "%s}\n", pad);
      fprintf(f, "%sif (term_val(%s) == %uULL) {\n", pad, arg_name, term_ext(term));
      fprintf(f, "%su16 %s = %s + 1;\n", pad1, i1, i);
      aot_emit_itrs_inc(f, pad1);
      aot_emit_hot_apply(f, def_id, mat_loc + 0, dep, argc, args, i1, pad1, tmp);
      fprintf(f, "%s}\n", pad);
      aot_emit_itrs_inc(f, pad);
      aot_emit_hot_apply(f, def_id, mat_loc + 1, dep, argc, args, i, pad, tmp);
      return;
    }

    case MAT: {
      u64 mat_loc = term_val(term);
      char arg_name[32];
      char tag_name[32];
      char i1[32];
      char ari[32];
      char rem[32];
      char hit_args[32];
      char hit_argc[32];
      char hit_i[32];
      char ctr_loc[32];
      aot_emit_tmp(arg_name, sizeof(arg_name), "arg", tmp);
      aot_emit_tmp(tag_name, sizeof(tag_name), "tag", tmp);
      aot_emit_tmp(i1, sizeof(i1), "i", tmp);
      aot_emit_tmp(ari, sizeof(ari), "ari", tmp);
      aot_emit_tmp(rem, sizeof(rem), "rem", tmp);
      aot_emit_tmp(hit_args, sizeof(hit_args), "hit_args", tmp);
      aot_emit_tmp(hit_argc, sizeof(hit_argc), "hit_argc", tmp);
      aot_emit_tmp(hit_i, sizeof(hit_i), "hit_i", tmp);
      aot_emit_tmp(ctr_loc, sizeof(ctr_loc), "ctr", tmp);

      fprintf(f, "%sif (%s >= %s) {\n", pad, i, argc);
      aot_emit_hot_fail_apply(f, loc, dep, pad1, argc, args, i);
      fprintf(f, "%s}\n", pad);
      fprintf(f, "%sTerm %s = %s[%s];\n", pad, arg_name, args, i);
      fprintf(f, "%su8 %s = term_tag(%s);\n", pad, tag_name, arg_name);
      fprintf(f, "%sif (%s < C00 || %s > C16) {\n", pad, tag_name, tag_name);
      aot_emit_hot_fail_apply(f, loc, dep, pad1, argc, args, i);
      fprintf(f, "%s}\n", pad);
      fprintf(f, "%sif (term_ext(%s) == %u) {\n", pad, arg_name, term_ext(term));
      aot_emit_itrs_inc(f, pad1);
      fprintf(f, "%su16 %s = %s + 1;\n", pad1, i1, i);
      fprintf(f, "%su32 %s = (u32)(%s - C00);\n", pad1, ari, tag_name);
      fprintf(f, "%su16 %s = %s - %s;\n", pad1, rem, argc, i1);
      fprintf(f, "%sif (%s + %s > AOT_HOT_ARG_CAP) {\n", pad1, ari, rem);
      aot_emit_hot_fail_apply(f, loc, dep, pad1, argc, args, i);
      fprintf(f, "%s}\n", pad1);
      fprintf(f, "%sTerm %s[AOT_HOT_ARG_CAP];\n", pad1, hit_args);
      fprintf(f, "%su64 %s = term_val(%s);\n", pad1, ctr_loc, arg_name);
      fprintf(f, "%sfor (u32 j = 0; j < %s; j++) {\n", pad1, ari);
      fprintf(f, "%s  %s[j] = heap_read(%s + j);\n", pad1, hit_args, ctr_loc);
      fprintf(f, "%s}\n", pad1);
      fprintf(f, "%sfor (u16 j = 0; j < %s; j++) {\n", pad1, rem);
      fprintf(f, "%s  %s[%s + j] = %s[%s + j];\n", pad1, hit_args, ari, args, i1);
      fprintf(f, "%s}\n", pad1);
      fprintf(f, "%su16 %s = (u16)(%s + %s);\n", pad1, hit_argc, ari, rem);
      fprintf(f, "%su16 %s = 0;\n", pad1, hit_i);
      aot_emit_hot_apply(f, def_id, mat_loc + 0, dep, hit_argc, hit_args, hit_i, pad1, tmp);
      fprintf(f, "%s}\n", pad);
      aot_emit_itrs_inc(f, pad);
      aot_emit_hot_apply(f, def_id, mat_loc + 1, dep, argc, args, i, pad, tmp);
      return;
    }

    default: {
      fprintf(f, "%sif (%s < %s) {\n", pad, i, argc);
      aot_emit_hot_fail_apply(f, loc, dep, pad1, argc, args, i);
      fprintf(f, "%s}\n", pad);
      fprintf(f, "%sreturn aot_hot_eval_loc_env(%lluULL, env, %u, AOT_HOT_ENV_CAP, depth);\n", pad, (unsigned long long)loc, dep);
      return;
    }
  }
}

// Numeric Emitters
// ----------------

// Forward declarations for recursive numeric emitters.
fn void aot_emit_num_expr(FILE *f, u32 def_id, u64 loc, u32 dep, const char *dst, const char *pad, u32 *tmp);
fn void aot_emit_num_apply(FILE *f, u32 def_id, u64 loc, u32 dep, const char *argc, const char *args, const char *i, const char *pad, u32 *tmp);

// Emits one numeric expression fragment that sets `dst`.
fn void aot_emit_num_expr(FILE *f, u32 def_id, u64 loc, u32 dep, const char *dst, const char *pad, u32 *tmp) {
  Term term = heap_read(loc);
  u8   tag  = term_tag(term);

  char pad1[128];
  aot_emit_pad_next(pad1, sizeof(pad1), pad);

  switch (tag) {
    case VAR:
    case BJV:
    case DP0:
    case BJ0:
    case DP1:
    case BJ1: {
      u64 lvl = term_val(term);
      if (lvl == 0 || lvl > dep) {
        fprintf(f, "%s%s = aot_num_fail();\n", pad, dst);
      } else {
        fprintf(f, "%s%s = aot_num_ok(x%llu);\n", pad, dst, (unsigned long long)(lvl - 1));
      }
      return;
    }

    case NUM: {
      fprintf(f, "%s%s = aot_num_ok(%uU);\n", pad, dst, (u32)term_val(term));
      return;
    }

    case OP2: {
      u64 op2_loc = term_val(term);
      char lhs[32];
      char rhs[32];
      aot_emit_tmp(lhs, sizeof(lhs), "lhs", tmp);
      aot_emit_tmp(rhs, sizeof(rhs), "rhs", tmp);

      fprintf(f, "%sdo {\n", pad);
      fprintf(f, "%sAotNumRes %s;\n", pad1, lhs);
      aot_emit_num_expr(f, def_id, op2_loc + 0, dep, lhs, pad1, tmp);
      fprintf(f, "%sif (!%s.ok) {\n", pad1, lhs);
      fprintf(f, "%s%s = aot_num_fail();\n", pad1, dst);
      fprintf(f, "%sbreak;\n", pad1);
      fprintf(f, "%s}\n", pad1);
      fprintf(f, "%sAotNumRes %s;\n", pad1, rhs);
      aot_emit_num_expr(f, def_id, op2_loc + 1, dep, rhs, pad1, tmp);
      fprintf(f, "%sif (!%s.ok) {\n", pad1, rhs);
      fprintf(f, "%s%s = aot_num_fail();\n", pad1, dst);
      fprintf(f, "%sbreak;\n", pad1);
      fprintf(f, "%s}\n", pad1);
      aot_emit_itrs_inc(f, pad1);
      switch (term_ext(term)) {
        case OP_ADD: fprintf(f, "%s%s = aot_num_ok(%s.val + %s.val);\n", pad1, dst, lhs, rhs); break;
        case OP_SUB: fprintf(f, "%s%s = aot_num_ok(%s.val - %s.val);\n", pad1, dst, lhs, rhs); break;
        case OP_MUL: fprintf(f, "%s%s = aot_num_ok(%s.val * %s.val);\n", pad1, dst, lhs, rhs); break;
        case OP_DIV: fprintf(f, "%s%s = aot_num_ok(%s.val != 0 ? %s.val / %s.val : 0U);\n", pad1, dst, rhs, lhs, rhs); break;
        case OP_MOD: fprintf(f, "%s%s = aot_num_ok(%s.val != 0 ? %s.val %% %s.val : 0U);\n", pad1, dst, rhs, lhs, rhs); break;
        case OP_AND: fprintf(f, "%s%s = aot_num_ok(%s.val & %s.val);\n", pad1, dst, lhs, rhs); break;
        case OP_OR:  fprintf(f, "%s%s = aot_num_ok(%s.val | %s.val);\n", pad1, dst, lhs, rhs); break;
        case OP_XOR: fprintf(f, "%s%s = aot_num_ok(%s.val ^ %s.val);\n", pad1, dst, lhs, rhs); break;
        case OP_LSH: fprintf(f, "%s%s = aot_num_ok(%s.val << %s.val);\n", pad1, dst, lhs, rhs); break;
        case OP_RSH: fprintf(f, "%s%s = aot_num_ok(%s.val >> %s.val);\n", pad1, dst, lhs, rhs); break;
        case OP_NOT: fprintf(f, "%s%s = aot_num_ok(~%s.val);\n", pad1, dst, rhs); break;
        case OP_EQ:  fprintf(f, "%s%s = aot_num_ok(%s.val == %s.val ? 1U : 0U);\n", pad1, dst, lhs, rhs); break;
        case OP_NE:  fprintf(f, "%s%s = aot_num_ok(%s.val != %s.val ? 1U : 0U);\n", pad1, dst, lhs, rhs); break;
        case OP_LT:  fprintf(f, "%s%s = aot_num_ok(%s.val < %s.val ? 1U : 0U);\n", pad1, dst, lhs, rhs); break;
        case OP_LE:  fprintf(f, "%s%s = aot_num_ok(%s.val <= %s.val ? 1U : 0U);\n", pad1, dst, lhs, rhs); break;
        case OP_GT:  fprintf(f, "%s%s = aot_num_ok(%s.val > %s.val ? 1U : 0U);\n", pad1, dst, lhs, rhs); break;
        case OP_GE:  fprintf(f, "%s%s = aot_num_ok(%s.val >= %s.val ? 1U : 0U);\n", pad1, dst, lhs, rhs); break;
        default:     fprintf(f, "%s%s = aot_num_fail();\n", pad1, dst); break;
      }
      fprintf(f, "%s} while (0);\n", pad);
      return;
    }

    case DUP: {
      u64 dup_loc = term_val(term);
      char val[32];
      aot_emit_tmp(val, sizeof(val), "val", tmp);

      fprintf(f, "%sdo {\n", pad);
      fprintf(f, "%sAotNumRes %s;\n", pad1, val);
      aot_emit_num_expr(f, def_id, dup_loc + 0, dep, val, pad1, tmp);
      fprintf(f, "%sif (!%s.ok) {\n", pad1, val);
      fprintf(f, "%s%s = aot_num_fail();\n", pad1, dst);
      fprintf(f, "%sbreak;\n", pad1);
      fprintf(f, "%s}\n", pad1);
      if (dep >= AOT_HOT_ENV_CAP) {
        fprintf(f, "%s%s = aot_num_fail();\n", pad1, dst);
        fprintf(f, "%sbreak;\n", pad1);
        fprintf(f, "%s} while (0);\n", pad);
        return;
      }
      fprintf(f, "%su32 x%u = %s.val;\n", pad1, dep, val);
      aot_emit_itrs_inc(f, pad1);
      aot_emit_num_expr(f, def_id, dup_loc + 1, dep + 1, dst, pad1, tmp);
      fprintf(f, "%s} while (0);\n", pad);
      return;
    }

    case APP: {
      u16 ref_id = 0;
      u16 arg_len = 0;
      u64 arg_locs[AOT_HOT_ARG_CAP];

      if (!aot_emit_collect_app_ref(loc, &ref_id, &arg_len, arg_locs)) {
        fprintf(f, "%s%s = aot_num_fail();\n", pad, dst);
        return;
      }

      u16 cap = arg_len == 0 ? 1 : arg_len;
      char call_args[32];
      char ok_name[32];
      aot_emit_tmp(call_args, sizeof(call_args), "call_args", tmp);
      aot_emit_tmp(ok_name, sizeof(ok_name), "ok", tmp);

      fprintf(f, "%sdo {\n", pad);
      fprintf(f, "%su32 %s[%u];\n", pad1, call_args, cap);
      fprintf(f, "%su8 %s = 1;\n", pad1, ok_name);

      for (u16 i_arg = 0; i_arg < arg_len; i_arg++) {
        char arg_name[32];
        aot_emit_tmp(arg_name, sizeof(arg_name), "arg", tmp);
        fprintf(f, "%sAotNumRes %s;\n", pad1, arg_name);
        aot_emit_num_expr(f, def_id, arg_locs[i_arg], dep, arg_name, pad1, tmp);
        fprintf(f, "%sif (!%s.ok) {\n", pad1, arg_name);
        fprintf(f, "%s%s = aot_num_fail();\n", pad1, dst);
        fprintf(f, "%s%s = 0;\n", pad1, ok_name);
        fprintf(f, "%sbreak;\n", pad1);
        fprintf(f, "%s}\n", pad1);
        fprintf(f, "%s%s[%u] = %s.val;\n", pad1, call_args, i_arg, arg_name);
      }

      fprintf(f, "%sif (!%s) {\n", pad1, ok_name);
      fprintf(f, "%sbreak;\n", pad1);
      fprintf(f, "%s}\n", pad1);
      if (ref_id == def_id) {
        fprintf(f, "%s%s = NH_%u(%u, %s, depth + 1);\n", pad1, dst, def_id, arg_len, call_args);
      } else {
        fprintf(f, "%s%s = aot_num_apply_ref(%u, %u, %s, depth + 1);\n", pad1, dst, ref_id, arg_len, call_args);
      }
      fprintf(f, "%s} while (0);\n", pad);
      return;
    }

    default: {
      fprintf(f, "%s%s = aot_num_fail();\n", pad, dst);
      return;
    }
  }
}

// Emits one numeric apply fragment that returns from NH_<id>.
fn void aot_emit_num_apply(FILE *f, u32 def_id, u64 loc, u32 dep, const char *argc, const char *args, const char *i, const char *pad, u32 *tmp) {
  Term term = heap_read(loc);
  u8   tag  = term_tag(term);

  char pad1[128];
  aot_emit_pad_next(pad1, sizeof(pad1), pad);
  aot_emit_hot_wnf_comment(f, loc, pad);

  switch (tag) {
    case LAM: {
      if (dep >= AOT_HOT_ENV_CAP) {
        fprintf(f, "%sreturn aot_num_fail();\n", pad);
        return;
      }

      char i1[32];
      char x_name[32];
      aot_emit_tmp(i1, sizeof(i1), "i", tmp);
      snprintf(x_name, sizeof(x_name), "x%u", dep);

      fprintf(f, "%sif (%s >= %s) {\n", pad, i, argc);
      fprintf(f, "%sreturn aot_num_fail();\n", pad1);
      fprintf(f, "%s}\n", pad);
      fprintf(f, "%su32 %s = %s[%s];\n", pad, x_name, args, i);
      fprintf(f, "%su16 %s = %s + 1;\n", pad, i1, i);
      aot_emit_itrs_inc(f, pad);
      aot_emit_num_apply(f, def_id, term_val(term), dep + 1, argc, args, i1, pad, tmp);
      return;
    }

    case SWI: {
      u64 mat_loc = term_val(term);
      char i1[32];
      aot_emit_tmp(i1, sizeof(i1), "i", tmp);

      fprintf(f, "%sif (%s >= %s) {\n", pad, i, argc);
      fprintf(f, "%sreturn aot_num_fail();\n", pad1);
      fprintf(f, "%s}\n", pad);
      fprintf(f, "%sif (%s[%s] == %uU) {\n", pad, args, i, term_ext(term));
      fprintf(f, "%su16 %s = %s + 1;\n", pad1, i1, i);
      aot_emit_itrs_inc(f, pad1);
      aot_emit_num_apply(f, def_id, mat_loc + 0, dep, argc, args, i1, pad1, tmp);
      fprintf(f, "%s}\n", pad);
      aot_emit_itrs_inc(f, pad);
      aot_emit_num_apply(f, def_id, mat_loc + 1, dep, argc, args, i, pad, tmp);
      return;
    }

    default: {
      fprintf(f, "%sif (%s < %s) {\n", pad, i, argc);
      fprintf(f, "%sreturn aot_num_fail();\n", pad1);
      fprintf(f, "%s}\n", pad);
      char out_name[32];
      aot_emit_tmp(out_name, sizeof(out_name), "out", tmp);
      fprintf(f, "%sAotNumRes %s;\n", pad, out_name);
      aot_emit_num_expr(f, def_id, loc, dep, out_name, pad, tmp);
      fprintf(f, "%sreturn %s;\n", pad, out_name);
      return;
    }
  }
}

// Definition Emitter
// ------------------

// Emits one compiled definition.
fn void aot_emit_def(FILE *f, u32 id) {
  if (BOOK[id] == 0) {
    return;
  }

  char *name = table_get(id);
  if (name == NULL) {
    return;
  }

  u64 root = BOOK[id];

  fprintf(f, "// Compiled hot/numeric paths for @%s (id %u).\n", name, id);
  fprintf(f, "static AotHotRes FH_%u(u16 argc, const Term *args, u32 depth);\n", id);
  fprintf(f, "static AotNumRes NH_%u(u16 argc, const u32 *args, u32 depth);\n\n", id);

  fprintf(f, "static AotNumRes NH_%u(u16 argc, const u32 *args, u32 depth) {\n", id);
  fprintf(f, "  if (depth >= AOT_HOT_MAX_DEPTH) {\n");
  fprintf(f, "    return aot_num_fail();\n");
  fprintf(f, "  }\n");
  fprintf(f, "  u16 i = 0;\n");
  {
    u32 tmp = 0;
    aot_emit_num_apply(f, id, root, 0, "argc", "args", "i", "  ", &tmp);
  }
  fprintf(f, "}\n\n");

  fprintf(f, "static AotHotRes FH_%u(u16 argc, const Term *args, u32 depth) {\n", id);
  fprintf(f, "  if (depth >= AOT_HOT_MAX_DEPTH) {\n");
  fprintf(f, "    Term call = aot_hot_reapply(term_new_ref(%u), argc, args, 0);\n", id);
  fprintf(f, "    return aot_hot_fail(call);\n");
  fprintf(f, "  }\n");
  fprintf(f, "\n");
  fprintf(f, "  if (argc <= AOT_HOT_ARG_CAP) {\n");
  fprintf(f, "    u32 num_args[AOT_HOT_ARG_CAP];\n");
  fprintf(f, "    u16 k = 0;\n");
  fprintf(f, "    for (; k < argc; k++) {\n");
  fprintf(f, "      if (term_tag(args[k]) != NUM) {\n");
  fprintf(f, "        break;\n");
  fprintf(f, "      }\n");
  fprintf(f, "      num_args[k] = (u32)term_val(args[k]);\n");
  fprintf(f, "    }\n");
  fprintf(f, "    if (k == argc) {\n");
  fprintf(f, "      AotNumRes num = NH_%u(argc, num_args, depth);\n", id);
  fprintf(f, "      if (num.ok) {\n");
  fprintf(f, "        return aot_hot_ok(term_new_num(num.val));\n");
  fprintf(f, "      }\n");
  fprintf(f, "    }\n");
  fprintf(f, "  }\n");
  fprintf(f, "\n");
  fprintf(f, "  Term env[AOT_HOT_ENV_CAP];\n");
  fprintf(f, "  u16 i = 0;\n");
  {
    u32 tmp = 0;
    aot_emit_hot_apply(f, id, root, 0, "argc", "args", "i", "  ", &tmp);
  }
  fprintf(f, "}\n\n");

  fprintf(f, "// Compiled stack-entry path for @%s (id %u).\n", name, id);
  fprintf(f, "static Term F_%u(Term *stack, u32 *s_pos, u32 base) {\n", id);
  fprintf(f, "  u32 t_pos = *s_pos;\n");
  fprintf(f, "  u16 argc = 0;\n");
  fprintf(f, "  while (t_pos > base) {\n");
  fprintf(f, "    Term frame = stack[t_pos - 1];\n");
  fprintf(f, "    if (term_tag(frame) != APP) {\n");
  fprintf(f, "      break;\n");
  fprintf(f, "    }\n");
  fprintf(f, "    if (argc >= AOT_HOT_ARG_CAP) {\n");
  fprintf(f, "      return aot_fallback_alo(%lluULL, 0, NULL);\n", (unsigned long long)root);
  fprintf(f, "    }\n");
  fprintf(f, "    argc++;\n");
  fprintf(f, "    t_pos--;\n");
  fprintf(f, "  }\n");
  fprintf(f, "\n");
  fprintf(f, "  Term args[AOT_HOT_ARG_CAP];\n");
  fprintf(f, "  u16 k = 0;\n");
  fprintf(f, "  while (*s_pos > t_pos) {\n");
  fprintf(f, "    Term frame = stack[*s_pos - 1];\n");
  fprintf(f, "    (*s_pos)--;\n");
  fprintf(f, "    u64 app_loc = term_val(frame);\n");
  fprintf(f, "    args[k] = heap_read(app_loc + 1);\n");
  fprintf(f, "    k++;\n");
  fprintf(f, "  }\n");
  fprintf(f, "\n");
  fprintf(f, "  AotHotRes out = FH_%u(argc, args, 0);\n", id);
  fprintf(f, "  return out.term;\n");
  fprintf(f, "}\n\n");
}

// Registration Emitter
// --------------------

// Emits registration for all compiled definitions.
fn void aot_emit_register(FILE *f) {
  fprintf(f, "// Registers generated fast paths into the runtime tables.\n");
  fprintf(f, "static void aot_register_generated(void) {\n");
  for (u32 id = 0; id < TABLE.len; id++) {
    if (BOOK[id] == 0) {
      continue;
    }

    char *name = table_get(id);
    if (name == NULL) {
      continue;
    }

    fprintf(f, "  // @%s\n", name);
    fprintf(f, "  AOT_FNS[%u] = F_%u;\n", id, id);
    fprintf(f, "  AOT_HOT_FNS[%u] = FH_%u;\n", id, id);
    fprintf(f, "  AOT_NUM_FNS[%u] = NH_%u;\n", id, id);
  }
  fprintf(f, "}\n\n");
}

// Emits the static FFI-load table used by standalone AOT programs.
fn void aot_emit_ffi_table(FILE *f, const AotBuildCfg *cfg) {
  u32 ffi_len = cfg ? cfg->ffi_len : 0;
  u32 ffi_cap = ffi_len == 0 ? 1 : ffi_len;

  fprintf(f, "static const RuntimeFfiLoad AOT_FFI_LOADS[%u] = {\n", ffi_cap);
  if (ffi_len == 0) {
    fprintf(f, "  { .is_dir = 0, .path = NULL },\n");
  } else {
    for (u32 i = 0; i < ffi_len; i++) {
      fprintf(f, "  { .is_dir = %d, .path = ", cfg->ffi[i].is_dir);
      aot_emit_c_string_token(f, cfg->ffi[i].path ? cfg->ffi[i].path : "");
      fprintf(f, " },\n");
    }
  }
  fprintf(f, "};\n");
  fprintf(f, "static const u32 AOT_FFI_LEN = %u;\n\n", ffi_len);
}

// Emits standalone entrypoint using shared runtime helper functions.
fn void aot_emit_entry_main(FILE *f, const AotBuildCfg *cfg) {
  u32 threads = (cfg && cfg->threads > 0) ? cfg->threads : 1;
  int debug   = cfg ? cfg->debug : 0;

  RuntimeEvalCfg eval_cfg = {
    .do_collapse    = 0,
    .collapse_limit = -1,
    .stats          = 0,
    .silent         = 0,
    .step_by_step   = 0,
  };

  if (cfg != NULL) {
    eval_cfg = cfg->eval;
  }

  fprintf(f, "int main(void) {\n");
  fprintf(f, "  runtime_init(%u, %d, %d, %d);\n", threads, debug, eval_cfg.silent, eval_cfg.step_by_step);
  fprintf(f, "  runtime_load_ffi(AOT_FFI_LOADS, AOT_FFI_LEN, 0);\n");
  fprintf(f, "\n");
  fprintf(f, "  u32 main_id = 0;\n");
  fprintf(f, "  if (!runtime_prepare_text(&main_id, AOT_SOURCE_PATH, AOT_SOURCE_TEXT)) {\n");
  fprintf(f, "    runtime_free();\n");
  fprintf(f, "    return 1;\n");
  fprintf(f, "  }\n");
  fprintf(f, "\n");
  fprintf(f, "  aot_register_generated();\n");
  fprintf(f, "  RuntimeEvalCfg eval = {\n");
  fprintf(f, "    .do_collapse = %d,\n", eval_cfg.do_collapse);
  fprintf(f, "    .collapse_limit = %d,\n", eval_cfg.collapse_limit);
  fprintf(f, "    .stats = %d,\n", eval_cfg.stats);
  fprintf(f, "    .silent = %d,\n", eval_cfg.silent);
  fprintf(f, "    .step_by_step = %d,\n", eval_cfg.step_by_step);
  fprintf(f, "  };\n");
  fprintf(f, "  runtime_eval_main(main_id, &eval);\n");
  fprintf(f, "  runtime_free();\n");
  fprintf(f, "  return 0;\n");
  fprintf(f, "}\n");
}

// Program Emitter
// ---------------

// Emits the full standalone AOT C program.
fn void aot_emit_to_file(FILE *f, const char *runtime_path, const char *src_path, const char *src_text, const AotBuildCfg *cfg) {
  AOT_EMIT_ITRS = aot_emit_counting(cfg);

  fprintf(f, "// Auto-generated by HVM AOT.\n");
  fprintf(f, "// This file is standalone: compile with `clang -O2 -o <out> <file.c>`.\n");
  fprintf(f, "//\n");
  fprintf(f, "// AOT summary:\n");
  fprintf(f, "// - Includes full runtime TU directly (%s).\n", runtime_path);
  fprintf(f, "// - Emits monolithic per-definition compiled functions.\n");
  fprintf(f, "// - Uses lexical binder registers x0, x1, ...\n");
  fprintf(f, "// - Deopts via linear-safe residual terms (ALO + concrete progress).\n\n");

  fprintf(f, "#include ");
  aot_emit_c_string_token(f, runtime_path);
  fprintf(f, "\n\n");

  aot_emit_c_string_decl(f, "AOT_SOURCE_PATH", src_path);
  aot_emit_c_string_decl(f, "AOT_SOURCE_TEXT", src_text);
  aot_emit_ffi_table(f, cfg);

  for (u32 id = 0; id < TABLE.len; id++) {
    if (BOOK[id] == 0) {
      continue;
    }
    aot_emit_def(f, id);
  }

  aot_emit_register(f);
  aot_emit_entry_main(f, cfg);
}

// Emits the full standalone AOT C program to a file path.
fn void aot_emit(const char *c_path, const char *runtime_path, const char *src_path, const char *src_text, const AotBuildCfg *cfg) {
  FILE *f = fopen(c_path, "w");
  if (f == NULL) {
    fprintf(stderr, "ERROR: failed to open AOT output '%s'\n", c_path);
    exit(1);
  }

  aot_emit_to_file(f, runtime_path, src_path, src_text, cfg);
  fclose(f);
}

// Emits the full standalone AOT C program to stdout.
fn void aot_emit_stdout(const char *runtime_path, const char *src_path, const char *src_text, const AotBuildCfg *cfg) {
  aot_emit_to_file(stdout, runtime_path, src_path, src_text, cfg);
}
