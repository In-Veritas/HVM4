typedef struct {
  u32 *lam_locs;
  u32 *lam_names;
  u32  lam_len;
  u32  lam_cap;
  u32 *dup_locs;
  u32 *dup_names;
  u32 *dup_labels;
  u32  dup_len;
  u32  dup_cap;
  u32  dup_print;
  u32  next_lam;
  u32  next_dup;
  u8   lam_use_ext;
} PrintState;

fn void print_term_go(FILE *f, Term term, u32 depth, u8 quoted, u32 subst, PrintState *st);

fn void print_term_at(FILE *f, Term term, u32 depth, u8 quoted, u32 subst, PrintState *st) {
  assert(!term_sub(term));
  print_term_go(f, term, depth, quoted, subst, st);
}

fn void print_alpha_name(FILE *f, u32 n, char base) {
  if (n == 0) {
    fputc('_', f);
    return;
  }
  char buf[32];
  u32  len = 0;
  while (n > 0) {
    n--;
    buf[len++] = (char)(base + (n % 26));
    n /= 26;
  }
  for (u32 i = 0; i < len; i++) {
    fputc(buf[len - 1 - i], f);
  }
}

fn void print_lam_name(FILE *f, PrintState *st, u32 name) {
  if (st->lam_use_ext) {
    print_name(f, name);
  } else {
    print_alpha_name(f, name, 'a');
  }
}

fn void print_dup_name(FILE *f, u32 name) {
  print_alpha_name(f, name, 'A');
}

fn void print_state_init(PrintState *st, u8 lam_use_ext) {
  st->lam_locs    = NULL;
  st->lam_names   = NULL;
  st->lam_len     = 0;
  st->lam_cap     = 0;
  st->dup_locs    = NULL;
  st->dup_names   = NULL;
  st->dup_labels  = NULL;
  st->dup_len     = 0;
  st->dup_cap     = 0;
  st->dup_print   = 0;
  st->next_lam    = 1;
  st->next_dup    = 1;
  st->lam_use_ext = lam_use_ext;
}

fn void print_state_free(PrintState *st) {
  free(st->lam_locs);
  free(st->lam_names);
  free(st->dup_locs);
  free(st->dup_names);
  free(st->dup_labels);
}

fn void print_state_grow_lam(PrintState *st) {
  u32 cap = st->lam_cap == 0 ? 16 : st->lam_cap * 2;
  st->lam_locs  = realloc(st->lam_locs, sizeof(u32) * cap);
  st->lam_names = realloc(st->lam_names, sizeof(u32) * cap);
  if (st->lam_locs == NULL || st->lam_names == NULL) {
    fprintf(stderr, "print_state: out of memory\n");
    exit(1);
  }
  st->lam_cap = cap;
}

fn void print_state_grow_dup(PrintState *st) {
  u32 cap = st->dup_cap == 0 ? 16 : st->dup_cap * 2;
  st->dup_locs   = realloc(st->dup_locs, sizeof(u32) * cap);
  st->dup_names  = realloc(st->dup_names, sizeof(u32) * cap);
  st->dup_labels = realloc(st->dup_labels, sizeof(u32) * cap);
  if (st->dup_locs == NULL || st->dup_names == NULL || st->dup_labels == NULL) {
    fprintf(stderr, "print_state: out of memory\n");
    exit(1);
  }
  st->dup_cap = cap;
}

fn u32 print_state_get_lam(PrintState *st, u32 loc, u32 hint) {
  for (u32 i = 0; i < st->lam_len; i++) {
    if (st->lam_locs[i] == loc) {
      return st->lam_names[i];
    }
  }
  if (st->lam_len == st->lam_cap) {
    print_state_grow_lam(st);
  }
  u32 name = 0;
  if (st->lam_use_ext && hint != 0) {
    name = hint;
  } else {
    name = st->next_lam++;
  }
  st->lam_locs[st->lam_len]  = loc;
  st->lam_names[st->lam_len] = name;
  st->lam_len++;
  return name;
}

fn u32 print_state_get_dup(PrintState *st, u32 loc, u32 lab) {
  for (u32 i = 0; i < st->dup_len; i++) {
    if (st->dup_locs[i] == loc) {
      return st->dup_names[i];
    }
  }
  if (st->dup_len == st->dup_cap) {
    print_state_grow_dup(st);
  }
  u32 name = st->next_dup++;
  st->dup_locs[st->dup_len]   = loc;
  st->dup_names[st->dup_len]  = name;
  st->dup_labels[st->dup_len] = lab;
  st->dup_len++;
  return name;
}

fn u32 alo_subst_get(u32 ls_loc, u32 idx) {
  u32 ls = ls_loc;
  for (u32 i = 0; i < idx && ls != 0; i++) {
    ls = (u32)(HEAP[ls] & 0xFFFFFFFF);
  }
  return ls != 0 ? (u32)(HEAP[ls] >> 32) : 0;
}

fn void print_mat_name(FILE *f, u32 nam) {
  if (nam == NAM_ZER) {
    fputs("0n", f);
  } else if (nam == NAM_SUC) {
    fputs("1n+", f);
  } else if (nam == NAM_NIL) {
    fputs("[]", f);
  } else if (nam == NAM_CON) {
    fputs("<>", f);
  } else {
    fputc('#', f);
    print_name(f, nam);
  }
}

// Prints APP and DRY chains as f(x,y,z)
fn void print_app(FILE *f, Term term, u32 depth, u8 quoted, u32 subst, PrintState *st) {
  Term spine[256];
  u32  len  = 0;
  Term curr = term;
  while ((term_tag(curr) == APP || term_tag(curr) == DRY) && len < 256) {
    u32 loc = term_val(curr);
    spine[len++] = HEAP[loc + 1];
    curr = HEAP[loc];
  }
  if (term_tag(curr) == LAM) {
    fputc('(', f);
    print_term_at(f, curr, depth, quoted, subst, st);
    fputc(')', f);
  } else {
    print_term_at(f, curr, depth, quoted, subst, st);
  }
  fputc('(', f);
  for (u32 i = 0; i < len; i++) {
    if (i > 0) {
      fputc(',', f);
    }
    print_term_at(f, spine[len - 1 - i], depth, quoted, subst, st);
  }
  fputc(')', f);
}

fn void print_ctr(FILE *f, Term t, u32 d, u8 quoted, u32 subst, PrintState *st) {
  u32 nam = term_ext(t), loc = term_val(t), ari = term_tag(t) - C00;
  // Nat: count SUCs, print as Nn or Nn+x
  if (nam == NAM_ZER || nam == NAM_SUC) {
    u32 n = 0;
    while (term_tag(t) == C01 && term_ext(t) == NAM_SUC) {
      n++;
      t = HEAP[term_val(t)];
    }
    fprintf(f, "%un", n);
    if (!(term_tag(t) == C00 && term_ext(t) == NAM_ZER)) {
      fputc('+', f);
      print_term_at(f, t, d, quoted, subst, st);
    }
    return;
  }
  // Char: 'x' or 'λ'
  if (nam == NAM_CHR && ari == 1 && term_tag(HEAP[loc]) == NUM) {
    u32 c = term_val(HEAP[loc]);
    if (c >= 32 && c != 127) {
      fputc('\'', f);
      print_utf8(f, c);
      fputc('\'', f);
      return;
    }
  }
  // List/String
  if (nam == NAM_NIL || nam == NAM_CON) {
    // Check if string (non-empty, all printable chars including Unicode)
    int is_str = (nam == NAM_CON);
    for (Term x = t; term_tag(x) == C02 && term_ext(x) == NAM_CON; x = HEAP[term_val(x) + 1]) {
      Term h = HEAP[term_val(x)];
      if (!(term_tag(h) == C01 && term_ext(h) == NAM_CHR)) {
        is_str = 0;
        break;
      }
      if (term_tag(HEAP[term_val(h)]) != NUM) {
        is_str = 0;
        break;
      }
      u32 c = term_val(HEAP[term_val(h)]);
      if (c < 32 || c == 127) {
        is_str = 0;
        break;
      }
    }
    Term end = t;
    while (term_tag(end) == C02 && term_ext(end) == NAM_CON) {
      end = HEAP[term_val(end) + 1];
    }
    if (is_str && term_tag(end) == C00 && term_ext(end) == NAM_NIL) {
      fputc('"', f);
      for (Term x = t; term_tag(x) == C02; x = HEAP[term_val(x) + 1]) {
        print_utf8(f, term_val(HEAP[term_val(HEAP[term_val(x)])]));
      }
      fputc('"', f);
      return;
    }
    // Proper list: [a,b,c]
    if (term_tag(end) == C00 && term_ext(end) == NAM_NIL) {
      fputc('[', f);
      for (Term x = t; term_tag(x) == C02; x = HEAP[term_val(x) + 1]) {
        if (x != t) {
          fputc(',', f);
        }
        print_term_at(f, HEAP[term_val(x)], d, quoted, subst, st);
      }
      fputc(']', f);
      return;
    }
    // Improper list: h<>t
    if (nam == NAM_CON) {
      print_term_at(f, HEAP[loc], d, quoted, subst, st);
      fputs("<>", f);
      print_term_at(f, HEAP[loc + 1], d, quoted, subst, st);
      return;
    }
  }
  // Default CTR
  fputc('#', f);
  print_name(f, nam);
  fputc('{', f);
  for (u32 i = 0; i < ari; i++) {
    if (i) {
      fputc(',', f);
    }
    print_term_at(f, HEAP[loc + i], d, quoted, subst, st);
  }
  fputc('}', f);
}

fn void print_term_go(FILE *f, Term term, u32 depth, u8 quoted, u32 subst, PrintState *st) {
  switch (term_tag(term)) {
    case NAM: {
      // Print stuck variable as just the name
      print_name(f, term_ext(term));
      break;
    }
    case DRY: {
      // Print stuck application as f(x,y)
      print_app(f, term, depth, quoted, subst, st);
      break;
    }
    case VAR: {
      if (quoted) {
        u32 idx  = term_val(term);
        u32 bind = 0;
        if (idx >= depth) {
          bind = alo_subst_get(subst, idx - depth);
        }
        if (bind != 0) {
          Term val = HEAP[bind];
          if (term_sub(val)) {
            val = term_unmark(val);
            print_term_at(f, val, depth, 0, 0, st);
          } else {
            print_term_at(f, term_new_var(bind), depth, 0, 0, st);
          }
        } else {
          u32 nam = depth > idx ? depth - idx : 0;
          print_alpha_name(f, nam, 'a');
        }
      } else {
        u32 loc = term_val(term);
        if (loc != 0 && term_sub(HEAP[loc])) {
          print_term_at(f, term_unmark(HEAP[loc]), depth, 0, 0, st);
        } else {
          u32 nam = print_state_get_lam(st, loc, 0);
          print_lam_name(f, st, nam);
        }
      }
      break;
    }
    case NUM: {
      fprintf(f, "%u", term_val(term));
      break;
    }
    case REF: {
      fputc('@', f);
      char *name = table_get(term_ext(term));
      if (name != NULL) {
        fputs(name, f);
      } else {
        print_name(f, term_ext(term));
      }
      break;
    }
    case ERA: {
      fputs("&{}", f);
      break;
    }
    case ANY: {
      fputc('*', f);
      break;
    }
    case CO0:
    case CO1: {
      if (quoted) {
        u32 idx  = term_val(term);
        u32 bind = 0;
        if (idx >= depth) {
          bind = alo_subst_get(subst, idx - depth);
        }
        if (bind != 0) {
          Term val = HEAP[bind];
          if (term_sub(val)) {
            val = term_unmark(val);
            print_term_at(f, val, depth, 0, 0, st);
          } else {
            u8  tag = term_tag(term);
            u32 lab = term_ext(term);
            print_term_at(f, term_new(0, tag, lab, bind), depth, 0, 0, st);
          }
        } else {
          u32 nam = depth > idx ? depth - idx : 0;
          if (nam == 0) {
            fputc('_', f);
          } else {
            print_alpha_name(f, nam, 'A');
          }
          fputs(term_tag(term) == CO0 ? "₀" : "₁", f);
        }
      } else {
        u32 loc = term_val(term);
        if (loc != 0 && term_sub(HEAP[loc])) {
          print_term_at(f, term_unmark(HEAP[loc]), depth, 0, 0, st);
        } else {
          u32 nam = print_state_get_dup(st, loc, term_ext(term));
          print_dup_name(f, nam);
          fputs(term_tag(term) == CO0 ? "₀" : "₁", f);
        }
      }
      break;
    }
    case LAM: {
      u32 loc = term_val(term);
      fputs("λ", f);
      if (quoted) {
        print_alpha_name(f, depth + 1, 'a');
        fputc('.', f);
        print_term_at(f, HEAP[loc], depth + 1, 1, subst, st);
      } else {
        u32 hint = 0;
        if (st->lam_use_ext) {
          hint = term_ext(term) + 1;
        }
        u32 nam = print_state_get_lam(st, loc, hint);
        print_lam_name(f, st, nam);
        fputc('.', f);
        print_term_at(f, HEAP[loc], depth + 1, 0, 0, st);
      }
      break;
    }
    case APP: {
      print_app(f, term, depth, quoted, subst, st);
      break;
    }
    case SUP: {
      u32 loc = term_val(term);
      fputc('&', f);
      print_name(f, term_ext(term));
      fputc('{', f);
      print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
      fputc(',', f);
      print_term_at(f, HEAP[loc + 1], depth, quoted, subst, st);
      fputc('}', f);
      break;
    }
    case DUP: {
      u32 loc = term_val(term);
      if (quoted) {
        fputc('!', f);
        print_alpha_name(f, depth + 1, 'A');
        fputc('&', f);
        print_name(f, term_ext(term));
        fputc('=', f);
        print_term_at(f, HEAP[loc + 0], depth, 1, subst, st);
        fputc(';', f);
        print_term_at(f, HEAP[loc + 1], depth + 1, 1, subst, st);
      } else {
        print_state_get_dup(st, loc, term_ext(term));
        print_term_at(f, HEAP[loc + 1], depth, 0, 0, st);
      }
      break;
    }
    case MAT:
    case SWI: {
      fputs("λ{", f);
      Term cur = term;
      while (term_tag(cur) == MAT || term_tag(cur) == SWI) {
        u32 loc = term_val(cur);
        if (term_tag(cur) == SWI) {
          fprintf(f, "%u", term_ext(cur));
        } else {
          print_mat_name(f, term_ext(cur));
        }
        fputc(':', f);
        print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
        Term next = HEAP[loc + 1];
        if (term_tag(next) == MAT || term_tag(next) == SWI) {
          fputc(';', f);
        }
        cur = next;
      }
      // Handle tail: NUM(0) = empty, USE = wrapped default, other = default
      if (term_tag(cur) == NUM && term_val(cur) == 0) {
        // empty default - just close
      } else if (term_tag(cur) == USE) {
        fputc(';', f);
        print_term_at(f, HEAP[term_val(cur)], depth, quoted, subst, st);
      } else {
        fputc(';', f);
        print_term_at(f, cur, depth, quoted, subst, st);
      }
      fputc('}', f);
      break;
    }
    case USE: {
      u32 loc = term_val(term);
      fputs("λ{", f);
      print_term_at(f, HEAP[loc], depth, quoted, subst, st);
      fputc('}', f);
      break;
    }
    case C00 ... C16: {
      print_ctr(f, term, depth, quoted, subst, st);
      break;
    }
    case OP2: {
      u32 opr = term_ext(term);
      u32 loc = term_val(term);
      static const char *op_syms[] = {
        "+", "-", "*", "/", "%", "&&", "||", "^", "<<", ">>",
        "~", "==", "!=", "<", "<=", ">", ">="
      };
      fputc('(', f);
      print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
      fputc(' ', f);
      if (opr < 17) {
        fputs(op_syms[opr], f);
      } else {
        fprintf(f, "?%u", opr);
      }
      fputc(' ', f);
      print_term_at(f, HEAP[loc + 1], depth, quoted, subst, st);
      fputc(')', f);
      break;
    }
    case DSU: {
      u32 loc = term_val(term);
      fputs("&(", f);
      print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
      fputs("){", f);
      print_term_at(f, HEAP[loc + 1], depth, quoted, subst, st);
      fputc(',', f);
      print_term_at(f, HEAP[loc + 2], depth, quoted, subst, st);
      fputc('}', f);
      break;
    }
    case DDU: {
      u32 loc = term_val(term);
      fputs("!(", f);
      print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
      fputs(")=", f);
      print_term_at(f, HEAP[loc + 1], depth, quoted, subst, st);
      fputc(';', f);
      print_term_at(f, HEAP[loc + 2], depth, quoted, subst, st);
      break;
    }
    case ALO: {
      u32 alo_loc = term_val(term);
      u64 pair    = HEAP[alo_loc];
      u32 tm_loc  = (u32)(pair & 0xFFFFFFFF);
      u32 ls_loc  = (u32)(pair >> 32);
      fputs("@{", f);
      print_term_at(f, HEAP[tm_loc], 0, 1, ls_loc, st);
      fputc('}', f);
      break;
    }
    case RED: {
      u32 loc = term_val(term);
      print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
      fputs(" ~> ", f);
      print_term_at(f, HEAP[loc + 1], depth, quoted, subst, st);
      break;
    }
    case EQL: {
      u32 loc = term_val(term);
      fputc('(', f);
      print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
      fputs(" === ", f);
      print_term_at(f, HEAP[loc + 1], depth, quoted, subst, st);
      fputc(')', f);
      break;
    }
    case AND: {
      u32 loc = term_val(term);
      fputc('(', f);
      print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
      fputs(" .&. ", f);
      print_term_at(f, HEAP[loc + 1], depth, quoted, subst, st);
      fputc(')', f);
      break;
    }
    case OR: {
      u32 loc = term_val(term);
      fputc('(', f);
      print_term_at(f, HEAP[loc + 0], depth, quoted, subst, st);
      fputs(" .|. ", f);
      print_term_at(f, HEAP[loc + 1], depth, quoted, subst, st);
      fputc(')', f);
      break;
    }
    case UNS: {
      u32 loc   = term_val(term);
      Term lamf = HEAP[loc];
      u32 locf  = term_val(lamf);
      Term lamv = HEAP[locf];
      u32 locv  = term_val(lamv);
      u32 hintf = st->lam_use_ext ? term_ext(lamf) + 1 : 0;
      u32 hintv = st->lam_use_ext ? term_ext(lamv) + 1 : 0;
      u32 namf  = print_state_get_lam(st, locf, hintf);
      u32 namv  = print_state_get_lam(st, locv, hintv);
      Term body = HEAP[locv];
      fputs("! ", f);
      print_lam_name(f, st, namf);
      fputs(" = λ ", f);
      print_lam_name(f, st, namv);
      fputs(" ; ", f);
      print_term_at(f, body, depth + 2, quoted, subst, st);
      break;
    }
    case INC: {
      u32 loc = term_val(term);
      fputs("↑", f);
      print_term_at(f, HEAP[loc], depth, quoted, subst, st);
      break;
    }
  }
}

fn void print_term_finish(FILE *f, PrintState *st) {
  while (st->dup_print < st->dup_len) {
    u32 idx = st->dup_print++;
    u32 loc = st->dup_locs[idx];
    u32 lab = st->dup_labels[idx];
    u32 nam = st->dup_names[idx];
    fputc('!', f);
    print_dup_name(f, nam);
    fputc('&', f);
    print_name(f, lab);
    fputc('=', f);
    Term val = HEAP[loc];
    if (term_sub(val)) {
      val = term_unmark(val);
    }
    print_term_at(f, val, 0, 0, 0, st);
    fputc(';', f);
  }
}

fn void print_term_ex(FILE *f, Term term, u8 lam_use_ext) {
  PrintState st;
  print_state_init(&st, lam_use_ext);
  print_term_at(f, term, 0, 0, 0, &st);
  print_term_finish(f, &st);
  print_state_free(&st);
}

fn void print_term(Term term) {
  print_term_ex(stdout, term, 0);
}

fn void print_term_quoted(Term term) {
  print_term_ex(stdout, term, 1);
}
