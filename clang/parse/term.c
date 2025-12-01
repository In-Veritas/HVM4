fn Term parse_term(PState *s, u32 depth);
fn void parse_def(PState *s);

fn Term parse_mat_body(PState *s, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) == '}') {
    parse_consume(s, "}");
    return term_era();
  }
  if (parse_peek(s) == '#') {
    parse_consume(s, "#");
    u32  nam = parse_name(s);
    parse_consume(s, ":");
    Term val = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ";");
    parse_skip(s);
    Term nxt = parse_mat_body(s, depth);
    return term_mat(nam, val, nxt);
  }
  Term val = parse_term(s, depth);
  parse_consume(s, "}");
  return val;
}

fn Term parse_lam(PState *s, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) == '{') {
    parse_consume(s, "{");
    return parse_mat_body(s, depth);
  }
  u32 nam = parse_name(s);
  parse_consume(s, ".");
  parse_bind_push(nam, depth, 0);
  u64  loc  = heap_alloc(1);
  Term body = parse_term(s, depth + 1);
  HEAP[loc] = body;
  parse_bind_pop();
  return term_new(0, LAM, depth, loc);
}

fn Term parse_dup(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  parse_consume(s, "&");
  u32  lab = parse_name(s);
  parse_consume(s, "=");
  Term val = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ";");
  parse_skip(s);
  parse_bind_push(nam, depth, lab);
  u64 loc       = heap_alloc(2);
  HEAP[loc + 0] = val;
  Term body     = parse_term(s, depth + 1);
  HEAP[loc + 1] = body;
  parse_bind_pop();
  return term_new(0, DUP, lab, loc);
}

fn Term parse_sup(PState *s, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) == '{') {
    parse_consume(s, "{");
    parse_consume(s, "}");
    return term_era();
  }
  u32 lab = parse_name(s);
  parse_consume(s, "{");
  Term tm0 = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ",");
  parse_skip(s);
  Term tm1 = parse_term(s, depth);
  parse_consume(s, "}");
  return term_sup(lab, tm0, tm1);
}

fn Term parse_ctr(PState *s, u32 depth) {
  u32  nam = parse_name(s);
  parse_consume(s, "{");
  Term args[16];
  u32  cnt = 0;
  parse_skip(s);
  if (parse_peek(s) != '}') {
    while (1) {
      args[cnt++] = parse_term(s, depth);
      parse_skip(s);
      if (parse_peek(s) == ',') {
        parse_consume(s, ",");
        continue;
      }
      break;
    }
  }
  parse_consume(s, "}");
  return term_ctr(nam, cnt, args);
}

fn Term parse_ref(PState *s) {
  return term_ref(parse_name(s));
}

fn Term parse_par(PState *s, u32 depth) {
  Term term = parse_term(s, depth);
  parse_consume(s, ")");
  return term;
}

fn Term parse_num(PState *s) {
  parse_skip(s);
  u32 n = 0;
  int has = 0;
  while (isdigit(parse_peek(s))) {
    has = 1;
    n = n * 10 + (u32)(parse_peek(s) - '0');
    parse_advance(s);
  }
  if (!has) {
    parse_error(s, "number", parse_peek(s));
  }
  parse_skip(s);
  return term_num(n);
}

fn Term parse_var(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  int idx;
  u32 lab;
  parse_bind_lookup(nam, depth, &idx, &lab);
  parse_skip(s);
  int side = parse_match(s, "₀") ? 0 : parse_match(s, "₁") ? 1 : -1;
  parse_skip(s);
  u32 val = (idx >= 0) ? (u32)idx : nam;
  u8  tag = (side == 0) ? CO0 : (side == 1) ? CO1 : VAR;
  return term_new(0, tag, lab, val);
}

fn Term parse_app(Term f, PState *s, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) != '(') {
    return f;
  }
  parse_consume(s, "(");
  if (parse_peek(s) == ')') {
    parse_consume(s, ")");
    return parse_app(f, s, depth);
  }
  while (1) {
    Term arg = parse_term(s, depth);
    f = term_app(f, arg);
    parse_skip(s);
    if (parse_peek(s) == ',') {
      parse_consume(s, ",");
      continue;
    }
    if (parse_peek(s) == ')') {
      parse_consume(s, ")");
      break;
    }
    parse_error(s, "',' or ')'", parse_peek(s));
  }
  return parse_app(f, s, depth);
}

fn Term parse_term(PState *s, u32 depth) {
  parse_skip(s);
  Term t;
  if (parse_match(s, "λ")) {
    t = parse_lam(s, depth);
  } else if (parse_match(s, "!")) {
    t = parse_dup(s, depth);
  } else if (parse_match(s, "&")) {
    t = parse_sup(s, depth);
  } else if (parse_match(s, "#")) {
    t = parse_ctr(s, depth);
  } else if (parse_match(s, "@")) {
    t = parse_ref(s);
  } else if (parse_match(s, "(")) {
    t = parse_par(s, depth);
  } else if (isdigit(parse_peek(s))) {
    t = parse_num(s);
  } else {
    t = parse_var(s, depth);
  }
  return parse_app(t, s, depth);
}

fn void parse_include(PState *s) {
  // Parse filename
  parse_skip(s);
  parse_consume(s, "\"");
  u32 start = s->pos;
  while (parse_peek(s) != '"' && !parse_at_end(s)) {
    parse_advance(s);
  }
  u32 len = s->pos - start;
  parse_consume(s, "\"");

  // Resolve path
  char filename[256], path[1024];
  memcpy(filename, s->src + start, len);
  filename[len] = 0;
  sys_path_join(path, sizeof(path), s->file, filename);

  // Check if already included
  for (u32 i = 0; i < PARSE_SEEN_FILES_LEN; i++) {
    if (strcmp(PARSE_SEEN_FILES[i], path) == 0) {
      return;
    }
  }
  if (PARSE_SEEN_FILES_LEN >= 1024) {
    sys_error("MAX_INCLUDES");
  }
  PARSE_SEEN_FILES[PARSE_SEEN_FILES_LEN++] = strdup(path);

  // Read and parse
  char *src = sys_file_read(path);
  if (!src) {
    fprintf(stderr, "Error: could not open '%s'\n", path);
    exit(1);
  }
  PState sub = {
    .file = PARSE_SEEN_FILES[PARSE_SEEN_FILES_LEN - 1],
    .src  = src,
    .pos  = 0,
    .len  = strlen(src),
    .line = 1,
    .col  = 1
  };
  parse_def(&sub);
  free(src);
}

fn void parse_def(PState *s) {
  parse_skip(s);
  if (parse_at_end(s)) {
    return;
  }
  if (parse_match(s, "#include")) {
    parse_include(s);
    parse_def(s);
    return;
  }
  if (parse_match(s, "@")) {
    u32 nam = parse_name(s) & EXT_MASK;
    parse_consume(s, "=");
    PARSE_BINDS_LEN = 0;
    Term val        = parse_term(s, 0);
    u64  loc        = heap_alloc(1);
    HEAP[loc]       = val;
    BOOK[nam]       = (u32)loc;
    parse_def(s);
    return;
  }
  parse_error(s, "definition or #include", parse_peek(s));
}
