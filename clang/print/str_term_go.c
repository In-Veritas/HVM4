fn void print_str_term_go(Term term, u32 depth);

fn void print_str_app(Term term, u32 depth) {
  Term spine[256];
  u32  len  = 0;
  Term curr = term;
  while (print_is_app(curr) && len < 256) {
    u32 loc = term_val(curr);
    spine[len++] = HEAP[loc + 1];
    curr = HEAP[loc];
  }
  if (term_tag(curr) == LAM) {
    print_str_putc('(');
    print_str_term_go(curr, depth);
    print_str_putc(')');
  } else {
    print_str_term_go(curr, depth);
  }
  print_str_putc('(');
  for (u32 i = 0; i < len; i++) {
    if (i > 0) {
      print_str_putc(',');
    }
    print_str_term_go(spine[len - 1 - i], depth);
  }
  print_str_putc(')');
}

fn void print_str_term_go(Term term, u32 depth) {
  switch (term_tag(term)) {
    case VAR: {
      print_str_name(term_val(term));
      break;
    }
    case NUM: {
      print_str_uint(term_val(term));
      break;
    }
    case REF: {
      print_str_putc('@');
      print_str_name(term_ext(term));
      break;
    }
    case ERA: {
      print_str_puts("&{}");
      break;
    }
    case CO0:
    case CO1: {
      print_str_name(term_val(term));
      print_str_puts(term_tag(term) == CO0 ? "₀" : "₁");
      break;
    }
    case LAM: {
      u32 loc = term_val(term);
      u32 nam = depth + 1;
      print_str_puts("λ");
      print_str_name(nam);
      print_str_putc('.');
      print_str_term_go(HEAP[loc], depth + 1);
      break;
    }
    case APP: {
      print_str_app(term, depth);
      break;
    }
    case SUP: {
      u32 loc = term_val(term);
      print_str_putc('&');
      print_str_name(term_ext(term));
      print_str_putc('{');
      print_str_term_go(HEAP[loc + 0], depth);
      print_str_putc(',');
      print_str_term_go(HEAP[loc + 1], depth);
      print_str_putc('}');
      break;
    }
    case DUP: {
      u32 loc = term_val(term);
      u32 nam = depth + 1;
      print_str_putc('!');
      print_str_name(nam);
      print_str_putc('&');
      print_str_name(term_ext(term));
      print_str_putc('=');
      print_str_term_go(HEAP[loc + 0], depth);
      print_str_putc(';');
      print_str_term_go(HEAP[loc + 1], depth + 1);
      break;
    }
    case MAT: {
      u32 loc = term_val(term);
      print_str_puts("λ{#");
      print_str_name(term_ext(term));
      print_str_putc(':');
      print_str_term_go(HEAP[loc + 0], depth);
      print_str_putc(';');
      print_str_term_go(HEAP[loc + 1], depth);
      print_str_putc('}');
      break;
    }
    case C00 ... C16: {
      u32 ari = term_tag(term) - C00;
      u32 loc = term_val(term);
      u32 nam = term_ext(term);
      // #VAR{#name{}} -> name
      if (nam == _VAR_ && ari == 1 && term_tag(HEAP[loc]) == C00) {
        print_str_name(term_ext(HEAP[loc]));
        break;
      }
      // #APP{f,x} -> f(x)
      if (nam == _APP_ && ari == 2) {
        print_str_app(term, depth);
        break;
      }
      print_str_putc('#');
      print_str_name(nam);
      print_str_putc('{');
      for (u32 i = 0; i < ari; i++) {
        if (i > 0) {
          print_str_putc(',');
        }
        print_str_term_go(HEAP[loc + i], depth);
      }
      print_str_putc('}');
      break;
    }
    case ALO: {
      print_str_puts("<ALO>");
      break;
    }
  }
}
