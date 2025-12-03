// Generic binary numeric operation handler
// Handles ERA/SUP cases, then applies the operation on NUM-NUM
fn Term prim_op2(u32 op, Term a, Term b) {
  a = wnf(a);
  switch (term_tag(a)) {
    case ERA: {
      return term_new_era();
    }
    case SUP: {
      u32  lab = term_ext(a);
      Term a0  = HEAP[term_val(a) + 0];
      Term a1  = HEAP[term_val(a) + 1];
      Copy B   = term_clone(lab, b);
      Term s0  = term_new_pri(op, 2, (Term[]){a0, B.k0});
      Term s1  = term_new_pri(op, 2, (Term[]){a1, B.k1});
      return term_new_sup(lab, s0, s1);
    }
    case NUM: {
      b = wnf(b);
      switch (term_tag(b)) {
        case ERA: {
          return term_new_era();
        }
        case SUP: {
          u32  lab = term_ext(b);
          Copy A   = term_clone(lab, term_new_num(term_val(a)));
          Term b0  = HEAP[term_val(b) + 0];
          Term b1  = HEAP[term_val(b) + 1];
          Term s0  = term_new_pri(op, 2, (Term[]){A.k0, b0});
          Term s1  = term_new_pri(op, 2, (Term[]){A.k1, b1});
          return term_new_sup(lab, s0, s1);
        }
        case NUM: {
          u32 av = term_val(a);
          u32 bv = term_val(b);
          u32 result;
          switch (op) {
            case PRIM_ADD: result = av + bv; break;
            case PRIM_SUB: result = av - bv; break;
            case PRIM_MUL: result = av * bv; break;
            case PRIM_DIV: result = bv == 0 ? 0 : av / bv; break;
            case PRIM_MOD: result = bv == 0 ? 0 : av % bv; break;
            case PRIM_AND: result = av & bv; break;
            case PRIM_OR:  result = av | bv; break;
            case PRIM_XOR: result = av ^ bv; break;
            case PRIM_LSH: result = av << (bv & 31); break;
            case PRIM_RSH: result = av >> (bv & 31); break;
            case PRIM_EQ:  result = av == bv ? 1 : 0; break;
            case PRIM_NE:  result = av != bv ? 1 : 0; break;
            case PRIM_LT:  result = av <  bv ? 1 : 0; break;
            case PRIM_LE:  result = av <= bv ? 1 : 0; break;
            case PRIM_GT:  result = av >  bv ? 1 : 0; break;
            case PRIM_GE:  result = av >= bv ? 1 : 0; break;
            default:
              fprintf(stderr, "prim_op2: unknown op %u\n", op);
              exit(1);
          }
          return term_new_num(result);
        }
        default: {
          fprintf(stderr, "prim_op2: expected NUM for second arg, got tag %u\n", term_tag(b));
          exit(1);
        }
      }
    }
    default: {
      fprintf(stderr, "prim_op2: expected NUM for first arg, got tag %u\n", term_tag(a));
      exit(1);
    }
  }
}
