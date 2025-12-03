// Generic unary numeric operation handler
// Handles ERA/SUP cases, then applies the operation on NUM
fn Term prim_op1(u32 op, Term a) {
  a = wnf(a);
  switch (term_tag(a)) {
    case ERA: {
      return term_new_era();
    }
    case SUP: {
      u32  lab = term_ext(a);
      Term a0  = HEAP[term_val(a) + 0];
      Term a1  = HEAP[term_val(a) + 1];
      Term s0  = term_new_pri(op, 1, (Term[]){a0});
      Term s1  = term_new_pri(op, 1, (Term[]){a1});
      return term_new_sup(lab, s0, s1);
    }
    case NUM: {
      u32 av = term_val(a);
      u32 result;
      switch (op) {
        case PRIM_NOT: result = ~av; break;
        default:
          fprintf(stderr, "prim_op1: unknown op %u\n", op);
          exit(1);
      }
      return term_new_num(result);
    }
    default: {
      fprintf(stderr, "prim_op1: expected NUM, got tag %u\n", term_tag(a));
      exit(1);
    }
  }
}
