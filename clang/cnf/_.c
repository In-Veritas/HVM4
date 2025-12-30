// CNF (collapsed normal form) step.
// - cnf reduces to WNF, then lifts the first SUP to the top.
// - Output is either SUP/ERA/INC at the root with arbitrary fields, or a term
//   with no SUP/ERA/INC at any position.
// - RED keeps only RHS; ERA propagates upward.

fn Term cnf_at(Term term, u32 depth) {
  term = wnf(term);

  switch (term_tag(term)) {
    case ERA:
    case REF:
    case NUM:
    case NAM:
    case BJV:
    case BJM:
    case BJ0:
    case BJ1: {
      return term;
    }

    case SUP: {
      return term;
    }

    case INC: {
      return term;
    }

    case RED: {
      u64 loc = term_val(term);
      return cnf_at(heap_read(loc + 1), depth);
    }

    case LAM: {
      u64  lam_loc = term_val(term);
      Term body    = heap_read(lam_loc);
      u32  level   = depth + 1;
      heap_subst_var(lam_loc, term_new(0, BJV, 0, level));
      Term body_collapsed = cnf_at(body, level);
      u64  body_loc = heap_alloc(1);
      heap_set(body_loc, body_collapsed);
      Term lam = term_new(0, LAM, level, body_loc);

      u8 body_tag = term_tag(body_collapsed);
      if (body_tag == ERA) {
        return term_new_era();
      }

      if (body_tag == INC) {
        u32 inc_loc = term_val(body_collapsed);
        heap_set(body_loc, heap_read(inc_loc));
        return term_new_inc(lam);
      }

      if (body_tag != SUP) {
        return lam;
      }

      u32  lab     = term_ext(body_collapsed);
      u64  sup_loc = term_val(body_collapsed);
      Term sup_a   = heap_read(sup_loc + 0);
      Term sup_b   = heap_read(sup_loc + 1);

      u64 loc0 = heap_alloc(1);
      u64 loc1 = heap_alloc(1);
      heap_set(loc0, sup_a);
      heap_set(loc1, sup_b);

      Term lam0 = term_new(0, LAM, level, loc0);
      Term lam1 = term_new(0, LAM, level, loc1);

      return term_new_sup(lab, lam0, lam1);
    }

    case DUP:
    case MOV:
    case APP:
    case DRY:
    case MAT:
    case SWI:
    case USE:
    case OP2:
    case DSU:
    case DDU:
    case EQL:
    case AND:
    case OR:
    case UNS:
    case C00 ... C16: {
      u32 ari = term_arity(term);
      u32 loc = (u32)term_val(term);

      if (ari == 0) {
        return term;
      }

      int  sup_idx = -1;
      Term children[16];

      for (u32 i = 0; i < ari; i++) {
        Term child = heap_read(loc + i);
        children[i] = cnf_at(child, depth);
        if (children[i] != child) {
          heap_set(loc + i, children[i]);
        }

        if (term_tag(children[i]) == ERA) {
          return term_new_era();
        }

        if (sup_idx < 0 && term_tag(children[i]) == SUP) {
          sup_idx = i;
        }
      }

      if (sup_idx < 0) {
        return term;
      }

      Term sup     = children[sup_idx];
      u32  lab     = term_ext(sup);
      u64  sup_loc = term_val(sup);
      Term sup_a   = heap_read(sup_loc + 0);
      Term sup_b   = heap_read(sup_loc + 1);

      Term args0[16], args1[16];

      for (u32 i = 0; i < ari; i++) {
        if ((int)i == sup_idx) {
          args0[i] = sup_a;
          args1[i] = sup_b;
        } else {
          Copy c = term_clone(lab, children[i]);
          args0[i] = c.k0;
          args1[i] = c.k1;
        }
      }

      Term node0 = term_new_at(loc, term_tag(term), term_ext(term), ari, args0);
      Term node1 = term_new_(term_tag(term), term_ext(term), ari, args1);

      return term_new_sup(lab, node0, node1);
    }

    default: {
      return term;
    }
  }
}

fn Term cnf(Term term) {
  return cnf_at(term, 0);
}

fn Term cnf_inj(Term template, Term *args, u32 n_args) {
  if (n_args == 0) {
    return template;
  }

  Term head = wnf(args[0]);

  if (term_tag(head) == SUP) {
    u32  lab     = term_ext(head);
    u64  sup_loc = term_val(head);
    Term sup_a   = heap_read(sup_loc + 0);
    Term sup_b   = heap_read(sup_loc + 1);

    Copy T = term_clone(lab, template);
    Term args0[16], args1[16];
    args0[0] = sup_a;
    args1[0] = sup_b;
    term_clone_many(lab, args + 1, n_args - 1, args0 + 1, args1 + 1);

    Term r0 = cnf_inj(T.k0, args0, n_args);
    Term r1 = cnf_inj(T.k1, args1, n_args);

    return term_new_sup(lab, r0, r1);
  } else {
    Term applied = term_new_app(template, head);
    return cnf_inj(applied, args + 1, n_args - 1);
  }
}
