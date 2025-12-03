// @@dup(lab, val, fun)
// -------------------------- prim-dup
// !X &lab = val; fun(X₀,X₁)
fn Term prim_dup(Term lab, Term val, Term fun) {
  lab = wnf(lab);
  switch (term_tag(lab)) {
    case ERA: {
      return term_new_era();
    }
    case SUP: {
      u32  sup_lab = term_ext(lab);
      Term lab0    = HEAP[term_val(lab) + 0];
      Term lab1    = HEAP[term_val(lab) + 1];
      Copy V       = term_clone(sup_lab, val);
      Copy F       = term_clone(sup_lab, fun);
      Term s0      = term_new_pri(PRIM_DUP, 3, (Term[]){lab0, V.k0, F.k0});
      Term s1      = term_new_pri(PRIM_DUP, 3, (Term[]){lab1, V.k1, F.k1});
      return term_new_sup(sup_lab, s0, s1);
    }
    case NUM: {
      u32  lab_val = term_val(lab);
      u32  loc     = heap_alloc(2);
      Term x0      = term_new_co0(lab_val, loc);
      Term x1      = term_new_co1(lab_val, loc);
      Term bod     = term_new_app(term_new_app(fun, x0), x1);
      HEAP[loc + 0] = val;
      HEAP[loc + 1] = bod;
      return term_new(0, DUP, lab_val, loc);
    }
    default: {
      fprintf(stderr, "@@dup: expected NUM for label, got tag %u\n", term_tag(lab));
      exit(1);
    }
  }
}

