// @@sup(lab, a, b)
// ---------------- prim-sup
// &lab{a, b}
fn Term prim_sup(Term lab, Term a, Term b) {
  lab = wnf(lab);
  switch (term_tag(lab)) {
    case ERA: {
      return term_new_era();
    }
    case SUP: {
      u32  sup_lab = term_ext(lab);
      Term lab0    = HEAP[term_val(lab) + 0];
      Term lab1    = HEAP[term_val(lab) + 1];
      Copy A       = term_clone(sup_lab, a);
      Copy B       = term_clone(sup_lab, b);
      Term s0      = term_new_pri(PRIM_SUP, 3, (Term[]){lab0, A.k0, B.k0});
      Term s1      = term_new_pri(PRIM_SUP, 3, (Term[]){lab1, A.k1, B.k1});
      return term_new_sup(sup_lab, s0, s1);
    }
    case NUM: {
      u32 lab_val = term_val(lab);
      return term_new_sup(lab_val, a, b);
    }
    default: {
      fprintf(stderr, "@@sup: expected NUM for label, got tag %u\n", term_tag(lab));
      exit(1);
    }
  }
}

