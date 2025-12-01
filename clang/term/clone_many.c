fn void term_clone_many(u32 lab, Term *src, u32 n, Term *dst0, Term *dst1) {
  for (u32 i = 0; i < n; i++) {
    Copy c  = term_clone(lab, src[i]);
    dst0[i] = c.k0;
    dst1[i] = c.k1;
  }
}
