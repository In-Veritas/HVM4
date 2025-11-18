wnf_eql_lam_lam :: WnfEql
wnf_eql_lam_lam e s (Lam ax af) (Lam bx bf) = do
  inc_inters e
  x <- fresh e
  subst VAR e ax (Nam (int_to_name x))
  subst VAR e bx (Nam (int_to_name x))
  wnf_enter e s (Eql af bf)

