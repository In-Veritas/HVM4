wnf_eql_tup_tup :: WnfEql
wnf_eql_tup_tup e s (Tup a1 a2) (Tup b1 b2) = do
  inc_inters e
  wnf_enter e s (And (Eql a1 b1) (Eql a2 b2))

