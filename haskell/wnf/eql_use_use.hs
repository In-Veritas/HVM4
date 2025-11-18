wnf_eql_use_use :: WnfEql
wnf_eql_use_use e s (Use au) (Use bu) = do
  inc_inters e
  wnf_enter e s (Eql au bu)

