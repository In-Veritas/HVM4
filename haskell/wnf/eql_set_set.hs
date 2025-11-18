wnf_eql_set_set :: WnfEql
wnf_eql_set_set e s Set Set = do
  inc_inters e
  wnf e s Tru

