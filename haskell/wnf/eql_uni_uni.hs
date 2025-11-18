wnf_eql_uni_uni :: WnfEql
wnf_eql_uni_uni e s Uni Uni = do
  inc_inters e
  wnf e s Tru

