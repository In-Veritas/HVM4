wnf_eql_one_one :: WnfEql
wnf_eql_one_one e s One One = do
  inc_inters e
  wnf e s Tru

