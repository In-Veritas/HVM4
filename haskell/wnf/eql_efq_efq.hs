wnf_eql_efq_efq :: WnfEql
wnf_eql_efq_efq e s Efq Efq = do
  inc_inters e
  wnf e s Tru

