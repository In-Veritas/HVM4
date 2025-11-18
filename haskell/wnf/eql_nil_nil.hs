wnf_eql_nil_nil :: WnfEql
wnf_eql_nil_nil e s Nil Nil = do
  inc_inters e
  wnf e s Tru

