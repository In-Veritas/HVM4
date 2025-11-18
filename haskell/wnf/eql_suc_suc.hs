wnf_eql_suc_suc :: WnfEql
wnf_eql_suc_suc e s (Suc a) (Suc b) = do
  inc_inters e
  wnf_enter e s (Eql a b)

