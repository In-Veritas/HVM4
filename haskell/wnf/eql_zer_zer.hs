wnf_eql_zer_zer :: WnfEql
wnf_eql_zer_zer e s Zer Zer = do
  inc_inters e
  wnf e s Tru

