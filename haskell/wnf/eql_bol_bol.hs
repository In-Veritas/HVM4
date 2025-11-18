wnf_eql_bol_bol :: WnfEql
wnf_eql_bol_bol e s Bol Bol = do
  inc_inters e
  wnf e s Tru

