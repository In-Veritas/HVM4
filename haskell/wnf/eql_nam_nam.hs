wnf_eql_nam_nam :: WnfEql
wnf_eql_nam_nam e s (Nam x) (Nam y) = do
  inc_inters e
  if x == y then
    wnf e s Tru
  else
    wnf e s Fal

