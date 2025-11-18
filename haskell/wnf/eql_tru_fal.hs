wnf_eql_tru_fal :: WnfEql
wnf_eql_tru_fal e s Tru Fal = do
  inc_inters e
  wnf e s Fal

