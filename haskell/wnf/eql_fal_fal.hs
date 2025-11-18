wnf_eql_fal_fal :: WnfEql
wnf_eql_fal_fal e s Fal Fal = do
  inc_inters e
  wnf e s Tru

