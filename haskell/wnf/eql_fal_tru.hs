wnf_eql_fal_tru :: WnfEql
wnf_eql_fal_tru e s Fal Tru = do
  inc_inters e
  wnf e s Fal

