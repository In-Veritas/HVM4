wnf_eql_tru_tru :: WnfEql
wnf_eql_tru_tru e s Tru Tru = do
  inc_inters e
  wnf e s Tru

