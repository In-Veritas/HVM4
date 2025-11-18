wnf_eql_lst_lst :: WnfEql
wnf_eql_lst_lst e s (Lst aT) (Lst bT) = do
  inc_inters e
  wnf_enter e s (Eql aT bT)

