wnf_eql_all_all :: WnfEql
wnf_eql_all_all e s (All aA aB) (All bA bB) = do
  inc_inters e
  wnf_enter e s (And (Eql aA bA) (Eql aB bB))

