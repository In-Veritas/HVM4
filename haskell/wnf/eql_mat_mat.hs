wnf_eql_mat_mat :: WnfEql
wnf_eql_mat_mat e s (Mat an ac) (Mat bn bc) = do
  inc_inters e
  wnf_enter e s (And (Eql an bn) (Eql ac bc))

