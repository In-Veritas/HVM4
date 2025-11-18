wnf_eql_get_get :: WnfEql
wnf_eql_get_get e s (Get ac) (Get bc) = do
  inc_inters e
  wnf_enter e s (Eql ac bc)

