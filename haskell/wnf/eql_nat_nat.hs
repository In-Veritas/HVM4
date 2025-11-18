wnf_eql_nat_nat :: WnfEql
wnf_eql_nat_nat e s Nat Nat = do
  inc_inters e
  wnf e s Tru

