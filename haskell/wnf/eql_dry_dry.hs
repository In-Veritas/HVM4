wnf_eql_dry_dry :: WnfEql
wnf_eql_dry_dry e s (Dry af ax) (Dry bf bx) = do
  inc_inters e
  wnf_enter e s (And (Eql af bf) (Eql ax bx))

