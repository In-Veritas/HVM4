wnf_eql_swi_swi :: WnfEql
wnf_eql_swi_swi e s (Swi az as) (Swi bz bs) = do
  inc_inters e
  wnf_enter e s (And (Eql az bz) (Eql as bs))

