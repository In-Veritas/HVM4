wnf_eql_con_con :: WnfEql
wnf_eql_con_con e s (Con ah at) (Con bh bt) = do
  inc_inters e
  wnf_enter e s (And (Eql ah bh) (Eql at bt))

