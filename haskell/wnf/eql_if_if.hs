wnf_eql_if_if :: WnfEql
wnf_eql_if_if e s (If af at) (If bf bt) = do
  inc_inters e
  wnf_enter e s (And (Eql af bf) (Eql at bt))

