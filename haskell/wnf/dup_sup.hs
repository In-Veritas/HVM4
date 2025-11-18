wnf_dup_sup :: WnfDup
wnf_dup_sup e s (Sup vl va vb) k l t
  | l == vl = do
      inc_inters e
      subst DP0 e k va
      subst DP1 e k vb
      wnf e s t
  | otherwise = do
      wnf_dup_2 e s k l t va vb (Sup vl)

