wnf_eql :: WnfEql
wnf_eql e s a b = do
  case (a, b) of
    (Era, b) -> do
      inc_inters e
      wnf e s Era
    (a, Era) -> do
      inc_inters e
      wnf e s Era
    (Sup l a0 a1, b) -> do
      inc_inters e
      k <- fresh e
      make_dup e k l b
      wnf_enter e s (Sup l (Eql a0 (Dp0 k)) (Eql a1 (Dp1 k)))
    (a, Sup l b0 b1) -> do
      inc_inters e
      k <- fresh e
      make_dup e k l a
      wnf_enter e s (Sup l (Eql (Dp0 k) b0) (Eql (Dp1 k) b1))
    (a, b) -> do
      wnf_eql_val e s a b

