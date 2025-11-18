wnf_and_sup :: Env -> Stack -> Term -> Term -> IO Term
wnf_and_sup e s (Sup l a0 a1) b = do
  inc_inters e
  (b0, b1) <- clone e l b
  wnf_enter e s (Sup l (And a0 b0) (And a1 b1))

