wnf_and_fal :: Env -> Stack -> Term -> Term -> IO Term
wnf_and_fal e s Fal b = do
  inc_inters e
  wnf e s Fal

