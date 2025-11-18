wnf_app_era :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_era e s Era v = do
  inc_inters e
  wnf e s Era

