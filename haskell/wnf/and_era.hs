wnf_and_era :: Env -> Stack -> Term -> Term -> IO Term
wnf_and_era e s Era b = do
  inc_inters e
  wnf e s Era

