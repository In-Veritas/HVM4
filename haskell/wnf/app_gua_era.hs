wnf_app_gua_era :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_era e s f Era a = do
  inc_inters e
  wnf e s Era

