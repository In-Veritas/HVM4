wnf_app_swi_era :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_swi_era e s (Swi z sc) Era = do
  inc_inters e
  wnf e s Era

