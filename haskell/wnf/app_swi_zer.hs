wnf_app_swi_zer :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_swi_zer e s (Swi z sc) Zer = do
  inc_inters e
  wnf e s z

