wnf_app_swi :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_swi e s f@(Swi z sc) a = case a of
  Era    -> wnf_app_swi_era e s f a
  Sup {} -> wnf_app_swi_sup e s f a
  Zer    -> wnf_app_swi_zer e s f a
  Suc {} -> wnf_app_swi_suc e s f a
  _      -> wnf_unwind e s (App f a)

