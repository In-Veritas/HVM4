wnf_app_gua_swi :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_swi e s f (Swi z sc) a = case a of
  Era    -> wnf_app_gua_swi_era e s f z sc a
  Sup {} -> wnf_app_gua_swi_sup e s f z sc a
  Zer    -> wnf_app_gua_swi_zer e s f z sc a
  Suc {} -> wnf_app_gua_swi_suc e s f z sc a
  _      -> wnf_unwind e s (App f a)

