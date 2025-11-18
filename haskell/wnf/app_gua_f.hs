wnf_app_gua_f :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_f e s f m a = case m of
  Swi {} -> wnf_app_gua_swi e s f m a
  Get {} -> wnf_app_gua_get e s f m a
  Efq    -> wnf_app_gua_efq e s f m a
  Use {} -> wnf_app_gua_use e s f m a
  If {}  -> wnf_app_gua_if  e s f m a
  Mat {} -> wnf_app_gua_mat e s f m a
  _      -> wnf_unwind e s (App (Gua f m) a)

