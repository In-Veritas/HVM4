wnf_app_gua_gua :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_gua e s f g a = do
  inc_inters e
  wnf_enter e s (Gua (App f a) (App g a))

