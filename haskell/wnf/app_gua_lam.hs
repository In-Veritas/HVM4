wnf_app_gua_lam :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_lam e s f (Lam x g) a = do
  inc_inters e
  subst VAR e x a
  wnf_enter e s (Gua (App f (Var x)) g)

