wnf_app_gua_sup :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_sup e s f (Sup l x y) a = do
  inc_inters e
  (f0,f1) <- clone e l f
  (a0,a1) <- clone e l a
  let app0 = (App (Gua f0 x) a0)
  let app1 = (App (Gua f1 y) a1)
  wnf_enter e s (Sup l app0 app1)

