wnf_app_gua_get :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_get e s f (Get c) a = case a of
  Era -> do { inc_inters e; wnf e s Era }
  Sup l x y -> do
    inc_inters e
    (f0, f1) <- clone e l f
    (c0, c1) <- clone e l c
    wnf_enter e s (Sup l (App (Gua f0 (Get c0)) x) (App (Gua f1 (Get c1)) y))
  Tup x y -> do
    inc_inters e
    xV <- fresh e
    yV <- fresh e
    let fn = Lam xV (Lam yV (App f (Tup (Var xV) (Var yV))))
    wnf_enter e s (App (App (Gua fn c) x) y)
  _ -> wnf_unwind e s (App (Gua f (Get c)) a)

