wnf_app_gua_mat :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_mat e s f (Mat n c) a = case a of
  Era -> do { inc_inters e; wnf e s Era }
  Sup l x y -> do
    inc_inters e
    (f0, f1) <- clone e l f
    (n0, n1) <- clone e l n
    (c0, c1) <- clone e l c
    wnf_enter e s (Sup l (App (Gua f0 (Mat n0 c0)) x) (App (Gua f1 (Mat n1 c1)) y))
  Nil -> do
    inc_inters e
    wnf_enter e s (Gua (App f Nil) n)
  Con h t -> do
    inc_inters e
    hV <- fresh e
    tV <- fresh e
    let fn = Lam hV (Lam tV (App f (Con (Var hV) (Var tV))))
    wnf_enter e s (App (App (Gua fn c) h) t)
  _ -> wnf_unwind e s (App (Gua f (Mat n c)) a)

