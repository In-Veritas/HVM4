wnf_app_gua_use :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_use e s f (Use u) a = case a of
  Era -> do { inc_inters e; wnf e s Era }
  Sup l x y -> do
    inc_inters e
    (f0, f1) <- clone e l f
    (u0, u1) <- clone e l u
    wnf_enter e s (Sup l (App (Gua f0 (Use u0)) x) (App (Gua f1 (Use u1)) y))
  One -> do
    inc_inters e
    wnf_enter e s (Gua (App f One) u)
  _ -> wnf_unwind e s (App (Gua f (Use u)) a)

