wnf_app_gua_efq :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_efq e s f Efq a = case a of
  Era -> do { inc_inters e; wnf e s Era }
  Sup l x y -> do
    inc_inters e
    (f0, f1) <- clone e l f
    wnf_enter e s (Sup l (App (Gua f0 Efq) x) (App (Gua f1 Efq) y))
  _ -> wnf_unwind e s (App (Gua f Efq) a)

