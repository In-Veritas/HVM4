wnf_app_gua_if :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua_if e s f (If ft ff) a = case a of
  Era -> do { inc_inters e; wnf e s Era }
  Sup l x y -> do
    inc_inters e
    (f0, f1) <- clone e l f
    (ft0, ft1) <- clone e l ft
    (ff0, ff1) <- clone e l ff
    wnf_enter e s (Sup l (App (Gua f0 (If ft0 ff0)) x) (App (Gua f1 (If ft1 ff1)) y))
  Fal -> do
    inc_inters e
    wnf_enter e s (Gua (App f Fal) ft)
  Tru -> do
    inc_inters e
    wnf_enter e s (Gua (App f Tru) ff)
  _ -> wnf_unwind e s (App (Gua f (If ft ff)) a)

