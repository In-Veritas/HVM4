wnf_app_efq :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_efq e s Efq a = case a of
  Era -> do
    inc_inters e
    wnf e s Era
  Sup l x y -> do
    inc_inters e
    wnf_enter e s (Sup l (App Efq x) (App Efq y))
  _ -> wnf_unwind e s (App Efq a)

