wnf_app_get :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_get e s f@(Get c) a = case a of
  Era -> do
    inc_inters e
    wnf e s Era
  Sup l x y -> do
    inc_inters e
    (c0, c1) <- clone e l c
    wnf_enter e s (Sup l (App (Get c0) x) (App (Get c1) y))
  Tup x y -> do
    inc_inters e
    wnf_enter e s (App (App c x) y)
  _ -> wnf_unwind e s (App f a)

