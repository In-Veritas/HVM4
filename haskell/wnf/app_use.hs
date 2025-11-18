wnf_app_use :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_use e s f@(Use u) a = case a of
  Era -> do
    inc_inters e
    wnf e s Era
  Sup l x y -> do
    inc_inters e
    (u0, u1) <- clone e l u
    wnf_enter e s (Sup l (App (Use u0) x) (App (Use u1) y))
  One -> do
    inc_inters e
    wnf e s u
  _ -> wnf_unwind e s (App f a)

