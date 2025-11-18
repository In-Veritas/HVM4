wnf_app_if :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_if e s f@(If ft ff) a = case a of
  Era -> do
    inc_inters e
    wnf e s Era
  Sup l x y -> do
    inc_inters e
    (ft0, ft1) <- clone e l ft
    (ff0, ff1) <- clone e l ff
    wnf_enter e s (Sup l (App (If ft0 ff0) x) (App (If ft1 ff1) y))
  Fal -> do
    inc_inters e
    wnf e s ft
  Tru -> do
    inc_inters e
    wnf e s ff
  _ -> wnf_unwind e s (App f a)

