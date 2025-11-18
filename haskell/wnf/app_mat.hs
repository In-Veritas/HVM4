wnf_app_mat :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_mat e s f@(Mat n c) a = case a of
  Era -> do
    inc_inters e
    wnf e s Era
  Sup l x y -> do
    inc_inters e
    (n0, n1) <- clone e l n
    (c0, c1) <- clone e l c
    wnf_enter e s (Sup l (App (Mat n0 c0) x) (App (Mat n1 c1) y))
  Nil -> do
    inc_inters e
    wnf e s n
  Con h t -> do
    inc_inters e
    wnf_enter e s (App (App c h) t)
  _ -> wnf_unwind e s (App f a)

