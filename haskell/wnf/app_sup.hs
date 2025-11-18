wnf_app_sup :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_sup e s (Sup fL fa fb) v = do
  inc_inters e
  (x0,x1) <- clone e fL v
  wnf e s (Sup fL (App fa x0) (App fb x1))

