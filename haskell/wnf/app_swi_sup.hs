wnf_app_swi_sup :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_swi_sup e s (Swi z sc) (Sup l a b) = do
  inc_inters e
  (z0, z1) <- clone e l z
  (s0, s1) <- clone e l sc
  let app0 = App (Swi z0 s0) a
  let app1 = App (Swi z1 s1) b
  wnf_enter e s (Sup l app0 app1)

