wnf_app_swi_suc :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_swi_suc e s (Swi z sc) (Suc n) = do
  inc_inters e
  wnf_enter e s (App sc n)

