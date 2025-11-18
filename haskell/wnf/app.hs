wnf_app :: Env -> Stack -> Term -> Term -> IO Term
wnf_app e s f a = do
  when debug $ putStrLn $ "wnf_app: " ++ show (App f a)
  case f of
    Era       -> wnf_app_era e s f a
    Sup {}    -> wnf_app_sup e s f a
    Lam {}    -> wnf_app_lam e s f a
    Nam {}    -> wnf_app_nam e s f a
    Dry {}    -> wnf_app_dry e s f a
    Swi {}    -> wnf_enter e (FAppF f : s) a
    Get {}    -> wnf_enter e (FAppF f : s) a
    Efq       -> wnf_enter e (FAppF f : s) a
    Use {}    -> wnf_enter e (FAppF f : s) a
    If {}     -> wnf_enter e (FAppF f : s) a
    Mat {}    -> wnf_enter e (FAppF f : s) a
    Gua f0 g0 -> wnf_enter e (FAppG (Tup f0 a) : s) g0
    _         -> wnf_unwind e s (App f a)

