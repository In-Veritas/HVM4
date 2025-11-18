wnf_app_gua :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_gua e s f g a = do
  case g of
    Era    -> wnf_app_gua_era e s f g a
    Sup {} -> wnf_app_gua_sup e s f g a
    Lam {} -> wnf_app_gua_lam e s f g a
    Swi {} -> wnf_enter e (FAppGF (Tup f g) : s) a
    Get {} -> wnf_enter e (FAppGF (Tup f g) : s) a
    Efq    -> wnf_enter e (FAppGF (Tup f g) : s) a
    Use {} -> wnf_enter e (FAppGF (Tup f g) : s) a
    If {}  -> wnf_enter e (FAppGF (Tup f g) : s) a
    Mat {} -> wnf_enter e (FAppGF (Tup f g) : s) a
    Gua {} -> wnf_app_gua_gua e s f g a
    _      -> wnf_unwind e s (App (Tup f g) a)

