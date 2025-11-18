wnf_app_f :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_f e s f a = case f of
  Swi {} -> wnf_app_swi e s f a
  Get {} -> wnf_app_get e s f a
  Efq    -> wnf_app_efq e s f a
  Use {} -> wnf_app_use e s f a
  If {}  -> wnf_app_if  e s f a
  Mat {} -> wnf_app_mat e s f a
  _      -> wnf_unwind e s (App f a)

