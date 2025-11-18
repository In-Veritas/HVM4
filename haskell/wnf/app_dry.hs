wnf_app_dry :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_dry e s (Dry ff fx) v = wnf e s (Dry (Dry ff fx) v)

