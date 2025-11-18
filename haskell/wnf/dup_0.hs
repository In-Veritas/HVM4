wnf_dup_0 :: Env -> Stack -> Name -> Term -> Term -> IO Term
wnf_dup_0 e s k v t = do
  inc_inters e
  subst DP0 e k v
  subst DP1 e k v
  wnf e s t

