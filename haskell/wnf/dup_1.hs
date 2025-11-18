wnf_dup_1 :: Env -> Stack -> Name -> Lab -> Term -> Term -> (Term -> Term) -> IO Term
wnf_dup_1 e s k l t v1 c = do
  inc_inters e
  (v1a, v1b) <- clone e l v1
  subst DP0 e k (c v1a)
  subst DP1 e k (c v1b)
  wnf e s t

