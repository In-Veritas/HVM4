wnf_dup_2 :: Env -> Stack -> Name -> Lab -> Term -> Term -> Term -> (Term -> Term -> Term) -> IO Term
wnf_dup_2 e s k l t v1 v2 c = do
  inc_inters e
  (v1a, v1b) <- clone e l v1
  (v2a, v2b) <- clone e l v2
  subst DP0 e k (c v1a v2a)
  subst DP1 e k (c v1b v2b)
  wnf e s t

