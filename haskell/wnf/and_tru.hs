wnf_and_tru :: Env -> Stack -> Term -> Term -> IO Term
wnf_and_tru e s Tru b = do
  inc_inters e
  wnf e s b

