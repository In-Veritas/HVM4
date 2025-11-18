wnf_and :: Env -> Stack -> Term -> Term -> IO Term
wnf_and e s a b =  do
  case a of
    Era    -> wnf_and_era e s a b
    Sup {} -> wnf_and_sup e s a b
    Fal    -> wnf_and_fal e s a b
    Tru    -> wnf_and_tru e s a b
    _      -> wnf_unwind e s (And a b)

