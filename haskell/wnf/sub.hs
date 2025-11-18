wnf_sub :: Kind -> Env -> Stack -> Name -> IO Term
wnf_sub ki e s k = do
  when debug $ putStrLn $ "wnf_sub: " ++ show (Var k)
  mt <- take_sub ki e k
  case mt of
    Just t  -> wnf e s t
    Nothing -> wnf_unwind e s $ case ki of
      VAR -> Var k
      DP0 -> Dp0 k
      DP1 -> Dp1 k

