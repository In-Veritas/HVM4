wnf_enter :: Env -> Stack -> Term -> IO Term

wnf_enter e s (Var k) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (Var k)
  wnf_sub VAR e s k

wnf_enter e s (Dp0 k) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (Dp0 k)
  mlv <- take_dup e k
  case mlv of
    Just (l, v) -> wnf_enter e (FDp0 k l : s) v
    Nothing     -> wnf_sub DP0 e s k

wnf_enter e s (Dp1 k) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (Dp1 k)
  mlv <- take_dup e k
  case mlv of
    Just (l, v) -> wnf_enter e (FDp1 k l : s) v
    Nothing     -> wnf_sub DP1 e s k

wnf_enter e s (App f x) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (App f x)
  wnf_enter e (FApp x : s) f

wnf_enter e s (And a b) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (And a b)
  wnf_enter e (FAnd b : s) a

wnf_enter e s (Eql a b) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (Eql a b)
  wnf_enter e (FEql b : s) a

wnf_enter e s (Dup k l v t) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (Dup k l v t)
  make_dup e k l v
  wnf_enter e s t

-- wnf_ref
wnf_enter e s (Ref k) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (Ref k)
  let (Book m) = env_book e
  case M.lookup k m of
    Just f  -> do
      inc_inters e
      g <- alloc e f
      wnf_enter e s g -- ← with this line: 162 interactions
      -- wnf_enter e s (Gua (Nam ("@" ++ int_to_name k)) g) -- ← with this line: 765 interactions
    Nothing -> error $ "UndefinedReference: " ++ int_to_name k

wnf_enter e s (Gua f g) = do
  when debug $ putStrLn $ "wnf_enter: " ++ show (Gua f g)
  wnf_unwind e s (Gua f g)

wnf_enter e s f = do
  when debug $ putStrLn $ "wnf_enter: " ++ show f
  wnf_unwind e s f

