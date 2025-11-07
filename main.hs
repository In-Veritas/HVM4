{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Monad (replicateM, when)
import Data.IORef
import System.CPUTime (getCPUTime)
import System.IO (hPutStrLn, stderr)
import qualified Data.IntMap.Strict as IntMap

-- Definitions
-- ===========

-- Use Int for Names and Labels for maximum performance.
type Lab   = Int
type Name  = Int

-- Map a is a mutable IntMap stored inside an IORef.
type Map a = IORef (IntMap.IntMap a)

-- Optimized Term data structure. Strict fields and UNPACK pragmas.
data Term
  = Nam {-# UNPACK #-} !Name
  | Var {-# UNPACK #-} !Name
  | Dp0 {-# UNPACK #-} !Name
  | Dp1 {-# UNPACK #-} !Name
  | Era
  | Sup {-# UNPACK #-} !Lab !Term !Term
  | Dup {-# UNPACK #-} !Name {-# UNPACK #-} !Lab !Term !Term
  | Lam {-# UNPACK #-} !Name !Term
  | Dry !Term !Term
  | App !Term !Term

-- A minimal Show instance using Int IDs.
instance Show Term where
  show (Nam k)       = show k
  show (Dry f x)     = "(" ++ show f ++ " " ++ show x ++ ")"
  show (Var k)       = show k
  show (Dp0 k)       = show k ++ "₀"
  show (Dp1 k)       = show k ++ "₁"
  show Era           = "&{}"
  show (Sup l a b)   = "&" ++ show l ++ "{" ++ show a ++ "," ++ show b ++ "}"
  show (Dup k l v t) = "!" ++ show k ++ "&" ++ show l ++ "=" ++ show v ++ ";" ++ show t
  show (Lam k f)     = "λ" ++ show k ++ "." ++ show f
  show (App f x)     = "(" ++ show f ++ " " ++ show x ++ ")"

-- Environment
-- ===========

data Env = Env
  { inters  :: !(IORef Int)
  , id_new  :: !(IORef Int)
  , var_map :: !(Map Term)
  , dp0_map :: !(Map Term)
  , dp1_map :: !(Map Term)
  , dup_map :: !(Map (Lab,Term))
  }

newEnv :: IO Env
newEnv = do
  i   <- newIORef 0
  idn <- newIORef 0
  vm  <- newIORef IntMap.empty
  d0m <- newIORef IntMap.empty
  d1m <- newIORef IntMap.empty
  dm  <- newIORef IntMap.empty
  return $ Env i idn vm d0m d1m dm

-- Environment Operations
-- ======================

-- Strict atomic increment for the interaction counter.
inc_inters :: Env -> IO ()
inc_inters e = atomicModifyIORef' (inters e) (\x -> (x+1, ()))
{-# INLINE inc_inters #-}

-- Fresh name generation (O(1)).
fresh :: Env -> IO Name
fresh e = atomicModifyIORef' (id_new e) (\n -> (n + 1, n))
{-# INLINE fresh #-}

-- Substitution (O(log n) map update).
subst :: Map a -> Name -> a -> IO ()
subst vec k v = atomicModifyIORef' vec (\m -> (IntMap.insert k v m, ()))
{-# INLINE subst #-}

subst_var :: Env -> Name -> Term -> IO ()
subst_var e k v = subst (var_map e) k v
{-# INLINE subst_var #-}

subst_dp0 :: Env -> Name -> Term -> IO ()
subst_dp0 e k v = subst (dp0_map e) k v
{-# INLINE subst_dp0 #-}

subst_dp1 :: Env -> Name -> Term -> IO ()
subst_dp1 e k v = subst (dp1_map e) k v
{-# INLINE subst_dp1 #-}

delay_dup :: Env -> Name -> (Lab, Term) -> IO ()
delay_dup e k lv = subst (dup_map e) k lv
{-# INLINE delay_dup #-}

-- Taker (O(log n) map read and clear).
-- Implements linear substitution: read once and clear the slot.
taker :: Map a -> Name -> IO (Maybe a)
taker vec k =
  atomicModifyIORef' vec $ \m ->
    case IntMap.lookup k m of
      Just v  -> (IntMap.delete k m, Just v)
      Nothing -> (m, Nothing)
{-# INLINE taker #-}

take_var :: Env -> Name -> IO (Maybe Term)
take_var e = taker (var_map e)
{-# INLINE take_var #-}

take_dp0 :: Env -> Name -> IO (Maybe Term)
take_dp0 e = taker (dp0_map e)
{-# INLINE take_dp0 #-}

take_dp1 :: Env -> Name -> IO (Maybe Term)
take_dp1 e = taker (dp1_map e)
{-# INLINE take_dp1 #-}

take_dup :: Env -> Name -> IO (Maybe (Lab, Term))
take_dup e = taker (dup_map e)
{-# INLINE take_dup #-}


-- Evaluation (Weak Head Normal Form)
-- ==========

wnf :: Env -> Term -> IO Term
wnf e t = go t
  where
    go (App f x)     = do
      !f0 <- wnf e f
      app e f0 x
    go (Dup k l v t) = do
      delay_dup e k (l,v)
      wnf e t
    go (Var x)       = var e x
    go (Dp0 x)       = dp0 e x
    go (Dp1 x)       = dp1 e x
    go f             = return f

-- Application dispatcher
app :: Env -> Term -> Term -> IO Term
app e (Lam fk ff)   x = app_lam e fk ff x
app e (Sup fl fa fb) x = app_sup e fl fa fb x
app e (Nam fk)      x = app_nam e fk x
app e (Dry df dx)   x = app_dry e df dx x
app e f             x = return $ App f x
{-# INLINE app #-}

-- Duplication dispatcher
dup :: Env -> Name -> Lab -> Term -> Term -> IO Term
dup e k l (Lam vk vf)   t = dup_lam e k l vk vf t
dup e k l (Sup vl va vb) t = dup_sup e k l vl va vb t
dup e k l (Nam vk)      t = dup_nam e k l vk t
dup e k l (Dry vf vx)   t = dup_dry e k l vf vx t
dup e k l v             t = return $ Dup k l v t
{-# INLINE dup #-}

-- Interactions
-- ============

-- (λx.f v) -> f[x<-v]
app_lam :: Env -> Name -> Term -> Term -> IO Term
app_lam e fx ff v = do
  inc_inters e
  subst_var e fx v
  wnf e ff

-- (&L{fa,fb} v) -> !x&L=v; &L{(fa x₀),(fb x₁)}
app_sup :: Env -> Lab -> Term -> Term -> Term -> IO Term
app_sup e fL fa fb v = do
  inc_inters e
  x <- fresh e
  let app0 = App fa (Dp0 x)
  let app1 = App fb (Dp1 x)
  let sup  = Sup fL app0 app1
  -- Optimization: Inline the Dup effect instead of constructing the Dup term.
  -- Dup x fL v sup => delay_dup x (fL, v); wnf sup
  delay_dup e x (fL, v)
  wnf e sup

-- (Nam v)
app_nam :: Env -> Name -> Term -> IO Term
app_nam e fk v = do
    inc_inters e
    return $ Dry (Nam fk) v

-- (Dry v)
app_dry :: Env -> Term -> Term -> Term -> IO Term
app_dry e df dx v = do
    inc_inters e
    return $ Dry (Dry df dx) v

-- !k&L = λvk.vf; t -> !g&L=vf; t[k₀<-λx0.(g₀), k₁<-λx1.(g₁), vk<-&L{x0,x1}]
dup_lam :: Env -> Name -> Lab -> Name -> Term -> Term -> IO Term
dup_lam e k l vk vf t = do
  inc_inters e
  x0 <- fresh e
  x1 <- fresh e
  g  <- fresh e
  subst_dp0 e k (Lam x0 (Dp0 g))
  subst_dp1 e k (Lam x1 (Dp1 g))
  subst_var e vk (Sup l (Var x0) (Var x1))
  -- Optimization: Inline the Dup effect.
  delay_dup e g (l, vf)
  wnf e t

-- !k&L = &vL{va,vb}; t
dup_sup :: Env -> Name -> Lab -> Lab -> Term -> Term -> Term -> IO Term
dup_sup e k l vl va vb t
  | l == vl = do
      -- Annihilation
      inc_inters e
      subst_dp0 e k va
      subst_dp1 e k vb
      wnf e t
  | otherwise = do
      -- Commutation
      inc_inters e
      a <- fresh e
      b <- fresh e
      subst_dp0 e k (Sup vl (Dp0 a) (Dp0 b))
      subst_dp1 e k (Sup vl (Dp1 a) (Dp1 b))
      -- Optimization: Inline the nested Dup effects.
      delay_dup e a (l, va)
      delay_dup e b (l, vb)
      wnf e t

-- !k&L = Nam; t
dup_nam :: Env -> Name -> Lab -> Name -> Term -> IO Term
dup_nam e k l vk t = do
  inc_inters e
  -- Share the constructor allocation
  let !nam_vk = Nam vk
  subst_dp0 e k nam_vk
  subst_dp1 e k nam_vk
  wnf e t

-- !k&L = Dry; t
dup_dry :: Env -> Name -> Lab -> Term -> Term -> Term -> IO Term
dup_dry e k l vf vx t = do
  inc_inters e
  f <- fresh e
  x <- fresh e
  subst_dp0 e k (Dry (Dp0 f) (Dp0 x))
  subst_dp1 e k (Dry (Dp1 f) (Dp1 x))
  -- Optimization: Inline the nested Dup effects.
  delay_dup e f (l, vf)
  delay_dup e x (l, vx)
  wnf e t

-- Variable lookups
-- ================

-- x
var :: Env -> Name -> IO Term
var e k = do
  mt <- take_var e k
  case mt of
    Just t  -> wnf e t
    Nothing -> return $ Var k

-- Common logic for Dp0 and Dp1 lookups
dp_common :: (Env -> Name -> IO (Maybe Term)) -> (Name -> Term) -> Env -> Name -> IO Term
dp_common take_dp constructor e k = do
  mt <- take_dp e k
  case mt of
    Just t  -> wnf e t
    Nothing -> do
      -- Check for delayed duplication if the specific side (0 or 1) is not yet available.
      mlv <- take_dup e k
      case mlv of
        Just (l, v) -> do
            -- Normalize V before the interaction.
            !v0 <- wnf e v
            -- Trigger the duplication interaction. The continuation 't' is the Dp variable itself.
            dup e k l v0 (constructor k)
        Nothing -> return $ constructor k

-- x₀
dp0 :: Env -> Name -> IO Term
dp0 = dp_common take_dp0 Dp0

-- x₁
dp1 :: Env -> Name -> IO Term
dp1 = dp_common take_dp1 Dp1

-- Normalization (Full normalization / Read-back)
-- =============

nf :: Env -> Term -> IO Term
nf e x = do
  !x0 <- wnf e x
  go e x0
  where
    go e (Nam k)      = return $ Nam k
    go e (Dry f x)    = Dry <$> nf e f <*> nf e x
    go e (Var k)      = return $ Var k
    go e (Dp0 k)      = return $ Dp0 k
    go e (Dp1 k)      = return $ Dp1 k
    go e Era          = return Era
    go e (Sup l a b)  = Sup l <$> nf e a <*> nf e b
    go e (Dup k l v t)= Dup k l <$> nf e v <*> nf e t
    go e (App f x)    = App <$> nf e f <*> nf e x
    go e (Lam k f)    = do
      -- Normalize under a binder. This requires backtracking in the mutable environment.
      -- We save the current binding, set the new binding (k -> Nam k for readback),
      -- normalize the body, and then restore the old binding.
      let vec = var_map e

      -- 1. Backup
      !maybe_old_v <- IntMap.lookup k <$> readIORef vec

      -- 2. Substitute k -> Nam k
      subst_var e k (Nam k)

      -- 3. Normalize body
      !f0 <- nf e f

      -- 4. Restore
      case maybe_old_v of
        Just old -> subst_var e k old
        Nothing  -> atomicModifyIORef' vec (\m -> (IntMap.delete k m, ()))

      return $ Lam k f0

-- Main (Benchmark Setup)
-- ====

-- Programmatic construction of the benchmark term (f N).
-- This avoids parsing and ensures we use Int identifiers from the start.
-- The benchmark calculates (2^N) applied to Not and True.
-- Term: ((f N) λX.((X False) True)) True)

build_term :: Int -> IO (Term, Env)
build_term n = do
  e <- newEnv

  -- Allocate Int IDs for the names used in the construction.
  name_f <- fresh e  -- The function argument 'f'
  name_X <- fresh e  -- Variables for 'Not'
  name_T0 <- fresh e; name_F0 <- fresh e -- Variables for 'False' argument
  name_T1 <- fresh e; name_F1 <- fresh e -- Variables for 'True' argument
  name_T2 <- fresh e; name_F2 <- fresh e -- Variables for the final 'True' argument

  -- Variables x0..x(N-1) and F0..F(N-1)
  names_x <- replicateM n (fresh e)
  names_F <- replicateM n (fresh e)

  -- Superposition label 'A' (arbitrary constant Int)
  let lab_A = 1

  -- Helper to build the structure: λxi.(Fi₀ (Fi₁ xi))
  let build_body i =
        let name_Fi = names_F !! i
            name_xi = names_x !! i
        in Lam name_xi (App (Dp0 name_Fi) (App (Dp1 name_Fi) (Var name_xi)))

  -- Final term in the sequence
  let final_term = build_body (n-1)

  -- Construct the chain of duplications:
  -- !F0 &A = f; !F1 &A = λx0.(F0₀ (F0₁ x0)); ...
  let build_dup i body =
        let name_Fi = names_F !! i
        in if i == 0
           then Dup name_Fi lab_A (Var name_f) body
           else Dup name_Fi lab_A (build_body (i-1)) body

  let dups_term = foldr build_dup final_term [0..n-1]

  -- λf. dups_term
  let f_term = Lam name_f dups_term

  -- Arguments: arg1 (Not) and arg2 (True)

  -- Church Boolean True: λT.λF.T
  let church_true t f = Lam t (Lam f (Var t))
  -- Church Boolean False: λT.λF.F
  let church_false t f = Lam t (Lam f (Var f))

  -- arg1 = Not = λX.((X False) True)
  let arg1 = Lam name_X (App
                (App (Var name_X) (church_false name_T0 name_F0))
                (church_true name_T1 name_F1))

  -- arg2 = True
  let arg2 = church_true name_T2 name_F2

  -- ((f_term arg1) arg2)
  let term = App (App f_term arg1) arg2

  return (term, e)

main :: IO ()
main = do
  let n = 18
  -- 1. Setup: Build the term and initialize the environment
  (!term, !env) <- build_term n

  -- 2. Execution: Normalize the term
  start <- getCPUTime
  !res <- nf env term -- Force evaluation
  end <- getCPUTime

  -- 3. Output results
  let duration = realToFrac (end - start) / 1e12 :: Double
  interactions <- readIORef (inters env)

  -- Print the resulting term and the interaction count (matching the original output format)
  print res
  print interactions

  -- Optional: Print performance stats to stderr
  hPutStrLn stderr $ "Time: " ++ show duration ++ " s"
  when (duration > 0) $ do
    let ips = fromIntegral interactions / duration
    hPutStrLn stderr $ "Perf: " ++ show (round ips :: Integer) ++ " interactions/second"
