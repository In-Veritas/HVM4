{-# LANGUAGE MultilineStrings #-}

-- Calculus of Interactions
-- ========================
-- CoI is a term rewrite system for the following grammar:
--
-- Term ::=
-- | Var ::= Mode Name
-- | Dp0 ::= Mode Name "₀"
-- | Dp1 ::= Mode Name "₁"
-- | Era ::= "&{}"
-- | Sup ::= "&" Name "{" Term "," Term "}"
-- | Dup ::= "!" Name "&" Name "=" Term ";" Term
-- | Lam ::= "λ" Name "." Term
-- | App ::= "(" Term " " Term ")"
-- | Zer ::= "0"
-- | Suc ::= "1+"
-- | Swi ::= "Λ" "{" "0" ":" Term ";"? "1" "+" ":" Term ";"? "}"
-- | Ref ::= "@" Name
-- | Cal ::= Term "~>" Term "%" Subs
--
-- Where:
-- - Subs ::= "{" [Name "→" Term] "}"
-- - Name ::= any sequence of base-64 chars in _ A-Z a-z 0-9 $
-- - Mode ::= 0 (cold) | 1 (hot)
-- - [T]  ::= any sequence of T separated by ","
--
-- In CoI:
-- - Variables are affine (they must occur at most once)
-- - Variables range globally (they can occur anywhere)
-- - Cold variables (Mode=0) live inside Cal/Book and can be duplicated structurally.
-- - Hot variables (Mode=1) live in the runtime and interact with the environment.
--
-- Terms are rewritten via the following interaction rules:
--
-- (λx.f a)
-- -------- app-lam
-- x ← a
-- f
--
-- (&L{f,g} a)
-- ----------------- app-sup
-- ! A &L = a
-- &L{(f A₀),(g A₁)}
--
-- ! F &L = λx.f
-- ---------------- dup-lam
-- F₀ ← λ$x0.G₀
-- F₁ ← λ$x1.G₁
-- x  ← &L{$x0,$x1}
-- ! G &L = f
--
-- ! X &L = &R{a,b}
-- ---------------- dup-sup
-- if L == R:
--   X₀ ← a
--   X₁ ← b
-- else:
--   ! A &L = a
--   ! B &L = b
--   X₀ ← &R{A₀,B₀}
--   X₁ ← &R{A₁,B₁}
--
-- ! X &L = 0
-- ---------- dup-zer
-- X₀ ← 0
-- X₁ ← 0
--
-- ! X &L = 1+n
-- ------------ dup-suc
-- ! N &L = n
-- X₀ ← 1+N₀
-- X₁ ← 1+N₁
--
-- ! X &L = Λ{0:z;1+:s}
-- -------------------- dup-swi
-- ! Z &L = z
-- ! S &L = s
-- X₀ ← Λ{0:Z₀;1+:S₀}
-- X₁ ← Λ{0:Z₁;1+:S₁}
--
-- ! X &L = k
-- ---------- dup-var (cold)
-- X₀ ← k
-- X₁ ← k
--
-- ! X &L = k₀
-- ----------- dup-dp0 (cold)
-- X₀ ← k₀
-- X₁ ← k₀
--
-- ! X &L = k₁
-- ----------- dup-dp1 (cold)
-- X₀ ← k₁
-- X₁ ← k₁
--
-- @foo
-- ------------------ ref
-- foo ~> Book["foo"] % {}
--
-- ((f ~> λx.g % m) a)
-- ------------------- app-cal-lam
-- (f x) ~> g % {m, x→a}
--
-- ((f ~> Λ{0:z;1+:s} % m) 0)
-- -------------------------- app-cal-swi-zer
-- (f 0) ~> z % m
--
-- ((f ~> Λ{0:z;1+:s} % m) 1+n)
-- ---------------------------- app-cal-swi-suc
-- ((λp.(f 1+p) ~> s % m) n)
--
-- ((f ~> Λ{0:z;1+:s} % m) &L{a,b})
-- -------------------------------- app-cal-swi-sup
-- ! &L F = f
-- ! &L M = m
-- ! &L Z = z
-- ! &L S = s
-- &L{((F₀ ~> Λ{0:Z₀;1+:S₀} % M₀) a)
--   ,((F₁ ~> Λ{0:Z₁;1+:S₁} % M₁) b)}
--
-- ! &L X = f ~> g % m
-- ------------------- dup-cal
-- ! &L F = f
-- ! &L G = g
-- ! &L M = m
-- X₀ ← F₀ ~> G₀ % M₀
-- X₁ ← F₁ ~> G₁ % M₁

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Monad (replicateM, when, mapM)
import Data.Bits (shiftL)
import Data.Char (chr, ord)
import Data.IORef
import Data.List (foldl', intercalate, unzip)
import System.CPUTime
import Text.ParserCombinators.ReadP
import Text.Printf
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

-- Types
-- =====

type Lab  = Int
type Name = Int
type Mode = Int -- 0: cold (book/cal), 1: hot (runtime)

data Term
  = Nam !String
  | Var !Mode !Name
  | Dp0 !Mode !Name
  | Dp1 !Mode !Name
  | Era
  | Sup !Lab !Term !Term
  | Dup !Name !Lab !Term !Term
  | Lam !Name !Term
  | App !Term !Term
  | Zer
  | Suc !Term
  | Swi !Term !Term
  | Ref !Name
  | Cal !Term !Term !(M.Map Name Term)
  deriving (Eq)

data Book = Book (M.Map Name Term)

-- Showing
-- =======

instance Show Term where
  show (Nam k)        = k
  show (Var h k)      = (if h == 0 then "·" else "") ++ int_to_name k
  show (Dp0 h k)      = (if h == 0 then "·" else "") ++ int_to_name k ++ "₀"
  show (Dp1 h k)      = (if h == 0 then "·" else "") ++ int_to_name k ++ "₁"
  show Era            = "&{}"
  show (Sup l a b)    = "&" ++ int_to_name l ++ "{" ++ show a ++ "," ++ show b ++ "}"
  show (Dup k l v t)  = "!" ++ int_to_name k ++ "&" ++ int_to_name l ++ "=" ++ show v ++ ";" ++ show t
  show (Lam k f)      = "λ" ++ int_to_name k ++ "." ++ show f
  show (App f x)      = "(" ++ show f ++ " " ++ show x ++ ")"
  show Zer            = "0"
  show (Suc n)        = "1+" ++ show n
  show (Swi z s)      = "Λ{0:" ++ show z ++ ";1+:" ++ show s ++ "}"
  show (Ref k)        = "@" ++ int_to_name k
  show (Cal f g m)    = "«" ++ show f ++ "≡" ++ show g ++ concat ["|" ++ int_to_name k ++ "→" ++ show v | (k, v) <- M.toList m] ++ "»"

instance Show Book where
  show (Book m) = unlines [showFunc k ct | (k, ct) <- M.toList m]
    where showFunc k ct = "@" ++ int_to_name k ++ " = " ++ show ct

-- Name Encoding/Decoding
-- ======================

-- Base-64 encoding (for parsing user names/labels and printing)
-- Alphabet: _ (0), a-z (1-26), A-Z (27-52), 0-9 (53-62), $ (63).
alphabet :: String
alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"

char_map :: M.Map Char Int
char_map = M.fromList (zip alphabet [0..])

name_to_int :: String -> Int
name_to_int = foldl' go 0
  where go acc c = (acc `shiftL` 6) + char_map M.! c

int_to_name :: Int -> String
int_to_name 0 = "_"
int_to_name n = reverse $ go n
  where go 0 = ""
        go m = let (q,r) = m `divMod` 64
               in alphabet !! r : go q

-- Parsing
-- =======

lexeme :: ReadP a -> ReadP a
lexeme p = skipSpaces *> p

parse_nam :: ReadP String
parse_nam = lexeme $ munch1 (`M.member` char_map)

parse_term :: ReadP Term
parse_term = do
  base <- parse_term_base
  parse_term_suff base

parse_term_base :: ReadP Term
parse_term_base = lexeme $
      parse_lam_or_swi
  <++ parse_dup
  <++ parse_app
  <++ parse_sup
  <++ parse_era
  <++ parse_zer
  <++ parse_suc
  <++ parse_ref
  <++ parse_var

parse_term_suff :: Term -> ReadP Term
parse_term_suff base = return base

parse_app :: ReadP Term
parse_app = between (lexeme (char '(')) (lexeme (char ')')) $ do
  ts <- many1 parse_term
  let (t:rest) = ts
  return (foldl' App t rest)

parse_lam_or_swi :: ReadP Term
parse_lam_or_swi = do
  lexeme (choice [char 'λ', char 'Λ'])
  parse_swi_body <++ parse_lam_body

parse_lam_body :: ReadP Term
parse_lam_body = do
  k <- parse_nam
  lexeme (char '.')
  body <- parse_term
  return (Lam (name_to_int k) body)

parse_swi_body :: ReadP Term
parse_swi_body = do
  between (lexeme (char '{')) (lexeme (char '}')) $ do
    lexeme (string "0:")
    z <- parse_term
    lexeme (char ';')
    lexeme (string "1+:")
    s <- parse_term
    return (Swi z s)

parse_dup :: ReadP Term
parse_dup = do
  lexeme (char '!')
  k <- parse_nam
  lexeme (char '&')
  l <- parse_nam
  lexeme (char '=')
  v <- parse_term
  lexeme (char ';')
  t <- parse_term
  return (Dup (name_to_int k) (name_to_int l) v t)

parse_sup :: ReadP Term
parse_sup = do
  lexeme (char '&')
  l <- parse_nam
  between (lexeme (char '{')) (lexeme (char '}')) $ do
    a <- parse_term
    lexeme (char ',')
    b <- parse_term
    return (Sup (name_to_int l) a b)

parse_era :: ReadP Term
parse_era = lexeme (string "&{}") >> return Era

parse_ref :: ReadP Term
parse_ref = do
  lexeme (char '@')
  k <- parse_nam
  return (Ref (name_to_int k))

parse_zer :: ReadP Term
parse_zer = lexeme (char '0') >> return Zer

parse_suc :: ReadP Term
parse_suc = do
  lexeme (string "1+")
  n <- parse_term
  return (Suc n)

parse_var :: ReadP Term
parse_var = do
  k <- parse_nam
  let kid = name_to_int k
  choice
    [ string "₀" >> return (Dp0 0 kid)
    , string "₁" >> return (Dp1 0 kid)
    , return (Var 0 kid)
    ]

parse_func :: ReadP (Name, Term)
parse_func = do
  lexeme (char '@')
  k <- parse_nam
  lexeme (char '=')
  f <- parse_term
  return (name_to_int k, f)

parse_book :: ReadP Book
parse_book = do
  skipSpaces
  funcs <- many parse_func
  skipSpaces
  eof
  return $ Book (M.fromList funcs)

read_term :: String -> Term
read_term s = case readP_to_S (parse_term <* skipSpaces <* eof) s of
  [(t, "")] -> t
  _         -> error "bad-parse"

read_book :: String -> Book
read_book s = case readP_to_S parse_book s of
  [(b, "")] -> b
  _         -> error "bad-parse"

-- Environment
-- ===========

data Env = Env
  { env_book    :: !Book
  , env_inters  :: !(IORef Int)
  , env_new_id  :: !(IORef Int)
  , env_var_map :: !(IORef (IM.IntMap Term))
  , env_dp0_map :: !(IORef (IM.IntMap Term))
  , env_dp1_map :: !(IORef (IM.IntMap Term))
  , env_dup_map :: !(IORef (IM.IntMap (Lab, Term)))
  }

new_env :: Book -> IO Env
new_env bk = do
  itr <- newIORef 0
  ids <- newIORef 1
  vm  <- newIORef IM.empty
  d0m <- newIORef IM.empty
  d1m <- newIORef IM.empty
  dm  <- newIORef IM.empty
  return $ Env bk itr ids vm d0m d1m dm

inc_inters :: Env -> IO ()
inc_inters e = do
  !n <- readIORef (env_inters e)
  writeIORef (env_inters e) (n + 1)

fresh :: Env -> IO Name
fresh e = do
  !n <- readIORef (env_new_id e)
  writeIORef (env_new_id e) (n + 1)
  return $! (n `shiftL` 6) + 63

taker :: IORef (IM.IntMap a) -> Name -> IO (Maybe a)
taker ref k = do
  !m <- readIORef ref
  case IM.lookup k m of
    Nothing -> do
      return Nothing
    Just v  -> do
      writeIORef ref (IM.delete k m)
      return (Just v)

take_var :: Env -> Name -> IO (Maybe Term)
take_var e = taker (env_var_map e)

take_dp0 :: Env -> Name -> IO (Maybe Term)
take_dp0 e = taker (env_dp0_map e)

take_dp1 :: Env -> Name -> IO (Maybe Term)
take_dp1 e = taker (env_dp1_map e)

take_dup :: Env -> Name -> IO (Maybe (Lab, Term))
take_dup e = taker (env_dup_map e)

subst_var :: Env -> Name -> Term -> IO ()
subst_var e k v = modifyIORef' (env_var_map e) (IM.insert k v)

subst_dp0 :: Env -> Name -> Term -> IO ()
subst_dp0 e k v = modifyIORef' (env_dp0_map e) (IM.insert k v)

subst_dp1 :: Env -> Name -> Term -> IO ()
subst_dp1 e k v = modifyIORef' (env_dp1_map e) (IM.insert k v)

regis_dup :: Env -> Name -> Lab -> Term -> IO ()
regis_dup e k l v = modifyIORef' (env_dup_map e) (IM.insert k (l, v))

-- WNF: Weak Normal Form
-- =====================

data Frame
  = FApp Term
  | FDp0 Name Lab
  | FDp1 Name Lab

type Stack = [Frame]

wnf :: Env -> Stack -> Term -> IO Term
wnf = wnf_enter

-- WNF: Enter
-- ----------

wnf_enter :: Env -> Stack -> Term -> IO Term

wnf_enter e s (App f x) = do
  putStrLn $ ">> wnf_enter (App)      : " ++ show (App f x)
  wnf_enter e (FApp x : s) f

wnf_enter e s (Var 1 k) = do
  putStrLn $ ">> wnf_enter (Var hot)  : " ++ int_to_name k
  wnf_sub e s k take_var (Var 1)

wnf_enter e s (Dup k l v t) = do
  putStrLn $ ">> wnf_enter (Dup)      : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ show v
  regis_dup e k l v
  wnf_enter e s t

wnf_enter e s (Dp0 1 k) = do
  putStrLn $ ">> wnf_enter (Dp0 hot)  : " ++ int_to_name k ++ "₀"
  mlv <- take_dup e k
  case mlv of
    Just (l, v) -> wnf_enter e (FDp0 k l : s) v
    Nothing     -> wnf_sub e s k take_dp0 (Dp0 1)

wnf_enter e s (Dp1 1 k) = do
  putStrLn $ ">> wnf_enter (Dp1 hot)  : " ++ int_to_name k ++ "₁"
  mlv <- take_dup e k
  case mlv of
    Just (l, v) -> wnf_enter e (FDp1 k l : s) v
    Nothing     -> wnf_sub e s k take_dp1 (Dp1 1)

wnf_enter e s (Ref k) = do
  putStrLn $ ">> wnf_enter (Ref)      : @" ++ int_to_name k
  let (Book m) = env_book e
  case M.lookup k m of
    Just f  -> do
      inc_inters e
      wnf_enter e s (Cal (Nam ("@" ++ int_to_name k)) f M.empty)
    Nothing -> do
      error $ "UndefinedReference: " ++ int_to_name k

wnf_enter e s (Cal f g m) = do
  putStrLn $ ">> wnf_enter (Cal)      : " ++ show (Cal f g m)
  wnf_unwind e s (Cal f g m)

wnf_enter e s f = do
  putStrLn $ ">> wnf_enter (other)    : " ++ show f
  wnf_unwind e s f

-- WNF: Unwind
-- -----------

wnf_unwind :: Env -> Stack -> Term -> IO Term
wnf_unwind e [] v =
  case v of
    Cal f g m -> do
      -- putStrLn $ ">> wnf_unwind (Cal end) : " ++ show (Cal f g m)
      -- !g_wnf <- wnf e [] g
      putStrLn $ ">> wnf_unwind_cal_0     : " ++ show g
      !g_val <- wnf_alloc e m M.empty M.empty g
      putStrLn $ ">> wnf_unwind_cal_1     : " ++ show g_val
      !g_wnf <- wnf e [] g_val
      putStrLn $ ">> wnf_unwind_cal_2     : " ++ show g_wnf
      return $ g_wnf
    f -> do
      putStrLn $ ">> wnf_unwind (end)     : " ++ show f
      return f
wnf_unwind e (frame:s) v = case frame of
  FApp x -> case v of
    Lam fk ff    -> do
      putStrLn $ ">> wnf_unwind (app-lam) : " ++ show (App (Lam fk ff) x)
      wnf_app_lam e s fk ff x
    Sup fl fa fb -> do
      putStrLn $ ">> wnf_unwind (app-sup) : " ++ show (App (Sup fl fa fb) x)
      wnf_app_sup e s fl fa fb x
    Cal f g m    -> do
      putStrLn $ ">> wnf_unwind (app-cal) : " ++ show (App (Cal f g m) x)
      wnf_app_cal e s f g m x
    f            -> do
      putStrLn $ ">> wnf_unwind (app-oth) : " ++ show (App f x)
      wnf_unwind e s (App f x)
  -- Continuations (Dp0/Dp1 frames) always involve hot variables.
  FDp0 k l -> do
    putStrLn $ ">> wnf_unwind (dp0)     : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ show v
    wnf_unwind_dpn e s k l v (Dp0 1 k)
  FDp1 k l -> do
    putStrLn $ ">> wnf_unwind (dp1)     : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ show v
    wnf_unwind_dpn e s k l v (Dp1 1 k)

-- Helper for Dp0/Dp1 unwind to handle various interactions including cold variable duplication.
wnf_unwind_dpn :: Env -> Stack -> Name -> Lab -> Term -> Term -> IO Term
wnf_unwind_dpn e s k l v t = case v of
  Lam vk vf    -> wnf_dpn_lam e s k l vk vf    t
  Sup vl va vb -> wnf_dpn_sup e s k l vl va vb t
  Cal vf vg m  -> wnf_dpn_cal e s k l vf vg m  t
  Suc vp       -> wnf_dpn_suc e s k l vp       t
  Zer          -> wnf_dpn_zer e s k l          t
  Swi vz vs    -> wnf_dpn_swi e s k l vz vs    t
  Nam n        -> wnf_dpn_nam e s k l n        t
  Var 0 vk     -> wnf_dpn_var e s k l vk       t
  Dp0 0 vk     -> wnf_dpn_dp0 e s k l vk       t
  Dp1 0 vk     -> wnf_dpn_dp1 e s k l vk       t
  val          -> wnf_unwind  e s (Dup k l val t)

-- WNF: Interactions
-- -----------------

-- x | x₀ | x₁
wnf_sub :: Env -> Stack -> Name -> (Env -> Name -> IO (Maybe Term)) -> (Name -> Term) -> IO Term
wnf_sub e s k takeFunc mkTerm = do
  mt <- takeFunc e k
  case mt of
    Just t  -> do
      putStrLn $ ">> wnf_sub (found)      : " ++ int_to_name k ++ " = " ++ show t
      wnf e s t
    Nothing -> do
      putStrLn $ ">> wnf_sub (not found)  : " ++ int_to_name k
      wnf_unwind e s (Nam (int_to_name k))

-- (λx.f a)
wnf_app_lam :: Env -> Stack -> Name -> Term -> Term -> IO Term
wnf_app_lam e s fx ff v = do
  putStrLn $ ">> wnf_app_lam          : λ" ++ int_to_name fx ++ "." ++ show ff ++ " <- " ++ show v
  inc_inters e
  subst_var e fx v
  wnf e s ff

-- (&L{f,g} a)
wnf_app_sup :: Env -> Stack -> Lab -> Term -> Term -> Term -> IO Term
wnf_app_sup e s fL fa fb v = do
  putStrLn $ ">> wnf_app_sup          : &" ++ int_to_name fL ++ "{" ++ show fa ++ "," ++ show fb ++ "} " ++ show v
  inc_inters e
  x <- fresh e
  regis_dup e x fL v
  -- Created duplication pairs are hot.
  wnf e s (Sup fL (App fa (Dp0 1 x)) (App fb (Dp1 1 x)))

-- ! F &L = λx.f
wnf_dpn_lam :: Env -> Stack -> Name -> Lab -> Name -> Term -> Term -> IO Term
wnf_dpn_lam e s k l vk vf t = do
  putStrLn $ ">> wnf_dpn_lam          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = λ" ++ int_to_name vk ++ "." ++ show vf
  inc_inters e
  x0 <- fresh e
  x1 <- fresh e
  g  <- fresh e
  -- Created lambdas and variables/pairs are hot.
  subst_dp0 e k (Lam x0 (Dp0 1 g))
  subst_dp1 e k (Lam x1 (Dp1 1 g))
  subst_var e vk (Sup l (Var 1 x0) (Var 1 x1))
  regis_dup e g l vf
  wnf e s t

-- ! X &L = &R{a,b}
wnf_dpn_sup :: Env -> Stack -> Name -> Lab -> Lab -> Term -> Term -> Term -> IO Term
wnf_dpn_sup e s k l vl va vb t
  | l == vl = do
      putStrLn $ ">> wnf_dpn_sup (same)   : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = &" ++ int_to_name vl ++ "{" ++ show va ++ "," ++ show vb ++ "}"
      inc_inters e
      subst_dp0 e k va
      subst_dp1 e k vb
      wnf e s t
  | otherwise = do
      putStrLn $ ">> wnf_dpn_sup (diff)   : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = &" ++ int_to_name vl ++ "{" ++ show va ++ "," ++ show vb ++ "}"
      inc_inters e
      a <- fresh e
      b <- fresh e
      -- Created duplication pairs are hot.
      subst_dp0 e k (Sup vl (Dp0 1 a) (Dp0 1 b))
      subst_dp1 e k (Sup vl (Dp1 1 a) (Dp1 1 b))
      regis_dup e a l va
      regis_dup e b l vb
      wnf e s t

-- ! X &L = 0
wnf_dpn_zer :: Env -> Stack -> Name -> Lab -> Term -> IO Term
wnf_dpn_zer e s k l t = do
  putStrLn $ ">> wnf_dpn_zer          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = 0"
  inc_inters e
  subst_dp0 e k Zer
  subst_dp1 e k Zer
  wnf e s t

-- ! X &L = 1+n
wnf_dpn_suc :: Env -> Stack -> Name -> Lab -> Term -> Term -> IO Term
wnf_dpn_suc e s k l p t = do
  putStrLn $ ">> wnf_dpn_suc          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = 1+" ++ show p
  inc_inters e
  n <- fresh e
  regis_dup e n l p
  -- Created duplication pairs are hot.
  subst_dp0 e k (Suc (Dp0 1 n))
  subst_dp1 e k (Suc (Dp1 1 n))
  wnf e s t

-- ! X &L = Λ{0:z;1+:s}
wnf_dpn_swi :: Env -> Stack -> Name -> Lab -> Term -> Term -> Term -> IO Term
wnf_dpn_swi e s k l vz vs t = do
  putStrLn $ ">> wnf_dpn_swi          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = Λ{0:" ++ show vz ++ ";1+:" ++ show vs ++ "}"
  inc_inters e
  z <- fresh e
  sc <- fresh e
  regis_dup e z l vz
  regis_dup e sc l vs
  -- Created duplication pairs are hot.
  subst_dp0 e k (Swi (Dp0 1 z) (Dp0 1 sc))
  subst_dp1 e k (Swi (Dp1 1 z) (Dp1 1 sc))
  wnf e s t

-- ! X &L = Nam(N)
wnf_dpn_nam :: Env -> Stack -> Name -> Lab -> String -> Term -> IO Term
wnf_dpn_nam e s k l n t = do
  putStrLn $ ">> wnf_dpn_nam          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ n
  inc_inters e
  subst_dp0 e k (Nam n)
  subst_dp1 e k (Nam n)
  wnf e s t

-- ! X &L = (0 k) (Cold Var)
wnf_dpn_var :: Env -> Stack -> Name -> Lab -> Name -> Term -> IO Term
wnf_dpn_var e s k l vk t = do
  putStrLn $ ">> wnf_dpn_var          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ int_to_name vk
  inc_inters e
  -- The duplicated variables remain cold (structural copy).
  subst_dp0 e k (Var 0 vk)
  subst_dp1 e k (Var 0 vk)
  wnf e s t

-- ! X &L = (0 k₀) (Cold Dp0)
wnf_dpn_dp0 :: Env -> Stack -> Name -> Lab -> Name -> Term -> IO Term
wnf_dpn_dp0 e s k l vk t = do
  putStrLn $ ">> wnf_dpn_dp0          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ int_to_name vk ++ "₀"
  inc_inters e
  -- The duplicated pairs remain cold.
  subst_dp0 e k (Dp0 0 vk)
  subst_dp1 e k (Dp0 0 vk)
  wnf e s t

-- ! X &L = (0 k₁) (Cold Dp1)
wnf_dpn_dp1 :: Env -> Stack -> Name -> Lab -> Name -> Term -> IO Term
wnf_dpn_dp1 e s k l vk t = do
  putStrLn $ ">> wnf_dpn_dp1          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ int_to_name vk ++ "₁"
  inc_inters e
  -- The duplicated pairs remain cold.
  subst_dp0 e k (Dp1 0 vk)
  subst_dp1 e k (Dp1 0 vk)
  wnf e s t

-- ((f ~> g % m) a)
wnf_app_cal :: Env -> Stack -> Term -> Term -> M.Map Name Term -> Term -> IO Term
wnf_app_cal e s f g m a = do
  putStrLn $ ">> wnf_app_cal          : " ++ show (Cal f g m) ++ " " ++ show a
  -- We must WNF g first, as it might be a hot DP reducing to a structure (like Lam or Swi).
  !g_wnf <- wnf e [] g
  putStrLn $ ">> wnf_app_cal (g_wnf)  : " ++ show g_wnf
  case g_wnf of
    Lam fx ff -> wnf_app_cal_lam e s f fx ff m a
    Swi fz fs -> wnf_app_cal_swi e s f fz fs m a
    _         -> wnf_unwind e s (App (Cal f g m) a)

-- ((f ~> λx.g % m) a) -> (f x) ~> g % {m, x→a}
wnf_app_cal_lam :: Env -> Stack -> Term -> Name -> Term -> M.Map Name Term -> Term -> IO Term
wnf_app_cal_lam e s f x g m a = do
  putStrLn $ ">> wnf_app_cal_lam      : " ++ show (Cal f (Lam x g) m) ++ " " ++ show a
  inc_inters e
  -- We use a cold variable (Var 0 x) inside the new Cal term, referring to the book name.
  wnf_enter e s (Cal (App f (Var 0 x)) g (M.insert x a m))

-- ((f ~> Λ{0:z;1+:s} % m) a)
wnf_app_cal_swi :: Env -> Stack -> Term -> Term -> Term -> M.Map Name Term -> Term -> IO Term
wnf_app_cal_swi e s f z sc m a = do
  putStrLn $ ">> wnf_app_cal_swi      : " ++ show (Cal f (Swi z sc) m) ++ " " ++ show a
  !a_wnf <- wnf e [] a
  putStrLn $ ">> wnf_app_cal_swi (a)  : " ++ show a ++ " → " ++ show a_wnf
  case a_wnf of
    Zer       -> wnf_app_cal_swi_zer e s f z m
    Suc n     -> wnf_app_cal_swi_suc e s f sc m n
    Sup l b c -> wnf_app_cal_swi_sup e s f z sc m l b c
    Nam k     -> wnf_unwind e s (App f (Nam k))
    _         -> wnf_unwind e s (App (Cal f (Swi z sc) m) a_wnf)

-- ((f ~> Λ{...} % m) 0) -> (f 0) ~> z % m
wnf_app_cal_swi_zer :: Env -> Stack -> Term -> Term -> M.Map Name Term -> IO Term
wnf_app_cal_swi_zer e s f z m = do
  putStrLn $ ">> wnf_app_cal_swi_zer  : " ++ show (Cal (App f Zer) z m)
  inc_inters e
  wnf_enter e s (Cal (App f Zer) z m)

-- ((f ~> Λ{...} % m) 1+n) -> ((λp.(f 1+p) ~> s % m) n)
wnf_app_cal_swi_suc :: Env -> Stack -> Term -> Term -> M.Map Name Term -> Term -> IO Term
wnf_app_cal_swi_suc e s f sc m n = do
  putStrLn $ ">> wnf_app_cal_swi_suc  : " ++ show (Cal f (Swi Zer sc) m) ++ " 1+" ++ show n
  inc_inters e
  p <- fresh e
  -- The lambda created here is a runtime lambda, so its variable is hot.
  let cal_term = Cal (Lam p (App f (Suc (Var 1 p)))) sc m
  putStrLn $ ">> wnf_app_cal_swi_suc  : " ++ show (App cal_term n)
  wnf_enter e s (App cal_term n)

-- ((f ~> Λ{...} % m) &L{a,b})
wnf_app_cal_swi_sup :: Env -> Stack -> Term -> Term -> Term -> M.Map Name Term -> Lab -> Term -> Term -> IO Term
wnf_app_cal_swi_sup e s f z sc m l a b = do
  putStrLn $ ">> wnf_app_cal_swi_sup  : " ++ show (Cal f (Swi z sc) m) ++ " &" ++ int_to_name l ++ "{" ++ show a ++ "," ++ show b ++ "}"
  inc_inters e

  -- 1. Duplicate the context f (the caller), z (zero case), and s (succ case).
  f' <- fresh e
  z' <- fresh e
  s' <- fresh e
  regis_dup e f' l f
  regis_dup e z' l z
  regis_dup e s' l sc

  -- 2. Duplicate the resources (the substitution map m).
  (m0, m1) <- dup_substs e l m

  -- The duplication pairs created here are hot because they connect the duplicated Cal terms to the runtime.
  -- This allows the WNF-Alloc-WNF strategy to resolve these dynamic duplications.
  let swi0 = Swi (Dp0 1 z') (Dp0 1 s')
  let swi1 = Swi (Dp1 1 z') (Dp1 1 s')
  let app0 = App (Cal (Dp0 1 f') swi0 m0) a
  let app1 = App (Cal (Dp1 1 f') swi1 m1) b
  putStrLn $ ">> wnf_app_cal_swi_sup  : &" ++ int_to_name l ++ "{" ++ show app0 ++ "," ++ show app1 ++ "}"
  wnf_enter e s (Sup l app0 app1)

-- ! &L X = f ~> g % m
wnf_dpn_cal :: Env -> Stack -> Name -> Lab -> Term -> Term -> M.Map Name Term -> Term -> IO Term
wnf_dpn_cal e s k l f g m t = do
  putStrLn $ ">> wnf_dpn_cal          : !" ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ show (Cal f g m)
  inc_inters e

  -- 1. Duplicate f.
  f' <- fresh e
  regis_dup e f' l f

  -- 2. Duplicate g.
  g' <- fresh e
  regis_dup e g' l g

  -- 3. Duplicate the resources m.
  (m0, m1) <- dup_substs e l m

  -- The duplication pairs created here are hot.
  let cal0 = Cal (Dp0 1 f') (Dp0 1 g') m0
  let cal1 = Cal (Dp1 1 f') (Dp1 1 g') m1

  subst_dp0 e k cal0
  subst_dp1 e k cal1
  wnf_enter e s t

-- WNF: Utils
-- ----------

-- Duplicates the substitution map by duplicating every term inside it.
dup_substs :: Env -> Lab -> M.Map Name Term -> IO (M.Map Name Term, M.Map Name Term)
dup_substs e l m = do
  let binds = M.toList m
  binds' <- mapM (dup_bind e l) binds
  let (binds0, binds1) = unzip binds'
  return (M.fromList binds0, M.fromList binds1)

dup_bind :: Env -> Lab -> (Name, Term) -> IO ((Name, Term), (Name, Term))
dup_bind e l (k, v) = do
  v' <- fresh e
  regis_dup e v' l v
  return ((k, Dp0 1 v'), (k, Dp1 1 v'))

-- WNF: Allocation Helpers
-- -----------------------

-- WNF: Alloc
-- ----------

wnf_alloc :: Env -> M.Map Name Term -> M.Map Name Term -> M.Map Name Term -> Term -> IO Term
wnf_alloc e mV m0 m1 (Var 0 k) = do
  case M.lookup k mV of
    Just v  -> return v
    Nothing -> error $ "unbound var: " ++ int_to_name k

wnf_alloc e mV m0 m1 (Dp0 0 k) = do
  case M.lookup k m0 of
    Just v  -> return v
    Nothing -> error $ "unbound dp0: " ++ int_to_name k

wnf_alloc e mV m0 m1 (Dp1 0 k) = do
  case M.lookup k m1 of
    Just v  -> return v
    Nothing -> error $ "unbound dp1: " ++ int_to_name k

wnf_alloc e mV m0 m1 (Lam k f) = do
  k' <- fresh e
  let mV' = M.insert k (Var 1 k') mV
  f' <- wnf_alloc e mV' m0 m1 f
  return (Lam k' f')

wnf_alloc e mV m0 m1 (Dup k l v t) = do
  v' <- wnf_alloc e mV m0 m1 v
  k' <- fresh e
  let m0' = M.insert k (Dp0 1 k') m0
  let m1' = M.insert k (Dp1 1 k') m1
  t' <- wnf_alloc e mV m0' m1' t
  return (Dup k' l v' t')

wnf_alloc e mV m0 m1 (App f x) = do
  f' <- wnf_alloc e mV m0 m1 f
  x' <- wnf_alloc e mV m0 m1 x
  return (App f' x')

wnf_alloc e mV m0 m1 (Sup l a b) = do
  a' <- wnf_alloc e mV m0 m1 a
  b' <- wnf_alloc e mV m0 m1 b
  return (Sup l a' b')

wnf_alloc e mV m0 m1 (Suc n) = do
  n' <- wnf_alloc e mV m0 m1 n
  return (Suc n')

wnf_alloc e mV m0 m1 (Swi z s) = do
  z' <- wnf_alloc e mV m0 m1 z
  s' <- wnf_alloc e mV m0 m1 s
  return (Swi z' s')

wnf_alloc e mV m0 m1 v@(Dp0 1 _) = do
  v' <- wnf e [] v
  wnf_alloc e mV m0 m1 v'

wnf_alloc e mV m0 m1 v@(Dp1 1 _) = do
  v' <- wnf e [] v
  wnf_alloc e mV m0 m1 v'

wnf_alloc e mV m0 m1 (Cal f g m) = do
  error "unreachable: Cal should not appear in cold terms being allocated"

wnf_alloc e mV m0 m1 t = do
  return t

-- Normalization
-- =============

nf :: Env -> Int -> Term -> IO Term
nf e d x = do { !x0 <- wnf e [] x ; go e d x0 } where
  go :: Env -> Int -> Term -> IO Term
  go e d (Nam k) = do
    return $ Nam k
  -- Variables during normalization should be hot.
  go e d (Var 1 k) = do
    return $ Var 1 k
  go e d (Dp0 1 k) = do
    return $ Dp0 1 k
  go e d (Dp1 1 k) = do
    return $ Dp1 1 k
  -- Cold variables should not exist in normalized terms (WNF).
  go e d (Var 0 k) = error $ "Internal error: Cold Var found in normalized term: " ++ int_to_name k
  go e d (Dp0 0 k) = error $ "Internal error: Cold Dp0 found in normalized term: " ++ int_to_name k
  go e d (Dp1 0 k) = error $ "Internal error: Cold Dp1 found in normalized term: " ++ int_to_name k
  go e d Era = do
    return Era
  go e d (App f x) = do
    !f0 <- nf e d f
    !x0 <- nf e d x
    return $ App f0 x0
  go e d (Sup l a b) = do
    !a0 <- nf e d a
    !b0 <- nf e d b
    return $ Sup l a0 b0
  go e d (Lam k f) = do
    subst_var e k (Nam (int_to_name d))
    !f0 <- nf e (d + 1) f
    return $ Lam d f0
  go e d (Dup k l v t) = do
    !v0 <- nf e d v
    subst_dp0 e k (Nam (int_to_name d))
    subst_dp1 e k (Nam (int_to_name d))
    !t0 <- nf e (d + 1) t
    return $ Dup d l v0 t0
  go e d Zer = do
    return Zer
  go e d (Suc n) = do
    !n0 <- nf e d n
    return $ Suc n0
  go e d (Swi z s) = do
    !z0 <- nf e d z
    !s0 <- nf e d s
    return $ Swi z0 s0
  go e d (Ref k) = do
    return $ Ref k
  -- Cal should have been eliminated by wnf. If stuck, normalize the caller.
  go e d (Cal f g m) = do
    !f0 <- nf e d f
    return f0

-- Benchmark term generator
-- =========================

-- Generates the church-encoded exponentiation benchmark term.
f :: Int -> String
f n = "λf. " ++ dups ++ final where
  dups  = concat [dup i | i <- [0..n-1]]
  dup 0 = "!F00 &A = f;\n    "
  dup i = "!F" ++ pad i ++ " &A = λx" ++ pad (i-1) ++ ".(F" ++ pad (i-1) ++ "₀ (F" ++ pad (i-1) ++ "₁ x" ++ pad (i-1) ++ "));\n    "
  final = "λx" ++ pad (n-1) ++ ".(F" ++ pad (n-1) ++ "₀ (F" ++ pad (n-1) ++ "₁ x" ++ pad (n-1) ++ "))"
  pad x = if x < 10 then "0" ++ show x else show x

-- Main
-- ====

-- TODO: the test function receives:
-- - the source of a Book
-- - the source of a Term
-- And parses the book, the term, then runs
-- Exactly like the 'main' below
run :: String -> String -> IO ()
run book_src term_src = do
  let book = read_book book_src
  let term = read_term term_src -- Parsed term is cold.
  !env <- new_env book
  -- We must allocate the input term to make it hot before starting WNF.
  !hot <- wnf_alloc env M.empty M.empty M.empty term
  !ini <- getCPUTime
  !nf0 <- nf env 1 hot
  -- !nf1 <- nf env 1 nf0
  !end <- getCPUTime
  !itr <- readIORef (env_inters env)
  let diff = fromIntegral (end - ini) / (10^12)
  let rate = fromIntegral itr / diff
  putStrLn $ show nf0
  putStrLn $ "- Itrs: " ++ show itr ++ " interactions"
  printf "- Time: %.3f seconds\n" (diff :: Double)
  printf "- Perf: %.2f M interactions/s\n" (rate / 1000000 :: Double)

book :: String
book = """
  @c_true  = λt. λf. t
  @c_false = λt. λf. f
  @c_not   = λb. λt. λf. (b f t)

  @id  = λa.a
  @not = Λ{0:1+0;1+:λp.0}
  @dbl = Λ{0:0;1+:λp.1+1+(@dbl p)}
  @and = Λ{0:Λ{0:0;1+:λp.0};1+:λp.Λ{0:0;1+:λp.1+0}}
  @add = Λ{0:λb.b;1+:λa.λb.1+(@add a b)}
  @sum = Λ{0:0;1+:λp.!P&S=p;1+(@add P₀ (@sum P₁))}


  @foo = λx.Λ{0:x;1+:λp.x}
"""


main :: IO ()
-- main = run book $ "((" ++ f 4 ++ " λX.((X λT0.λF0.F0) λT1.λF1.T1)) λT2.λF2.T2)" -- λa.λb.a
-- main = run book "λx.(@dbl 1+1+x)" -- λa.1+1+1+1+(@dbl a)
-- main = run book "(@not 0)" -- 1+0
-- main = run book "(@not 1+0)" -- 0
-- main = run book "(@and 0 0)" -- 0
-- main = run book "(@and &L{0,1+0} 1+0)" -- &L{0,1+0}
-- main = run book "(@and &L{1+0,0} 1+0)" -- &L{1+0,0}
-- main = run book "(@and 1+0 &L{0,1+0})" -- &L{0,1+0}
-- main = run book "(@and 1+0 &L{1+0,0})" -- &L{1+0,0}
-- main = run book "λx.(@and 0 x)" -- λa.((^@and 0) ^a)
-- main = run book "λx.(@and x 0)" -- λa.((^@and ^a) 0)
-- main = run book "(@sum 1+1+1+0)" -- 1+1+1+1+1+1+0
-- main = run book "λx.(@sum 1+1+1+x)"
-- main = run book "λa.(@foo a 1+0)" -- λa.a
-- main = run book "λa.(@foo a &L{0,1+0})" -- "λa.&L{a,a}"
-- main = run book "(@foo 0 &L{0,1+0})" -- "&L{0,0}"
main = run book "! F & L = @id; λx.(F₀ x)" -- λa.a
-- main = run book "! F & L = @id; !G & L = F₀; λx.(G₁ x)" -- λa.^a

-- PROBLEM: the file above outputs


-- [1 of 2] Compiling Main             ( main.hs, main.o ) [Source file changed]
-- [2 of 2] Linking .tmp_hs [Objects changed]
-- >> wnf_enter (App)      : (@sum 1+1+1+0)
-- >> wnf_enter (Ref)      : @sum
-- >> wnf_enter (Cal)      : @sum~>Λ{0:0;1+:λp.!P&S=p;1+((@add P₀) (@sum P₁))}%{}
-- >> wnf_unwind (app-cal) : (@sum~>Λ{0:0;1+:λp.!P&S=p;1+((@add P₀) (@sum P₁))}%{} 1+1+1+0)
-- >> wnf_app_cal          : @sum~>Λ{0:0;1+:λp.!P&S=p;1+((@add P₀) (@sum P₁))}%{} 1+1+1+0
-- >> wnf_enter (other)    : Λ{0:0;1+:λp.!P&S=p;1+((@add P₀) (@sum P₁))}
-- >> wnf_unwind (end)     : Λ{0:0;1+:λp.!P&S=p;1+((@add P₀) (@sum P₁))}
-- >> wnf_app_cal (g_wnf)  : Λ{0:0;1+:λp.!P&S=p;1+((@add P₀) (@sum P₁))}
-- >> wnf_app_cal_swi      : @sum~>Λ{0:0;1+:λp.!P&S=p;1+((@add P₀) (@sum P₁))}%{} 1+1+1+0
-- >> wnf_enter (other)    : 1+1+1+0
-- >> wnf_unwind (end)     : 1+1+1+0
-- >> wnf_app_cal_swi (a)  : 1+1+1+0 → 1+1+1+0
-- >> wnf_app_cal_swi_suc  : @sum~>Λ{0:0;1+:λp.!P&S=p;1+((@add P₀) (@sum P₁))}%{} 1+1+1+0
-- >> wnf_app_cal_swi_suc  : (λa$.(@sum 1+a$)~>λp.!P&S=p;1+((@add P₀) (@sum P₁))%{} 1+1+0)
-- >> wnf_enter (App)      : (λa$.(@sum 1+a$)~>λp.!P&S=p;1+((@add P₀) (@sum P₁))%{} 1+1+0)
-- >> wnf_enter (Cal)      : λa$.(@sum 1+a$)~>λp.!P&S=p;1+((@add P₀) (@sum P₁))%{}
-- >> wnf_unwind (app-cal) : (λa$.(@sum 1+a$)~>λp.!P&S=p;1+((@add P₀) (@sum P₁))%{} 1+1+0)
-- >> wnf_app_cal          : λa$.(@sum 1+a$)~>λp.!P&S=p;1+((@add P₀) (@sum P₁))%{} 1+1+0
-- >> wnf_enter (other)    : λp.!P&S=p;1+((@add P₀) (@sum P₁))
-- >> wnf_unwind (end)     : λp.!P&S=p;1+((@add P₀) (@sum P₁))
-- >> wnf_app_cal (g_wnf)  : λp.!P&S=p;1+((@add P₀) (@sum P₁))
-- >> wnf_app_cal_lam      : λa$.(@sum 1+a$)~>λp.!P&S=p;1+((@add P₀) (@sum P₁))%{} 1+1+0
-- >> wnf_enter (Cal)      : (λa$.(@sum 1+a$) p)~>!P&S=p;1+((@add P₀) (@sum P₁))%{p→1+1+0}
-- >> wnf_unwind (Cal end) : (λa$.(@sum 1+a$) p)~>!P&S=p;1+((@add P₀) (@sum P₁))%{p→1+1+0}
-- >> wnf_enter (Dup)      : !P &S = p
-- >> wnf_enter (other)    : 1+((@add P₀) (@sum P₁))
-- >> wnf_unwind (end)     : 1+((@add P₀) (@sum P₁))
-- >> wnf_unwind (g_wnf)   : 1+((@add P₀) (@sum P₁))
-- >> wnf_unwind (g_hot)   : 1+((@add P₀) (@sum P₁))
-- >> wnf_enter (other)    : 1+((@add P₀) (@sum P₁))
-- >> wnf_unwind (end)     : 1+((@add P₀) (@sum P₁))
-- >> wnf_unwind (g_done)  : 1+((@add P₀) (@sum P₁))
-- >> wnf_enter (App)      : ((@add P₀) (@sum P₁))
-- >> wnf_enter (App)      : (@add P₀)
-- >> wnf_enter (Ref)      : @add
-- >> wnf_enter (Cal)      : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{}
-- >> wnf_unwind (app-cal) : (@add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀)
-- >> wnf_app_cal          : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀
-- >> wnf_enter (other)    : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_unwind (end)     : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_app_cal (g_wnf)  : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_app_cal_swi      : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀
-- >> wnf_enter (other)    : P₀
-- >> wnf_unwind (end)     : P₀
-- >> wnf_app_cal_swi (a)  : P₀ → P₀
-- >> wnf_unwind (app-oth) : ((@add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀) (@sum P₁))
-- >> wnf_unwind (end)     : ((@add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀) (@sum P₁))
-- >> wnf_enter (App)      : (@add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀)
-- >> wnf_enter (Cal)      : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{}
-- >> wnf_unwind (app-cal) : (@add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀)
-- >> wnf_app_cal          : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀
-- >> wnf_enter (other)    : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_unwind (end)     : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_app_cal (g_wnf)  : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_app_cal_swi      : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀
-- >> wnf_enter (other)    : P₀
-- >> wnf_unwind (end)     : P₀
-- >> wnf_app_cal_swi (a)  : P₀ → P₀
-- >> wnf_unwind (end)     : (@add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} P₀)
-- >> wnf_enter (Cal)      : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{}
-- >> wnf_unwind (Cal end) : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{}
-- >> wnf_enter (other)    : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_unwind (end)     : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_unwind (g_wnf)   : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_unwind (g_hot)   : Λ{0:λb$.b$;1+:λc$.λd$.1+((@add c$) d$)}
-- >> wnf_enter (other)    : Λ{0:λb$.b$;1+:λc$.λd$.1+((@add c$) d$)}
-- >> wnf_unwind (end)     : Λ{0:λb$.b$;1+:λc$.λd$.1+((@add c$) d$)}
-- >> wnf_unwind (g_done)  : Λ{0:λb$.b$;1+:λc$.λd$.1+((@add c$) d$)}
-- >> wnf_enter (other)    : λb$.b$
-- >> wnf_unwind (end)     : λb$.b$
-- >> wnf_enter (Var hot)  : b$
-- >> wnf_sub (found)      : b$ = a
-- >> wnf_enter (other)    : a
-- >> wnf_unwind (end)     : a
-- >> wnf_enter (other)    : λc$.λd$.1+((@add c$) d$)
-- >> wnf_unwind (end)     : λc$.λd$.1+((@add c$) d$)
-- >> wnf_enter (other)    : λd$.1+((@add c$) d$)
-- >> wnf_unwind (end)     : λd$.1+((@add c$) d$)
-- >> wnf_enter (other)    : 1+((@add c$) d$)
-- >> wnf_unwind (end)     : 1+((@add c$) d$)
-- >> wnf_enter (App)      : ((@add c$) d$)
-- >> wnf_enter (App)      : (@add c$)
-- >> wnf_enter (Ref)      : @add
-- >> wnf_enter (Cal)      : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{}
-- >> wnf_unwind (app-cal) : (@add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} c$)
-- >> wnf_app_cal          : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} c$
-- >> wnf_enter (other)    : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_unwind (end)     : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_app_cal (g_wnf)  : Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}
-- >> wnf_app_cal_swi      : @add~>Λ{0:λb.b;1+:λa.λb.1+((@add a) b)}%{} c$
-- >> wnf_enter (Var hot)  : c$
-- >> wnf_sub (found)      : c$ = a
-- >> wnf_enter (other)    : a
-- >> wnf_unwind (end)     : a
-- >> wnf_app_cal_swi (a)  : c$ → a
-- >> wnf_unwind (app-oth) : ((@add a) d$)
-- >> wnf_unwind (end)     : ((@add a) d$)
-- >> wnf_enter (App)      : (@add a)
-- >> wnf_enter (other)    : @add
-- >> wnf_unwind (app-oth) : (@add a)
-- >> wnf_unwind (end)     : (@add a)
-- >> wnf_enter (other)    : @add
-- >> wnf_unwind (end)     : @add
-- >> wnf_enter (other)    : a
-- >> wnf_unwind (end)     : a
-- >> wnf_enter (Var hot)  : d$
-- >> wnf_sub (found)      : d$ = b
-- >> wnf_enter (other)    : b
-- >> wnf_unwind (end)     : b
-- >> wnf_enter (other)    : P₀
-- >> wnf_unwind (end)     : P₀
-- .tmp_hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

-- Internal error: Cold Dp0 found in normalized term: P

-- this seems wrong - cold dp's are NOT supposed to be on normalized terms.
-- that's because book terms are bought to the runtime via the wnf_ref
-- interaction, which creates `F ~> G` where F and G are book terms (i.e.,
-- including cold dps). yet, once the wnf of `~>` concludes, we will have picked
-- one of the sides (F or G), and applied wnf_alloc to it. finally, the
-- wnf_alloc function gets rid of ALL cold vars/dps on the term, replacing by
-- substitutions. thus, we should NEVER see a cold dup on the normal form of
-- a program! yet, the example above clearly breaks this invariant. why?
-- write your explanation below:

-- Because the Dup inside the book term is executed during the first WNF phase, the
-- binder is removed and only its projections remain, still cold.

-- Concretely, in the suc branch of @sum we reduce
  -- λp. !P &S = p; 1+((@add P₀) (@sum P₁))
-- to WNF before allocation. This fires the dup, registers P ↦ (S, p) in
-- env_dup_map, and continues with the body
  -- 1+((@add P₀) (@sum P₁))
-- which contains the cold projections P₀ and P₁. At Cal-end we then call wnf_alloc
-- on this cold body, but wnf_alloc has no case for Dp0 0/Dp1 0, so it leaves P₀/P₁
-- cold. The allocated term returned by Cal thus still contains cold projections,
-- which then escape to the runtime; later, nf complains with “Cold Dp0 found...”.

-- This happens because of the WNF-Alloc-WNF schedule: Dup is executed before
-- allocation, so the binder P is gone by the time alloc runs; alloc’s m_dup table
-- is empty and (since it neither handles cold Dp0/Dp1 nor consults env_dup_map) it
-- cannot turn P₀/P₁ into hot projections. On top of that, the value stored in
-- env_dup_map for P is the cold variable p (not passed through the Cal
-- substitution m), so even if we turned projections hot, we would still risk
-- reintroducing cold terms when the dup payload is used.

-- In short: cold projections leak because wnf_alloc does not rewrite Dp0/Dp1 in
-- the cold term, and the dup was already fired before allocation. That is why a
-- cold P₀ reaches normalization and triggers the invariant violation.


-- that makes no sense. what do you mean by 
