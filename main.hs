{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP

--------------------------------------------------------------------------------
-- Raw syntax (String based, parsing only)
--------------------------------------------------------------------------------

type Lab = String
type Name = String

data RawTerm
  = RNam Name
  | RVar Name
  | RDp0 Name
  | RDp1 Name
  | REra
  | RSup Lab RawTerm RawTerm
  | RDup Name Lab RawTerm RawTerm
  | RLam Name RawTerm
  | RAbs Name RawTerm
  | RDry RawTerm RawTerm
  | RApp RawTerm RawTerm
  deriving (Show)

--------------------------------------------------------------------------------
-- Parsed combinators
--------------------------------------------------------------------------------

lexeme :: ReadP a -> ReadP a
lexeme p = skipSpaces *> p

parseName :: ReadP String
parseName = lexeme parseNam

parseNam :: ReadP String
parseNam = munch1 $ \c ->
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c == '_' || c == '/'

parseTerm :: ReadP RawTerm
parseTerm = lexeme $ choice
  [ parseLam
  , parseDup
  , parseApp
  , parseSup
  , parseEra
  , parseVar
  ]

parseApp :: ReadP RawTerm
parseApp = do
  _ <- lexeme (char '(')
  ts <- many1 parseTerm
  _ <- lexeme (char ')')
  case ts of
    (t:rest) -> pure $ foldl' RApp t rest
    _        -> pfail

parseLam :: ReadP RawTerm
parseLam = do
  _ <- lexeme (choice [char 'λ', char '\\'])
  k <- parseName
  _ <- lexeme (char '.')
  body <- parseTerm
  pure $ RLam k body

parseDup :: ReadP RawTerm
parseDup = do
  _ <- lexeme (char '!')
  k <- parseName
  _ <- lexeme (char '&')
  l <- parseName
  _ <- lexeme (char '=')
  v <- parseTerm
  _ <- lexeme (char ';')
  t <- parseTerm
  pure $ RDup k l v t

parseSup :: ReadP RawTerm
parseSup = do
  _ <- lexeme (char '&')
  l <- parseName
  _ <- lexeme (char '{')
  a <- parseTerm
  _ <- lexeme (char ',')
  b <- parseTerm
  _ <- lexeme (char '}')
  pure $ RSup l a b

parseEra :: ReadP RawTerm
parseEra = lexeme (string "&{}") *> pure REra

parseVar :: ReadP RawTerm
parseVar = do
  k <- parseName
  choice
    [ string "₀" *> pure (RDp0 k)
    , string "₁" *> pure (RDp1 k)
    , pure (RVar k)
    ]

readRawTerm :: String -> RawTerm
readRawTerm s =
  case readP_to_S (parseTerm <* skipSpaces <* eof) s of
    [(t, "")] -> t
    _         -> error "bad-parse"

--------------------------------------------------------------------------------
-- Interned representation (Name/Lab -> Int)
--------------------------------------------------------------------------------

type NameId = Int
type LabId  = Int

data Term
  = Nam !NameId
  | Var !NameId
  | Dp0 !NameId
  | Dp1 !NameId
  | Era
  | Sup !LabId !Term !Term
  | Dup !NameId !LabId !Term !Term
  | Lam !NameId !Term
  | Abs !NameId !Term
  | Dry !Term !Term
  | App !Term !Term

data InternState = InternState
  { stMap   :: !(M.Map String Int)
  , stPairs :: ![(Int, String)]
  , stNext  :: !Int
  }

internName :: String -> InternState -> (Int, InternState)
internName k st =
  case M.lookup k (stMap st) of
    Just i  -> (i, st)
    Nothing ->
      let !i    = stNext st
          !m'   = M.insert k i (stMap st)
          !lst' = (i, k) : stPairs st
      in (i, InternState m' lst' (i + 1))

internTerm :: RawTerm -> (Term, NamePool)
internTerm raw =
  let (t, InternState _ names nextId) = go raw (InternState M.empty [] 0)
      pool = mkPool names nextId
  in (t, pool)
  where
    go :: RawTerm -> InternState -> (Term, InternState)
    go term st = case term of
      RNam k       -> let (i, st') = internName k st in (Nam i, st')
      RVar k       -> let (i, st') = internName k st in (Var i, st')
      RDp0 k       -> let (i, st') = internName k st in (Dp0 i, st')
      RDp1 k       -> let (i, st') = internName k st in (Dp1 i, st')
      REra         -> (Era, st)
      RSup l a b   ->
        let (li, st1) = internName l st
            (a', st2) = go a st1
            (b', st3) = go b st2
        in (Sup li a' b', st3)
      RDup k l v t ->
        let (ki, st1) = internName k st
            (li, st2) = internName l st1
            (v', st3) = go v st2
            (t', st4) = go t st3
        in (Dup ki li v' t', st4)
      RLam k body  ->
        let (ki, st1) = internName k st
            (b', st2) = go body st1
        in (Lam ki b', st2)
      RAbs k body  ->
        let (ki, st1) = internName k st
            (b', st2) = go body st1
        in (Abs ki b', st2)
      RDry f x     ->
        let (f', st1) = go f st
            (x', st2) = go x st1
        in (Dry f' x', st2)
      RApp f x     ->
        let (f', st1) = go f st
            (x', st2) = go x st1
        in (App f' x', st2)

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

data Env = Env
  { inters   :: !Int
  , nextName :: !Int
  , varMap   :: !(IM.IntMap Term)
  , dp0Map   :: !(IM.IntMap Term)
  , dp1Map   :: !(IM.IntMap Term)
  , dupMap   :: !(IM.IntMap (LabId, Term))
  }

emptyEnv :: Int -> Env
emptyEnv base = Env
  { inters   = 0
  , nextName = base
  , varMap   = IM.empty
  , dp0Map   = IM.empty
  , dp1Map   = IM.empty
  , dupMap   = IM.empty
  }

incInters :: Env -> Env
incInters s = s { inters = inters s + 1 }

freshName :: Env -> (Env, NameId)
freshName s =
  let !n = nextName s
  in (s { nextName = n + 1 }, n)

freshVar, freshDup :: Env -> (Env, NameId)
freshVar = freshName
freshDup = freshName

insertVar :: NameId -> Term -> Env -> Env
insertVar k v s = s { varMap = IM.insert k v (varMap s) }

insertDp0 :: NameId -> Term -> Env -> Env
insertDp0 k v s = s { dp0Map = IM.insert k v (dp0Map s) }

insertDp1 :: NameId -> Term -> Env -> Env
insertDp1 k v s = s { dp1Map = IM.insert k v (dp1Map s) }

insertDup :: NameId -> (LabId, Term) -> Env -> Env
insertDup k v s = s { dupMap = IM.insert k v (dupMap s) }

takeOnce :: NameId -> IM.IntMap a -> (Maybe a, IM.IntMap a)
takeOnce k = IM.updateLookupWithKey (\_ _ -> Nothing) k

takeVar :: Env -> NameId -> (Maybe Term, Env)
takeVar s k =
  let (mv, mp) = takeOnce k (varMap s)
  in (mv, s { varMap = mp })

takeDp0 :: Env -> NameId -> (Maybe Term, Env)
takeDp0 s k =
  let (mv, mp) = takeOnce k (dp0Map s)
  in (mv, s { dp0Map = mp })

takeDp1 :: Env -> NameId -> (Maybe Term, Env)
takeDp1 s k =
  let (mv, mp) = takeOnce k (dp1Map s)
  in (mv, s { dp1Map = mp })

takeDup :: Env -> NameId -> (Maybe (LabId, Term), Env)
takeDup s k =
  let (mv, mp) = takeOnce k (dupMap s)
  in (mv, s { dupMap = mp })

delayDup :: Env -> NameId -> (LabId, Term) -> Env
delayDup s k pair = insertDup k pair s

--------------------------------------------------------------------------------
-- Weak head normal form
--------------------------------------------------------------------------------

wnf :: Env -> Term -> (Env, Term)
wnf env0 term0 = go env0 term0
  where
    go !s (App f x)     = let (s0, f0) = go s f in app s0 f0 x
    go !s (Dup k l v t) = go (delayDup s k (l, v)) t
    go !s (Var x)       = var s x
    go !s (Dp0 x)       = dp0 s x
    go !s (Dp1 x)       = dp1 s x
    go !s t             = (s, t)

app :: Env -> Term -> Term -> (Env, Term)
app s (Nam fk)       x = appNam s fk x
app s (Dry df dx)    x = appDry s df dx x
app s (Lam fk ff)    x = appLam s fk ff x
app s (Sup fl fa fb) x = appSup s fl fa fb x
app s f              x = (s, App f x)

dup :: Env -> NameId -> LabId -> Term -> Term -> (Env, Term)
dup s k l (Nam vk)       t = dupNam s k l vk t
dup s k l (Dry vf vx)    t = dupDry s k l vf vx t
dup s k l (Lam vk vf)    t = dupLam s k l vk vf t
dup s k l (Sup vl va vb) t = dupSup s k l vl va vb t
dup s k l v              t = (s, Dup k l v t)

-- (λx.f v)
appLam :: Env -> NameId -> Term -> Term -> (Env, Term)
appLam s fx ff v =
  let !s0 = incInters s
      !s1 = insertVar fx v s0
  in wnf s1 ff

-- (&fL{fa,fb} v)
appSup :: Env -> LabId -> Term -> Term -> Term -> (Env, Term)
appSup s fL fa fb v =
  let !s0      = incInters s
      (s1, x)  = freshDup s0
      app0     = App fa (Dp0 x)
      app1     = App fb (Dp1 x)
      supTerm  = Sup fL app0 app1
      dupTerm  = Dup x fL v supTerm
  in wnf s1 dupTerm

-- (fk v)
appNam :: Env -> NameId -> Term -> (Env, Term)
appNam s fk v = (incInters s, Dry (Nam fk) v)

-- ((df dx) v)
appDry :: Env -> Term -> Term -> Term -> (Env, Term)
appDry s df dx v = (incInters s, Dry (Dry df dx) v)

-- ! k &L = λvk.vf; t
dupLam :: Env -> NameId -> LabId -> NameId -> Term -> Term -> (Env, Term)
dupLam s k l vk vf t =
  let !s0        = incInters s
      (s1, x0)   = freshVar s0
      (s2, x1)   = freshVar s1
      (s3, g)    = freshDup s2
      !s4        = insertDp0 k (Lam x0 (Dp0 g)) s3
      !s5        = insertDp1 k (Lam x1 (Dp1 g)) s4
      !s6        = insertVar vk (Sup l (Var x0) (Var x1)) s5
      dupTerm    = Dup g l vf t
  in wnf s6 dupTerm

-- ! k &L = &vL{va,vb}; t
dupSup :: Env -> NameId -> LabId -> LabId -> Term -> Term -> Term -> (Env, Term)
dupSup s k l vl va vb t
  | l == vl =
      let !s0 = incInters s
          !s1 = insertDp0 k va s0
          !s2 = insertDp1 k vb s1
      in wnf s2 t
  | otherwise =
      let !s0       = incInters s
          (s1, a)   = freshDup s0
          (s2, b)   = freshDup s1
          !s3       = insertDp0 k (Sup vl (Dp0 a) (Dp0 b)) s2
          !s4       = insertDp1 k (Sup vl (Dp1 a) (Dp1 b)) s3
          dupTerm   = Dup a l va (Dup b l vb t)
      in wnf s4 dupTerm

-- ! k &L = vk; t
dupNam :: Env -> NameId -> LabId -> NameId -> Term -> (Env, Term)
dupNam s k _ vk t =
  let !s0 = incInters s
      !s1 = insertDp0 k (Nam vk) s0
      !s2 = insertDp1 k (Nam vk) s1
  in wnf s2 t

-- ! k &L = (vf vx); t
dupDry :: Env -> NameId -> LabId -> Term -> Term -> Term -> (Env, Term)
dupDry s k l vf vx t =
  let !s0      = incInters s
      (s1, f)  = freshDup s0
      (s2, x)  = freshDup s1
      !s3      = insertDp0 k (Dry (Dp0 f) (Dp0 x)) s2
      !s4      = insertDp1 k (Dry (Dp1 f) (Dp1 x)) s3
      dupTerm  = Dup f l vf (Dup x l vx t)
  in wnf s4 dupTerm

-- var lookup
var :: Env -> NameId -> (Env, Term)
var s k =
  case takeVar s k of
    (Just t, s0) -> wnf s0 t
    (Nothing, _) -> (s, Var k)

dp0 :: Env -> NameId -> (Env, Term)
dp0 s k =
  case takeDp0 s k of
    (Just t, s0) -> wnf s0 t
    (Nothing, _) ->
      case takeDup s k of
        (Just (l, v), s0) ->
          let (s1, v0) = wnf s0 v
          in dup s1 k l v0 (Dp0 k)
        (Nothing, _) -> (s, Dp0 k)

dp1 :: Env -> NameId -> (Env, Term)
dp1 s k =
  case takeDp1 s k of
    (Just t, s0) -> wnf s0 t
    (Nothing, _) ->
      case takeDup s k of
        (Just (l, v), s0) ->
          let (s1, v0) = wnf s0 v
          in dup s1 k l v0 (Dp1 k)
        (Nothing, _) -> (s, Dp1 k)

--------------------------------------------------------------------------------
-- Normal form
--------------------------------------------------------------------------------

nf :: Env -> Term -> (Env, Term)
nf s x =
  let (s0, x0) = wnf s x
  in go s0 x0
  where
    go !env0 term0 = case term0 of
      Nam k       -> (env0, Nam k)
      Dry f x'    ->
        let (s1, f0) = nf env0 f
            (s2, x0) = nf s1 x'
        in (s2, Dry f0 x0)
      Var k       -> (env0, Var k)
      Dp0 k       -> (env0, Dp0 k)
      Dp1 k       -> (env0, Dp1 k)
      Era         -> (env0, Era)
      Sup l a b   ->
        let (s1, a0) = nf env0 a
            (s2, b0) = nf s1 b
        in (s2, Sup l a0 b0)
      Dup k l v t ->
        let (s1, v0) = nf env0 v
            (s2, t0) = nf s1 t
        in (s2, Dup k l v0 t0)
      Lam k f     ->
        let s1      = insertVar k (Nam k) env0
            (s2, f0) = nf s1 f
        in (s2, Lam k f0)
      Abs k f     ->
        let (s1, f0) = nf env0 f
        in (s1, Abs k f0)
      App f x'    ->
        let (s1, f0) = nf env0 f
            (s2, x0) = nf s1 x'
        in (s2, App f0 x0)

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

data NamePool = NamePool
  { poolNames :: !(IM.IntMap String)
  , poolSize  :: !Int
  }

mkPool :: [(Int, String)] -> Int -> NamePool
mkPool pairs size = NamePool (IM.fromList pairs) size

renderName :: NamePool -> Int -> String
renderName NamePool{poolNames = names, poolSize = base} idx
  | idx < base = IM.findWithDefault ("$" ++ show idx) idx names
  | otherwise  = '$' : encode (idx - base)
  where
    alphabet = ['a'..'z']
    encode :: Int -> String
    encode n =
      let go k
            | k < 26    = [alphabet !! k]
            | otherwise =
                let (q, r) = (k `div` 26, k `mod` 26)
                in go (q - 1) ++ [alphabet !! r]
      in go n

renderTerm :: NamePool -> Term -> String
renderTerm pool term = case term of
  Nam k       -> renderName pool k
  Var k       -> renderName pool k
  Dp0 k       -> renderName pool k ++ "₀"
  Dp1 k       -> renderName pool k ++ "₁"
  Era         -> "&{}"
  Sup l a b   -> "&" ++ renderName pool l ++ "{" ++ renderTerm pool a ++ "," ++ renderTerm pool b ++ "}"
  Dup k l v t ->
    "!" ++ renderName pool k ++ "&" ++ renderName pool l ++ "="
      ++ renderTerm pool v ++ ";" ++ renderTerm pool t
  Lam k f     -> "λ" ++ renderName pool k ++ "." ++ renderTerm pool f
  Abs k f     -> "λ" ++ renderName pool k ++ "." ++ renderTerm pool f
  Dry f x     -> "(" ++ renderTerm pool f ++ " " ++ renderTerm pool x ++ ")"
  App f x     -> "(" ++ renderTerm pool f ++ " " ++ renderTerm pool x ++ ")"

--------------------------------------------------------------------------------
-- Problem setup
--------------------------------------------------------------------------------

f :: Int -> String
f n = "λf. " ++ dups ++ final
  where
    dups  = concat [dup i | i <- [0 .. n - 1]]
    dup 0 = "!F00 &A = f;\n    "
    dup i =
      "!F" ++ pad i ++ " &A = λx" ++ pad (i - 1) ++ ".(F" ++ pad (i - 1) ++ "₀ (F"
      ++ pad (i - 1) ++ "₁ x" ++ pad (i - 1) ++ "));\n    "
    final =
      "λx" ++ pad (n - 1) ++ ".(F" ++ pad (n - 1) ++ "₀ (F" ++ pad (n - 1)
      ++ "₁ x" ++ pad (n - 1) ++ "))"
    pad x = if x < 10 then '0' : show x else show x

initialInput :: String
initialInput = "((" ++ f 20 ++ " λX.((X λT0.λF0.F0) λT1.λF1.T1)) λT2.λF2.T2)"

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let raw             = readRawTerm initialInput
      (term0, pool)   = internTerm raw
      (finalEnv, nfT) = nf (emptyEnv (poolSize pool)) term0
  putStrLn $ renderTerm pool nfT
  print (inters finalEnv)
