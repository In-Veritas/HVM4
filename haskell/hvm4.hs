-- 
-- [>./../README.md<]
-- 
-- [>./main.hs<]
-- 
-- [># LANGUAGE BangPatterns, CPP #<]
-- 
-- import Control.Monad (foldM, forM_, when)
-- import Data.Bits (shiftL)
-- import Data.Char (isDigit)
-- import Data.IORef
-- import Data.List (foldl', elemIndex, intercalate)
-- import System.CPUTime
-- import System.Directory (canonicalizePath)
-- import System.Environment (getArgs)
-- import System.Exit (exitFailure)
-- import System.FilePath ((</>), takeDirectory)
-- import System.IO (hPutStrLn, stderr)
-- import Text.ParserCombinators.ReadP
-- import Text.Printf
-- import qualified Data.IntMap.Strict as IM
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
-- 
-- debug :: Bool
-- debug = False
-- 
-- -- Types
-- -- =====
-- 
-- type Lab  = Int
-- type Name = Int
-- 
-- data Term
--   = Var !Name
--   | Cop !Int !Name
--   | Ref !Name
--   | Nam !String
--   | Dry !Term !Term
--   | Era
--   | Sup !Lab !Term !Term
--   | Dup !Name !Lab !Term !Term
--   | Lam !Name !Term
--   | App !Term !Term
--   | Ctr !Name ![Term]
--   | Mat !Name !Term !Term
--   | Alo ![Name] !Term
--   deriving (Eq)
-- 
-- data Book = Book (M.Map Name Term)
-- 
-- data Declaration
--   = Include FilePath
--   | Define Name Term
-- 
-- data Env = Env
--   { env_book  :: !Book
--   , env_itrs  :: !(IORef Int)
--   , env_fresh :: !(IORef Int)
--   , env_subst :: !(IORef (IM.IntMap Term))
--   , env_dups  :: !(IORef (IM.IntMap (Lab, Term)))
--   }
-- 
-- -- Showing
-- -- =======
-- 
-- instance Show Term where
--   show (Var k)       = int_to_name k
--   show (Cop s k)     = int_to_name k ++ (if s == 0 then "₀" else "₁")
--   show (Ref k)       = "@" ++ int_to_name k
--   show (Nam k)       = k
--   show (Dry f x)     = show_app f x
--   show Era           = "&{}"
--   show (Sup l a b)   = "&" ++ int_to_name l ++ "{" ++ show a ++ "," ++ show b ++ "}"
--   show (Dup k l v t) = "!" ++ int_to_name k ++ "&" ++ int_to_name l ++ "=" ++ show v ++ ";" ++ show t
--   show (Lam k f)     = "λ" ++ int_to_name k ++ "." ++ show f
--   show (App f x)     = show_app f x
--   show (Ctr k xs)    = "#" ++ int_to_name k ++ "{" ++ intercalate "," (map show xs) ++ "}"
--   show (Mat k h m)   = "λ{#" ++ int_to_name k ++ ":" ++ show h ++ ";" ++ show m ++ "}"
--   show (Alo s t)     = "@{" ++ intercalate "," (map int_to_name s) ++ "}" ++ show t
-- 
-- show_app :: Term -> Term -> String
-- show_app f x = case f of
--   App _ _ -> init (show f) ++ "," ++ show x ++ ")"
--   Dry _ _ -> init (show f) ++ "," ++ show x ++ ")"
--   Lam _ _ -> "(" ++ show f ++ ")(" ++ show x ++ ")"
--   _       -> show f ++ "(" ++ show x ++ ")"
-- 
-- instance Show Book where
--   show (Book m) = unlines [ "@" ++ int_to_name k ++ " = " ++ show ct | (k, ct) <- M.toList m ]
-- 
-- show_dups  :: IM.IntMap (Lab, Term) -> String
-- show_dups  m = unlines [ "! " ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ show v | (k, (l, v)) <- IM.toList m ]
-- 
-- show_subst :: IM.IntMap Term -> String
-- show_subst m = unlines [ int_to_name (k `div` 4) ++ suffix (k `mod` 4) ++ " ← " ++ show v | (k, v) <- IM.toList m ]
--   where suffix x = case x of { 0 -> "" ; 1 -> "₀" ; 2 -> "₁" ; _ -> "?" }
-- 
-- -- Name Encoding/Decoding
-- -- ======================
-- 
-- alphabet :: String
-- alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"
-- 
-- alphabet_first :: String
-- alphabet_first = filter (`notElem` "_0123456789") alphabet
-- 
-- name_to_int :: String -> Int
-- name_to_int = foldl' (\acc c -> (acc `shiftL` 6) + idx c) 0
--   where idx c = maybe (error "bad name char") id (elemIndex c alphabet)
-- 
-- int_to_name :: Int -> String
-- int_to_name n | n <  64 = [alphabet !! n]
--               | n >= 64 = int_to_name (n `div` 64) ++ [alphabet !! (n `mod` 64)]
-- 
-- -- Parsing
-- -- =======
-- 
-- parse_lexeme :: ReadP a -> ReadP a
-- parse_lexeme p = skip *> p
-- 
-- skip :: ReadP ()
-- skip = do
--   skipSpaces
--   skip_comment <++ return ()
-- 
-- skip_comment :: ReadP ()
-- skip_comment = do
--   _ <- string "//"
--   _ <- munch (/= '\n')
--   _ <- char '\n' <++ return '\n'
--   skip
-- 
-- parse_name :: ReadP String
-- parse_name = parse_lexeme $ do
--   head <- satisfy (`elem` alphabet_first)
--   tail <- munch (`elem` alphabet)
--   return (head : tail)
-- 
-- parse_term :: ReadP Term
-- parse_term = do
--   t <- parse_term_base
--   parse_term_suff t
-- 
-- parse_term_suff :: Term -> ReadP Term
-- parse_term_suff t = loop <++ return t where
--   loop = do
--     parse_lexeme (char '(')
--     args <- sepBy parse_term (parse_lexeme (char ','))
--     parse_lexeme (char ')')
--     parse_term_suff (foldl' App t args)
-- 
-- parse_term_base :: ReadP Term
-- parse_term_base = parse_lexeme $ choice
--   [ parse_abs
--   , parse_dup
--   , parse_sup
--   , parse_era
--   , parse_ctr
--   , parse_ref
--   , parse_par
--   , parse_nam
--   ]
-- 
-- parse_par :: ReadP Term
-- parse_par = do
--   parse_lexeme (char '(')
--   t <- parse_term
--   parse_lexeme (char ')')
--   return t
-- 
-- parse_abs :: ReadP Term
-- parse_abs = do
--   parse_lexeme (char 'λ')
--   choice [ parse_mat , parse_lam ]
-- 
-- parse_lam :: ReadP Term
-- parse_lam = do
--   k <- parse_name
--   parse_lexeme (char '.')
--   t <- parse_term
--   return (Lam (name_to_int k) t)
-- 
-- parse_mat :: ReadP Term
-- parse_mat = do
--   parse_lexeme (char '{')
--   parse_mat_body
--  where
--   parse_mat_body = parse_mat_case <++ parse_mat_default
-- 
--   parse_mat_case = do
--     parse_lexeme (char '#')
--     k <- parse_name
--     parse_lexeme (char ':')
--     h <- parse_term
--     optional (parse_lexeme (char ';'))
--     m <- (parse_lexeme (char '}') >> return Era) <++ parse_mat_body
--     return (Mat (name_to_int k) h m)
-- 
--   parse_mat_default = do
--     t <- parse_term
--     parse_lexeme (char '}')
--     return t
-- 
-- parse_dup :: ReadP Term
-- parse_dup = do
--   parse_lexeme (char '!')
--   k <- parse_name
--   parse_lexeme (char '&')
--   l <- parse_name
--   parse_lexeme (char '=')
--   v <- parse_term
--   optional (parse_lexeme (char ';'))
--   t <- parse_term
--   return (Dup (name_to_int k) (name_to_int l) v t)
-- 
-- parse_sup :: ReadP Term
-- parse_sup = do
--   parse_lexeme (char '&')
--   l <- parse_name
--   between (parse_lexeme (char '{')) (parse_lexeme (char '}')) $ do
--     a <- parse_term
--     optional (parse_lexeme (char ','))
--     b <- parse_term
--     return (Sup (name_to_int l) a b)
-- 
-- parse_era :: ReadP Term
-- parse_era = parse_lexeme (string "&{}") >> return Era
-- 
-- parse_ref :: ReadP Term
-- parse_ref = do
--   parse_lexeme (char '@')
--   k <- parse_name
--   return (Ref (name_to_int k))
-- 
-- parse_nam :: ReadP Term
-- parse_nam = do
--   k <- parse_name
--   choice [ parse_cop_0 k , parse_cop_1 k , parse_var k ]
-- 
-- parse_cop_0 :: String -> ReadP Term
-- parse_cop_0 k = do
--   _ <- string "₀"
--   return (Cop 0 (name_to_int k))
-- 
-- parse_cop_1 :: String -> ReadP Term
-- parse_cop_1 k = do
--   _ <- string "₁"
--   return (Cop 1 (name_to_int k))
-- 
-- parse_var :: String -> ReadP Term
-- parse_var k = do
--   return (Var (name_to_int k))
-- 
-- parse_ctr :: ReadP Term
-- parse_ctr = do
--   parse_lexeme (char '#')
--   k <- parse_name
--   parse_lexeme (char '{')
--   xs <- sepBy parse_term (parse_lexeme (char ','))
--   parse_lexeme (char '}')
--   return (Ctr (name_to_int k) xs)
-- 
-- parse_include :: ReadP FilePath
-- parse_include = parse_lexeme $ do
--   _ <- string "#include"
--   skip
--   _ <- char '"'
--   path <- munch (/= '"')
--   _ <- char '"'
--   return path
-- 
-- parse_function :: ReadP (Name, Term)
-- parse_function = do
--   parse_lexeme (char '@')
--   k <- parse_name
--   parse_lexeme (char '=')
--   f <- parse_term
--   return (name_to_int k, f)
-- 
-- parse_declaration :: ReadP Declaration
-- parse_declaration = choice
--   [ Include <$> parse_include
--   , uncurry Define <$> parse_function
--   ]
-- 
-- do_parse :: ReadP a -> String -> Maybe a
-- do_parse parser code =
--   case readP_to_S (parser <* skip <* eof) code of
--     (x, "") : _ -> Just x
--     _           -> Nothing
-- 
-- read_term :: String -> Term
-- read_term s = case do_parse parse_term s of
--   Just t  -> bruijn t
--   Nothing -> error "bad-parse"
-- 
-- load_book :: S.Set FilePath -> FilePath -> IO (M.Map Name Term)
-- load_book visited path = do
--   path <- canonicalizePath path
--   if S.member path visited then
--     return M.empty
--   else do
--     code <- readFile path
--     decs <- case do_parse (many parse_declaration) code of
--       Just x  -> return x
--       Nothing -> do
--         hPutStrLn stderr $ show_parse_error path code (many parse_declaration <* skip)
--         exitFailure
--     foldM go M.empty decs
--   where
--     go defs (Define k v) = do
--       return (M.insert k v defs)
--     go defs (Include p)  = do
--       incl <- load_book (S.insert path visited) (takeDirectory path </> p)
--       return (M.union incl defs)
-- 
-- show_parse_error :: FilePath -> String -> ReadP a -> String
-- show_parse_error path code parser =
--   let parses = readP_to_S parser code
--       best   = foldl' (\acc@(_,s) x@(_,s') -> if length s' < length s then x else acc) (error "no-parse", code) parses
--       rest   = snd best
--       idx    = length code - length rest
--       (line, col, text) = get_pos_info code idx
--       detected = case rest of
--         []    -> "end of file"
--         (c:_) -> show [c]
--   in "\ESC[1;31mPARSE_ERROR\ESC[0m (" ++ path ++ ":" ++ show line ++ ":" ++ show col ++ ")\n" ++
--      "- detected: " ++ detected ++ "\n" ++
--      show line ++ " | " ++ text ++ "\n" ++
--      replicate (length (show line) + 3 + col - 1) ' ' ++ "\ESC[1;31m^\ESC[0m"
-- 
-- get_pos_info :: String -> Int -> (Int, Int, String)
-- get_pos_info code idx = (line, col, text) where
--   pre  = take idx code
--   line = length (filter (== '\n') pre) + 1
--   col  = length (takeWhile (/= '\n') (reverse pre)) + 1
--   text = lines (code ++ "\n") !! (line - 1)
-- 
-- read_book :: FilePath -> IO Book
-- read_book path = do
--   defs <- load_book S.empty path
--   return $ Book (M.map bruijn defs)
-- 
-- -- Environment
-- -- ===========
-- 
-- new_env :: Book -> IO Env
-- new_env bk = do
--   itr <- newIORef 0
--   ids <- newIORef 1
--   sub <- newIORef IM.empty
--   dm  <- newIORef IM.empty
--   return $ Env bk itr ids sub dm
-- 
-- inc_itrs :: Env -> IO ()
-- inc_itrs e = do
--   !n <- readIORef (env_itrs e)
--   writeIORef (env_itrs e) (n + 1)
-- 
-- fresh :: Env -> IO Name
-- fresh e = do
--   !n <- readIORef (env_fresh e)
--   writeIORef (env_fresh e) (n + 1)
--   return ((n `shiftL` 6) + 63)
-- 
-- take_dup :: Env -> Name -> IO (Maybe (Lab, Term))
-- take_dup e k = atomicModifyIORef' (env_dups  e) $ \m -> (IM.delete k m, IM.lookup k m)
-- 
-- take_sub :: Env -> Name -> IO (Maybe Term)
-- take_sub e k = atomicModifyIORef' (env_subst e) $ \m -> (IM.delete k m, IM.lookup k m)
-- 
-- make_dup :: Env -> Name -> Lab -> Term -> IO ()
-- make_dup e k l v = modifyIORef' (env_dups  e) (IM.insert k (l, v))
-- 
-- subst :: Env -> Name -> Term -> IO ()
-- subst e k v = modifyIORef' (env_subst e) (IM.insert k v)
-- 
-- -- Quoting
-- -- =======
-- 
-- bruijn :: Term -> Term
-- bruijn t = go IM.empty 0 t where
--   go :: IM.IntMap Int -> Int -> Term -> Term
--   go env d t = case t of
--     Var k       -> Var   (d - 1 - env IM.! k)
--     Cop s k     -> Cop s (d - 1 - env IM.! k)
--     Ref k       -> Ref k
--     Nam k       -> Nam k
--     Dry f x     -> Dry (go env d f) (go env d x)
--     Era         -> Era
--     Sup l a b   -> Sup l (go env d a) (go env d b)
--     Dup k l v b -> Dup k l (go env d v) (go (IM.insert k d env) (d + 1) b)
--     Lam k f     -> Lam k (go (IM.insert k d env) (d + 1) f)
--     App f x     -> App (go env d f) (go env d x)
--     Ctr k xs    -> Ctr k (map (go env d) xs)
--     Mat k h m   -> Mat k (go env d h) (go env d m)
--     Alo s b     -> Alo s (go env d b)
-- 
-- -- Cloning
-- -- =======
-- 
-- clone :: Env -> Lab -> Term -> IO (Term, Term)
-- clone e l v = do
--   k <- fresh e
--   make_dup e k l v
--   return $ (Cop 0 k , Cop 1 k)
-- 
-- clone_list :: Env -> Lab -> [Term] -> IO ([Term], [Term])
-- clone_list e l []     = return ([], [])
-- clone_list e l (h:t) = do
--   (h0, h1) <- clone e l h
--   (t0, t1) <- clone_list e l t
--   return (h0:t0, h1:t1)
-- 
-- -- WNF: Weak Normal Form
-- -- =====================
-- 
-- type WnfApp = Env -> Term -> Term -> IO Term
-- type WnfDup = Int -> Env -> Name -> Lab -> Term -> IO Term
-- 
-- wnf :: Env -> Term -> IO Term
-- wnf e term = do
--   when debug $ putStrLn $ "wnf: " ++ show term
--   case term of
--     Var k -> do
--       var e k
--     Cop s k -> do
--       got <- take_dup e k
--       case got of
--         Just (l, v) -> do
--           v <- wnf e v
--           case v of
--             Era   -> dup_era s e k l v
--             Sup{} -> dup_sup s e k l v
--             Lam{} -> dup_lam s e k l v
--             Nam{} -> dup_nam s e k l v
--             Dry{} -> dup_dry s e k l v
--             Ctr{} -> dup_ctr s e k l v
--             Mat{} -> dup_mat s e k l v
--             _     -> return (Dup k l v (Cop s k))
--         Nothing -> do
--           cop s e k
--     App f x -> do
--       f <- wnf e f
--       case f of
--         Era   -> app_era e f x
--         Sup{} -> app_sup e f x
--         Lam{} -> app_lam e f x
--         Nam{} -> app_nam e f x
--         Dry{} -> app_dry e f x
--         Mat k h m -> do
--           x <- wnf e x
--           case x of
--             Era      -> app_mat_era e f x
--             Sup{}    -> app_mat_sup e f x
--             Ctr k' a -> app_mat_ctr e f k h m k' a
--             _        -> return (App f x)
--         _ -> return (App f x)
--     Dup k l v t -> do
--       make_dup e k l v
--       wnf e t
--     Ref k -> do
--       ref e k
--     Alo s t -> case t of
--       Var k       -> wnf e $ Var (s !! k)
--       Cop c k     -> wnf e $ Cop c (s !! k)
--       Ref k       -> wnf e $ Ref k
--       Nam k       -> wnf e $ Nam k
--       Dry f x     -> wnf e $ Dry (Alo s f) (Alo s x)
--       Era         -> wnf e $ Era
--       Sup l a b   -> wnf e $ Sup l (Alo s a) (Alo s b)
--       Dup k l v t -> do { x <- fresh e ; wnf e $ Dup x l (Alo s v) (Alo (x:s) t) }
--       Lam k f     -> do { x <- fresh e ; wnf e $ Lam x (Alo (x:s) f) }
--       App f x     -> wnf e $ App (Alo s f) (Alo s x)
--       Ctr k xs    -> wnf e $ Ctr k (map (Alo s) xs)
--       Mat k h m   -> wnf e $ Mat k (Alo s h) (Alo s m)
--       Alo s' t'   -> error "Nested Alo"
--     t -> do
--       return t
-- 
-- -- WNF: Interactions
-- -- =================
-- 
-- var :: Env -> Name -> IO Term
-- var e k = do
--   when debug $ putStrLn $ "var: " ++ show (Var k)
--   mt <- take_sub e k
--   case mt of
--     Just t  -> wnf e t
--     Nothing -> return $ Var k
-- 
-- cop :: Int -> Env -> Name -> IO Term
-- cop i e k = do
--   when debug $ putStrLn $ "cop: " ++ show (Cop i k)
--   mt <- take_sub e k
--   case mt of
--     Just t  -> wnf e t
--     Nothing -> return $ Cop i k
-- 
-- dup_era :: WnfDup
-- dup_era i e k _ Era = do
--   inc_itrs e
--   subst e k Era
--   wnf e Era
-- 
-- dup_sup :: WnfDup
-- dup_sup i e k l (Sup vl va vb)
--   | l == vl = do
--       inc_itrs e
--       if i == 0 then do
--         subst e k vb
--         wnf e va
--       else do
--         subst e k va
--         wnf e vb
--   | otherwise = do
--       inc_itrs e
--       (va0, va1) <- clone e l va
--       (vb0, vb1) <- clone e l vb
--       if i == 0 then do
--         subst e k (Sup vl va1 vb1)
--         wnf e (Sup vl va0 vb0)
--       else do
--         subst e k (Sup vl va0 vb0)
--         wnf e (Sup vl va1 vb1)
-- 
-- dup_lam :: WnfDup
-- dup_lam i e k l (Lam vk vf) = do
--   inc_itrs e
--   x0      <- fresh e
--   x1      <- fresh e
--   (g0,g1) <- clone e l vf
--   subst e vk (Sup l (Var x0) (Var x1))
--   if i == 0 then do
--     subst e k (Lam x1 g1)
--     wnf e (Lam x0 g0)
--   else do
--     subst e k (Lam x0 g0)
--     wnf e (Lam x1 g1)
-- 
-- dup_nam :: WnfDup
-- dup_nam i e k _ (Nam n) = do
--   inc_itrs e
--   subst e k (Nam n)
--   wnf e (Nam n)
-- 
-- dup_dry :: WnfDup
-- dup_dry i e k l (Dry vf vx) = do
--   inc_itrs e
--   (vf0, vf1) <- clone e l vf
--   (vx0, vx1) <- clone e l vx
--   if i == 0 then do
--     subst e k (Dry vf1 vx1)
--     wnf e (Dry vf0 vx0)
--   else do
--     subst e k (Dry vf0 vx0)
--     wnf e (Dry vf1 vx1)
-- 
-- dup_ctr :: WnfDup
-- dup_ctr i e k l (Ctr kn xs) = do
--   inc_itrs e
--   (xsA, xsB) <- clone_list e l xs
--   if i == 0 then do
--     subst e k (Ctr kn xsB)
--     wnf e (Ctr kn xsA)
--   else do
--     subst e k (Ctr kn xsA)
--     wnf e (Ctr kn xsB)
-- 
-- dup_mat :: WnfDup
-- dup_mat i e k l (Mat kn h m) = do
--   inc_itrs e
--   (hA, hB) <- clone e l h
--   (mA, mB) <- clone e l m
--   if i == 0 then do
--     subst e k (Mat kn hB mB)
--     wnf e (Mat kn hA mA)
--   else do
--     subst e k (Mat kn hA mA)
--     wnf e (Mat kn hB mB)
-- 
-- app_era :: WnfApp
-- app_era e Era v = do
--   inc_itrs e
--   wnf e Era
-- 
-- app_nam :: WnfApp
-- app_nam e (Nam fk) v = wnf e (Dry (Nam fk) v)
-- 
-- app_dry :: WnfApp
-- app_dry e (Dry ff fx) v = wnf e (Dry (Dry ff fx) v)
-- 
-- app_lam :: WnfApp
-- app_lam e (Lam fx ff) v = do
--   inc_itrs e
--   subst e fx v
--   wnf e ff
-- 
-- app_sup :: WnfApp
-- app_sup e (Sup fL fa fb) v = do
--   inc_itrs e
--   (x0,x1) <- clone e fL v
--   wnf e (Sup fL (App fa x0) (App fb x1))
-- 
-- app_mat_era :: WnfApp
-- app_mat_era e f x = do
--   inc_itrs e
--   wnf e Era
-- 
-- app_mat_sup :: WnfApp
-- app_mat_sup e (Mat k h m) (Sup l x y) = do
--   inc_itrs e
--   (h0, h1) <- clone e l h
--   (m0, m1) <- clone e l m
--   wnf e (Sup l (App (Mat k h0 m0) x) (App (Mat k h1 m1) y))
-- 
-- app_mat_ctr :: Env -> Term -> Int -> Term -> Term -> Int -> [Term] -> IO Term
-- app_mat_ctr e f k h m k' xs = do
--   inc_itrs e
--   if k == k' then do
--     wnf e (foldl' App h xs)
--   else do
--     wnf e (App m (Ctr k' xs))
-- 
-- ref :: Env -> Name -> IO Term
-- ref e k = do
--   let (Book m) = env_book e
--   case M.lookup k m of
--     Just f  -> do
--       inc_itrs e
--       wnf e (Alo [] f)
--     Nothing -> error $ "UndefinedReference: " ++ int_to_name k
-- 
-- -- Normalization
-- -- =============
-- 
-- snf :: Env -> Int -> Term -> IO Term
-- snf e d x = do
--   !x' <- wnf e x
--   case x' of
-- 
--     Var k -> do
--       return $ Var k
-- 
--     Cop s k -> do
--       return $ Cop s k
-- 
--     Era -> do
--       return $ Era
-- 
--     Sup l a b -> do
--       a' <- snf e d a
--       b' <- snf e d b
--       return $ Sup l a' b'
-- 
--     Dup k l v t -> do
--       error "TODO"
-- 
--     Lam k f -> do
--       subst e k (Nam (int_to_name d))
--       f' <- snf e (d + 1) f
--       return $ Lam d f'
-- 
--     App f x -> do
--       f' <- snf e d f
--       x' <- snf e d x
--       return $ App f' x'
-- 
--     Ref k -> do
--       return $ Ref k
-- 
--     Nam k -> do
--       return $ Nam k
-- 
--     Dry f x -> do
--       f' <- snf e d f
--       x' <- snf e d x
--       return $ Dry f' x'
-- 
--     Ctr k xs -> do
--       xs' <- mapM (snf e d) xs
--       return $ Ctr k xs'
-- 
--     Mat k h m -> do
--       h' <- snf e d h
--       m' <- snf e d m
--       return $ Mat k h' m'
-- 
--     Alo s t -> do
--       error "Should be gone"
-- 
-- -- Collapsing
-- -- ==========
-- 
-- collapse :: Env -> Term -> IO Term
-- collapse e x = do
--   !x <- wnf e x
--   case x of
-- 
--     Era -> do
--       return Era
-- 
--     (Sup l a b) -> do
--       a' <- collapse e a
--       b' <- collapse e b
--       return $ Sup l a' b'
-- 
--     (Lam k f) -> do
--       fV <- fresh e
--       f' <- collapse e f
--       inject e (Lam fV (Lam k (Var fV))) [f']
-- 
--     (App f x) -> do
--       fV <- fresh e
--       xV <- fresh e
--       f' <- collapse e f
--       x' <- collapse e x
--       inject e (Lam fV (Lam xV (App (Var fV) (Var xV)))) [f', x']
-- 
--     Nam n -> do
--       return $ Nam n
-- 
--     Dry f x -> do
--       fV <- fresh e
--       xV <- fresh e
--       f' <- collapse e f
--       x' <- collapse e x
--       inject e (Lam fV (Lam xV (Dry (Var fV) (Var xV)))) [f', x']
-- 
--     Ctr k xs -> do
--       vs <- mapM (\_ -> fresh e) xs
--       as <- mapM (collapse e) xs
--       inject e (foldr Lam (Ctr k (map Var vs)) vs) as
-- 
--     Mat k h m -> do
--       hV <- fresh e
--       mV <- fresh e
--       h' <- collapse e h
--       m' <- collapse e m
--       inject e (Lam hV (Lam mV (Mat k (Var hV) (Var mV)))) [h', m']
-- 
--     x -> do
--       return $ x
-- 
-- inject :: Env -> Term -> [Term] -> IO Term
-- inject e f [] = return f
-- inject e f (h:t) = do
--   !h <- wnf e h
--   case h of
--     Sup l a b -> do
--       (f0,f1) <- clone e l f
--       (t0,t1) <- clone_list e l t
--       a' <- inject e f0 (a:t0)
--       b' <- inject e f1 (b:t1)
--       return $ Sup l a' b'
--     _ -> do
--       inject e (App f h) t
-- 
-- flatten :: Term -> [Term]
-- flatten term = bfs [term] [] where
--   bfs []     acc = reverse acc
--   bfs (t:ts) acc = case t of
--     Sup _ a b -> bfs (ts ++ [a, b]) acc
--     _         -> bfs ts (t : acc)
-- 

-- @snf = λx. x

-- @snf_go = λ{
--   #A: #A
-- }

-- PROBLEM: the new error reporting feature is working, but it is not showing a very localized error. for example, in the file below:
-- 
-- @snf = λx. x
-- 
-- @snf_go = λ{
--   #A: #A
-- }
-- 
-- the error reported is:
-- 
-- PARSE_ERROR (/Users/v/vic/dev/hvm4-spec/bad.hvm4:3:1)
-- - detected: "@"
-- 3 | @snf_go = λ{
--     ^
-- 
-- but the actual issue is a missing {} after the #A constructor (should be
-- #A{}). in general, this sort of bad error reporting seems structural about
-- this approach. can we improve the parser / error reporting so that we always
-- get an error pointing to where the faulty character is? note that our parser
-- is non-backtracking, so, that makes things easier, but I'm not sure how we
-- can support for good errors on ReadP. what would you do? answer below:
-- answer below in ENGLISH

-- To fix the error reporting, I would introduce "Error Productions" or "Commit
-- Points" to the parser. Since `ReadP` is a backtracking parser, when it encounters
-- a syntax error deep inside a definition (like the missing `{}`), it simply fails
-- that branch and backtracks to the beginning, losing the error location.
--
-- By adding a catch-all "Error" case to the grammar and parser, we can force the
-- parser to accept the malformed input as a valid "ErrorTerm" instead of failing.
-- For example, if `parse_declaration` encounters an `@` but fails to parse the
-- rest, it should fall back to a parser that consumes the line and returns an
-- error node. This preserves the location of the failure in the AST, which can
-- then be reported to the user.

-- that's overly complex, surely. note that we do not need a backtracking
-- parser, since our grammar isn't backtracking, right? so, is there a simpler
-- way to achieve what we want? the goal is to keep this file size as small as
-- possible.

-- continue the conversation below, in English:

-- Yes, precisely. Since the grammar is non-backtracking (LL(1)), we can
-- replace the parser combinators with a simple recursive descent parser
-- (a.k.a. "string munching"). By inspecting the next character, we can decide
-- exactly which construct to parse. Once we commit to a path (e.g., seeing
-- '#'), any subsequent mismatch becomes a hard error at that specific
-- location, rather than triggering a backtrack. This approach provides
-- precise error reporting and is often more concise for simple grammars.
-- 
-- got it. that sounds great to me. can you rewrite this file from scratch
-- to include such string munching parser? keep it as simple as possible,
-- while still maintaining the elegance of the current approach.
-- 
-- I suggest the following type:
-- - the parser state holds the remaining string, and location (line, col?)
-- - a parser receives a state and returns a reply
-- - a reply is a value A and the new state, or an error
-- even better: we can make parsing operate in IO, so that the recursive
-- #include is seamless. perhaps we could also include the already seen
-- file map in the parser state itself? that sounds great to me. again,
-- the goal is to simplify the code as much as we can.
--
-- then, whenever there is an unexpected char, we just print a nice error
-- message with the current location (line / col), and exit.
-- 
-- does that make sense?
-- 
-- if so, please implement the full file now:


{-# LANGUAGE BangPatterns, CPP #-}

import Control.Monad (foldM, forM_, when)
import Data.Bits (shiftL)
import Data.Char (isSpace, isDigit)
import Data.IORef
import Data.List (foldl', elemIndex, intercalate)
import System.CPUTime
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import Text.Printf
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Config
-- ======

debug :: Bool
debug = False

-- Types
-- =====

type Lab  = Int
type Name = Int

data Term
  = Var !Name
  | Cop !Int !Name
  | Ref !Name
  | Nam !String
  | Dry !Term !Term
  | Era
  | Sup !Lab !Term !Term
  | Dup !Name !Lab !Term !Term
  | Lam !Name !Term
  | App !Term !Term
  | Ctr !Name ![Term]
  | Mat !Name !Term !Term
  | Alo ![Name] !Term
  deriving (Eq)

data Book = Book (M.Map Name Term)

data Declaration
  = Include FilePath
  | Define Name Term

data Env = Env
  { env_book  :: !Book
  , env_itrs  :: !(IORef Int)
  , env_fresh :: !(IORef Int)
  , env_subst :: !(IORef (IM.IntMap Term))
  , env_dups  :: !(IORef (IM.IntMap (Lab, Term)))
  }

-- Showing
-- =======

instance Show Term where
  show (Var k)       = int_to_name k
  show (Cop s k)     = int_to_name k ++ (if s == 0 then "₀" else "₁")
  show (Ref k)       = "@" ++ int_to_name k
  show (Nam k)       = k
  show (Dry f x)     = show_app f x
  show Era           = "&{}"
  show (Sup l a b)   = "&" ++ int_to_name l ++ "{" ++ show a ++ "," ++ show b ++ "}"
  show (Dup k l v t) = "!" ++ int_to_name k ++ "&" ++ int_to_name l ++ "=" ++ show v ++ ";" ++ show t
  show (Lam k f)     = "λ" ++ int_to_name k ++ "." ++ show f
  show (App f x)     = show_app f x
  show (Ctr k xs)    = "#" ++ int_to_name k ++ "{" ++ intercalate "," (map show xs) ++ "}"
  show (Mat k h m)   = "λ{#" ++ int_to_name k ++ ":" ++ show h ++ ";" ++ show m ++ "}"
  show (Alo s t)     = "@{" ++ intercalate "," (map int_to_name s) ++ "}" ++ show t

show_app :: Term -> Term -> String
show_app f x = case f of
  App _ _ -> init (show f) ++ "," ++ show x ++ ")"
  Dry _ _ -> init (show f) ++ "," ++ show x ++ ")"
  Lam _ _ -> "(" ++ show f ++ ")(" ++ show x ++ ")"
  _       -> show f ++ "(" ++ show x ++ ")"

instance Show Book where
  show (Book m) = unlines [ "@" ++ int_to_name k ++ " = " ++ show ct | (k, ct) <- M.toList m ]

show_dups  :: IM.IntMap (Lab, Term) -> String
show_dups  m = unlines [ "! " ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ show v | (k, (l, v)) <- IM.toList m ]

show_subst :: IM.IntMap Term -> String
show_subst m = unlines [ int_to_name (k `div` 4) ++ suffix (k `mod` 4) ++ " ← " ++ show v | (k, v) <- IM.toList m ]
  where suffix x = case x of { 0 -> "" ; 1 -> "₀" ; 2 -> "₁" ; _ -> "?" }

-- Name Encoding/Decoding
-- ======================

alphabet :: String
alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"

alphabet_first :: String
alphabet_first = filter (`notElem` "_0123456789") alphabet

name_to_int :: String -> Int
name_to_int = foldl' (\acc c -> (acc `shiftL` 6) + idx c) 0
  where idx c = maybe (error "bad name char") id (elemIndex c alphabet)

int_to_name :: Int -> String
int_to_name n | n <  64 = [alphabet !! n]
              | n >= 64 = int_to_name (n `div` 64) ++ [alphabet !! (n `mod` 64)]
-- Parsing
-- =======

data Context = Context
  { ctx_file :: FilePath
  , ctx_src  :: String
  , ctx_code :: String
  , ctx_line :: Int
  , ctx_col  :: Int
  , ctx_defs :: M.Map Name Term
  , ctx_seen :: S.Set FilePath
  }

type Parser a = IORef Context -> IO a

error_parse :: String -> Parser a
error_parse expected ref = do
  ctx <- readIORef ref
  let (line, col) = (ctx_line ctx, ctx_col ctx)
  let file = ctx_file ctx
  let src  = ctx_src  ctx
  let curr = case ctx_code ctx of { [] -> "EOF"; (c:_) -> show [c] }
  let text = case lines (src ++ "\n") of
               ls | line <= length ls -> ls !! (line - 1)
               _                      -> ""
  hPutStrLn stderr $ "\ESC[1;31mPARSE_ERROR\ESC[0m (" ++ file ++ ":" ++ show line ++ ":" ++ show col ++ ")"
  hPutStrLn stderr $ "- detected: " ++ curr
  hPutStrLn stderr $ "- expected: " ++ expected
  hPutStrLn stderr $ show line ++ " | " ++ text
  hPutStrLn stderr $ replicate (length (show line) + 3 + col - 1) ' ' ++ "\ESC[1;31m^\ESC[0m"
  exitFailure

peek :: Parser Char
peek ref = do
  ctx <- readIORef ref
  case ctx_code ctx of
    (c:_) -> return c
    []    -> return '\0'

advance :: Parser ()
advance ref = do
  ctx <- readIORef ref
  case ctx_code ctx of
    (c:cs) -> do
      let (l, c') = if c == '\n' then (ctx_line ctx + 1, 1) else (ctx_line ctx, ctx_col ctx + 1)
      writeIORef ref $! ctx { ctx_code = cs, ctx_line = l, ctx_col = c' }
    [] -> return ()

consume :: String -> Parser ()
consume str ref = mapM_ (\c -> do
  c' <- peek ref
  if c == c' then advance ref else error_parse ("'" ++ [c] ++ "'") ref) str

skip :: Parser ()
skip ref = do
  c <- peek ref
  case c of
    ' '  -> advance ref >> skip ref
    '\t' -> advance ref >> skip ref
    '\n' -> advance ref >> skip ref
    '\r' -> advance ref >> skip ref
    '/'  -> do
      advance ref
      c2 <- peek ref
      if c2 == '/' then do
        advance ref
        skip_line ref
      else error_parse "comment" ref
    _    -> return ()

skip_line :: Parser ()
skip_line ref = do
  c <- peek ref
  if c == '\0' || c == '\n' then skip ref else advance ref >> skip_line ref

parse_name_str :: Parser String
parse_name_str ref = do
  skip ref
  c <- peek ref
  if c `elem` alphabet_first then do
    advance ref
    cs <- parse_name_rest ref
    return (c:cs)
  else error_parse "name" ref
  where
    parse_name_rest ref = do
      c <- peek ref
      if c `elem` alphabet then do
        advance ref
        cs <- parse_name_rest ref
        return (c:cs)
      else return []

parse_name :: Parser Int
parse_name ref = name_to_int <$> parse_name_str ref

parse_term :: Parser Term
parse_term ref = do
  t <- parse_term_base ref
  parse_term_suff t ref

parse_term_base :: Parser Term
parse_term_base ref = do
  skip ref
  c <- peek ref
  case c of
    'λ' -> do
      advance ref
      skip ref
      c2 <- peek ref
      if c2 == '{' then parse_mat ref else parse_lam ref
    '!' -> parse_dup ref
    '&' -> parse_sup_era ref
    '#' -> parse_ctr ref
    '@' -> parse_ref_alo ref
    '(' -> parse_par ref
    _   -> parse_var_cop ref

parse_term_suff :: Term -> Parser Term
parse_term_suff t ref = do
  skip ref
  c <- peek ref
  if c == '(' then do
    advance ref
    args <- parse_args ref
    consume ")" ref
    parse_term_suff (foldl' App t args) ref
  else return t

parse_args :: Parser [Term]
parse_args ref = do
  skip ref
  c <- peek ref
  if c == ')' then return [] else do
    t <- parse_term ref
    skip ref
    c2 <- peek ref
    if c2 == ',' then do
      advance ref
      ts <- parse_args ref
      return (t:ts)
    else return [t]

parse_par :: Parser Term
parse_par ref = do
  consume "(" ref
  t <- parse_term ref
  consume ")" ref
  return t

parse_lam :: Parser Term
parse_lam ref = do
  name <- parse_name ref
  consume "." ref
  body <- parse_term ref
  return $ Lam name body

parse_mat :: Parser Term
parse_mat ref = do
  consume "{" ref
  parse_mat_body ref

parse_mat_body :: Parser Term
parse_mat_body ref = do
  skip ref
  ctx <- readIORef ref
  c <- peek ref
  is_case <- if c == '#' then do
      advance ref
      _ <- parse_name_str ref
      skip ref
      c2 <- peek ref
      return (c2 == ':')
    else return False
  writeIORef ref ctx
  if is_case then do
    consume "#" ref
    name <- parse_name ref
    consume ":" ref
    val <- parse_term ref
    skip ref
    c2 <- peek ref
    when (c2 == ';') $ advance ref
    rest <- parse_mat_body ref
    return $ Mat name val rest
  else if c == '}' then do
    advance ref
    return Era
  else do
    t <- parse_term ref
    skip ref
    consume "}" ref
    return t

parse_dup :: Parser Term
parse_dup ref = do
  advance ref
  nam <- parse_name ref
  consume "&" ref
  lab <- parse_name ref
  consume "=" ref
  val <- parse_term ref
  consume ";" ref
  bod <- parse_term ref
  return $ Dup nam lab val bod

parse_sup_era :: Parser Term
parse_sup_era ref = do
  advance ref
  c <- peek ref
  if c == '{' then do
    consume "{}" ref
    return Era
  else do
    lab <- parse_name ref
    consume "{" ref
    tm1 <- parse_term ref
    consume "," ref
    tm2 <- parse_term ref
    consume "}" ref
    return $ Sup lab tm1 tm2

parse_ctr :: Parser Term
parse_ctr ref = do
  advance ref
  nam <- parse_name ref
  consume "{" ref
  args <- parse_list ref
  consume "}" ref
  return $ Ctr nam args

parse_list :: Parser [Term]
parse_list ref = do
  skip ref
  c <- peek ref
  if c == '}' then return [] else do
    t <- parse_term ref
    skip ref
    c2 <- peek ref
    if c2 == ',' then do
      advance ref
      ts <- parse_list ref
      return (t:ts)
    else return [t]

parse_ref_alo :: Parser Term
parse_ref_alo ref = do
  advance ref
  c <- peek ref
  if c == '{' then do
    consume "{" ref
    names <- parse_names ref
    consume "}" ref
    term <- parse_term ref
    return $ Alo names term
  else do
    name <- parse_name ref
    return $ Ref name

parse_names :: Parser [Name]
parse_names ref = do
  skip ref
  c <- peek ref
  if c == '}' then return [] else do
    n <- parse_name ref
    skip ref
    c2 <- peek ref
    if c2 == ',' then do
      advance ref
      ns <- parse_names ref
      return (n:ns)
    else return [n]

parse_var_cop :: Parser Term
parse_var_cop ref = do
  name <- parse_name ref
  c <- peek ref
  case c of
    '₀' -> advance ref >> return (Cop 0 name)
    '₁' -> advance ref >> return (Cop 1 name)
    _   -> return (Var name)

parse_str_until :: Char -> Parser String
parse_str_until end ref = do
  c <- peek ref
  if c == end then return [] else do
    advance ref
    cs <- parse_str_until end ref
    return (c:cs)

read_book :: FilePath -> IO Book
read_book path = do
  path <- canonicalizePath path
  ref  <- newIORef $ Context path "" "" 1 1 M.empty S.empty
  do_include ref path
  ctx  <- readIORef ref
  return $ Book (M.map bruijn (ctx_defs ctx))

do_include :: IORef Context -> FilePath -> IO ()
do_include ref path = do
  ctx <- readIORef ref
  if S.member path (ctx_seen ctx) then return () else do
    code <- readFile path
    let old_file = ctx_file ctx
    let old_src  = ctx_src  ctx
    let old_code = ctx_code ctx
    let old_line = ctx_line ctx
    let old_col  = ctx_col  ctx
    writeIORef ref $ ctx { ctx_file = path, ctx_src = code, ctx_code = code, ctx_line = 1, ctx_col = 1, ctx_seen = S.insert path (ctx_seen ctx) }
    parse_defs ref
    ctx' <- readIORef ref
    writeIORef ref $ ctx' { ctx_file = old_file, ctx_src = old_src, ctx_code = old_code, ctx_line = old_line, ctx_col = old_col }

parse_defs :: Parser ()
parse_defs ref = do
  skip ref
  c <- peek ref
  case c of
    '\0' -> return ()
    '#'  -> do
      consume "#include" ref
      skip ref
      consume "\"" ref
      path <- parse_str_until '"' ref
      consume "\"" ref
      ctx <- readIORef ref
      let dir = takeDirectory (ctx_file ctx)
      inc <- canonicalizePath (dir </> path)
      do_include ref inc
      parse_defs ref
    '@'  -> do
      advance ref
      name <- parse_name ref
      skip ref
      consume "=" ref
      term <- parse_term ref
      ctx <- readIORef ref
      writeIORef ref $ ctx { ctx_defs = M.insert name term (ctx_defs ctx) }
      parse_defs ref
    _    -> error_parse "definition or #include" ref

read_term :: String -> Term
read_term _ = error "read_term not supported in IO parser without IO" 

-- Environment
-- ===========

new_env :: Book -> IO Env
new_env bk = do
  itr <- newIORef 0
  ids <- newIORef 1
  sub <- newIORef IM.empty
  dm  <- newIORef IM.empty
  return $ Env bk itr ids sub dm

inc_itrs :: Env -> IO ()
inc_itrs e = do
  !n <- readIORef (env_itrs e)
  writeIORef (env_itrs e) (n + 1)

fresh :: Env -> IO Name
fresh e = do
  !n <- readIORef (env_fresh e)
  writeIORef (env_fresh e) (n + 1)
  return ((n `shiftL` 6) + 63)

take_dup :: Env -> Name -> IO (Maybe (Lab, Term))
take_dup e k = atomicModifyIORef' (env_dups  e) $ \m -> (IM.delete k m, IM.lookup k m)

take_sub :: Env -> Name -> IO (Maybe Term)
take_sub e k = atomicModifyIORef' (env_subst e) $ \m -> (IM.delete k m, IM.lookup k m)

make_dup :: Env -> Name -> Lab -> Term -> IO ()
make_dup e k l v = modifyIORef' (env_dups  e) (IM.insert k (l, v))

subst :: Env -> Name -> Term -> IO ()
subst e k v = modifyIORef' (env_subst e) (IM.insert k v)

-- Quoting
-- =======

bruijn :: Term -> Term
bruijn t = go IM.empty 0 t where
  go :: IM.IntMap Int -> Int -> Term -> Term
  go env d t = case t of
    Var k       -> Var   (d - 1 - env IM.! k)
    Cop s k     -> Cop s (d - 1 - env IM.! k)
    Ref k       -> Ref k
    Nam k       -> Nam k
    Dry f x     -> Dry (go env d f) (go env d x)
    Era         -> Era
    Sup l a b   -> Sup l (go env d a) (go env d b)
    Dup k l v b -> Dup k l (go env d v) (go (IM.insert k d env) (d + 1) b)
    Lam k f     -> Lam k (go (IM.insert k d env) (d + 1) f)
    App f x     -> App (go env d f) (go env d x)
    Ctr k xs    -> Ctr k (map (go env d) xs)
    Mat k h m   -> Mat k (go env d h) (go env d m)
    Alo s b     -> Alo s (go env d b)

-- Cloning
-- =======

clone :: Env -> Lab -> Term -> IO (Term, Term)
clone e l v = do
  k <- fresh e
  make_dup e k l v
  return $ (Cop 0 k , Cop 1 k)

clone_list :: Env -> Lab -> [Term] -> IO ([Term], [Term])
clone_list e l []     = return ([], [])
clone_list e l (h:t) = do
  (h0, h1) <- clone e l h
  (t0, t1) <- clone_list e l t
  return (h0:t0, h1:t1)

-- WNF: Weak Normal Form
-- =====================

type WnfApp = Env -> Term -> Term -> IO Term
type WnfDup = Int -> Env -> Name -> Lab -> Term -> IO Term

wnf :: Env -> Term -> IO Term
wnf e term = do
  when debug $ putStrLn $ "wnf: " ++ show term
  case term of
    Var k -> do
      var e k
    Cop s k -> do
      got <- take_dup e k
      case got of
        Just (l, v) -> do
          v <- wnf e v
          case v of
            Era   -> dup_era s e k l v
            Sup{} -> dup_sup s e k l v
            Lam{} -> dup_lam s e k l v
            Nam{} -> dup_nam s e k l v
            Dry{} -> dup_dry s e k l v
            Ctr{} -> dup_ctr s e k l v
            Mat{} -> dup_mat s e k l v
            _     -> return (Dup k l v (Cop s k))
        Nothing -> do
          cop s e k
    App f x -> do
      f <- wnf e f
      case f of
        Era   -> app_era e f x
        Sup{} -> app_sup e f x
        Lam{} -> app_lam e f x
        Nam{} -> app_nam e f x
        Dry{} -> app_dry e f x
        Mat k h m -> do
          x <- wnf e x
          case x of
            Era      -> app_mat_era e f x
            Sup{}    -> app_mat_sup e f x
            Ctr k' a -> app_mat_ctr e f k h m k' a
            _        -> return (App f x)
        _ -> return (App f x)
    Dup k l v t -> do
      make_dup e k l v
      wnf e t
    Ref k -> do
      ref e k
    Alo s t -> case t of
      Var k       -> wnf e $ Var (s !! k)
      Cop c k     -> wnf e $ Cop c (s !! k)
      Ref k       -> wnf e $ Ref k
      Nam k       -> wnf e $ Nam k
      Dry f x     -> wnf e $ Dry (Alo s f) (Alo s x)
      Era         -> wnf e $ Era
      Sup l a b   -> wnf e $ Sup l (Alo s a) (Alo s b)
      Dup k l v t -> do { x <- fresh e ; wnf e $ Dup x l (Alo s v) (Alo (x:s) t) }
      Lam k f     -> do { x <- fresh e ; wnf e $ Lam x (Alo (x:s) f) }
      App f x     -> wnf e $ App (Alo s f) (Alo s x)
      Ctr k xs    -> wnf e $ Ctr k (map (Alo s) xs)
      Mat k h m   -> wnf e $ Mat k (Alo s h) (Alo s m)
      Alo s' t'   -> error "Nested Alo"
    t -> do
      return t

-- WNF: Interactions
-- =================

var :: Env -> Name -> IO Term
var e k = do
  when debug $ putStrLn $ "var: " ++ show (Var k)
  mt <- take_sub e k
  case mt of
    Just t  -> wnf e t
    Nothing -> return $ Var k

cop :: Int -> Env -> Name -> IO Term
cop i e k = do
  when debug $ putStrLn $ "cop: " ++ show (Cop i k)
  mt <- take_sub e k
  case mt of
    Just t  -> wnf e t
    Nothing -> return $ Cop i k

dup_era :: WnfDup
dup_era i e k _ Era = do
  inc_itrs e
  subst e k Era
  wnf e Era

dup_sup :: WnfDup
dup_sup i e k l (Sup vl va vb)
  | l == vl = do
      inc_itrs e
      if i == 0 then do
        subst e k vb
        wnf e va
      else do
        subst e k va
        wnf e vb
  | otherwise = do
      inc_itrs e
      (va0, va1) <- clone e l va
      (vb0, vb1) <- clone e l vb
      if i == 0 then do
        subst e k (Sup vl va1 vb1)
        wnf e (Sup vl va0 vb0)
      else do
        subst e k (Sup vl va0 vb0)
        wnf e (Sup vl va1 vb1)

dup_lam :: WnfDup
dup_lam i e k l (Lam vk vf) = do
  inc_itrs e
  x0      <- fresh e
  x1      <- fresh e
  (g0,g1) <- clone e l vf
  subst e vk (Sup l (Var x0) (Var x1))
  if i == 0 then do
    subst e k (Lam x1 g1)
    wnf e (Lam x0 g0)
  else do
    subst e k (Lam x0 g0)
    wnf e (Lam x1 g1)

dup_nam :: WnfDup
dup_nam i e k _ (Nam n) = do
  inc_itrs e
  subst e k (Nam n)
  wnf e (Nam n)

dup_dry :: WnfDup
dup_dry i e k l (Dry vf vx) = do
  inc_itrs e
  (vf0, vf1) <- clone e l vf
  (vx0, vx1) <- clone e l vx
  if i == 0 then do
    subst e k (Dry vf1 vx1)
    wnf e (Dry vf0 vx0)
  else do
    subst e k (Dry vf0 vx0)
    wnf e (Dry vf1 vx1)

dup_ctr :: WnfDup
dup_ctr i e k l (Ctr kn xs) = do
  inc_itrs e
  (xsA, xsB) <- clone_list e l xs
  if i == 0 then do
    subst e k (Ctr kn xsB)
    wnf e (Ctr kn xsA)
  else do
    subst e k (Ctr kn xsA)
    wnf e (Ctr kn xsB)

dup_mat :: WnfDup
dup_mat i e k l (Mat kn h m) = do
  inc_itrs e
  (hA, hB) <- clone e l h
  (mA, mB) <- clone e l m
  if i == 0 then do
    subst e k (Mat kn hB mB)
    wnf e (Mat kn hA mA)
  else do
    subst e k (Mat kn hA mA)
    wnf e (Mat kn hB mB)

app_era :: WnfApp
app_era e Era v = do
  inc_itrs e
  wnf e Era

app_nam :: WnfApp
app_nam e (Nam fk) v = wnf e (Dry (Nam fk) v)

app_dry :: WnfApp
app_dry e (Dry ff fx) v = wnf e (Dry (Dry ff fx) v)

app_lam :: WnfApp
app_lam e (Lam fx ff) v = do
  inc_itrs e
  subst e fx v
  wnf e ff

app_sup :: WnfApp
app_sup e (Sup fL fa fb) v = do
  inc_itrs e
  (x0,x1) <- clone e fL v
  wnf e (Sup fL (App fa x0) (App fb x1))

app_mat_era :: WnfApp
app_mat_era e f x = do
  inc_itrs e
  wnf e Era

app_mat_sup :: WnfApp
app_mat_sup e (Mat k h m) (Sup l x y) = do
  inc_itrs e
  (h0, h1) <- clone e l h
  (m0, m1) <- clone e l m
  wnf e (Sup l (App (Mat k h0 m0) x) (App (Mat k h1 m1) y))

app_mat_ctr :: Env -> Term -> Int -> Term -> Term -> Int -> [Term] -> IO Term
app_mat_ctr e f k h m k' xs = do
  inc_itrs e
  if k == k' then do
    wnf e (foldl' App h xs)
  else do
    wnf e (App m (Ctr k' xs))

ref :: Env -> Name -> IO Term
ref e k = do
  let (Book m) = env_book e
  case M.lookup k m of
    Just f  -> do
      inc_itrs e
      wnf e (Alo [] f)
    Nothing -> error $ "UndefinedReference: " ++ int_to_name k

-- Normalization
-- =============

snf :: Env -> Int -> Term -> IO Term
snf e d x = do
  !x' <- wnf e x
  case x' of

    Var k -> do
      return $ Var k

    Cop s k -> do
      return $ Cop s k

    Era -> do
      return $ Era

    Sup l a b -> do
      a' <- snf e d a
      b' <- snf e d b
      return $ Sup l a' b'

    Dup k l v t -> do
      error "TODO"

    Lam k f -> do
      subst e k (Nam (int_to_name d))
      f' <- snf e (d + 1) f
      return $ Lam d f'

    App f x -> do
      f' <- snf e d f
      x' <- snf e d x
      return $ App f' x'

    Ref k -> do
      return $ Ref k

    Nam k -> do
      return $ Nam k

    Dry f x -> do
      f' <- snf e d f
      x' <- snf e d x
      return $ Dry f' x'

    Ctr k xs -> do
      xs' <- mapM (snf e d) xs
      return $ Ctr k xs'

    Mat k h m -> do
      h' <- snf e d h
      m' <- snf e d m
      return $ Mat k h' m'

    Alo s t -> do
      error "Should be gone"

-- Collapsing
-- ==========

collapse :: Env -> Term -> IO Term
collapse e x = do
  !x <- wnf e x
  case x of

    Era -> do
      return Era

    (Sup l a b) -> do
      a' <- collapse e a
      b' <- collapse e b
      return $ Sup l a' b'

    (Lam k f) -> do
      fV <- fresh e
      f' <- collapse e f
      inject e (Lam fV (Lam k (Var fV))) [f']

    (App f x) -> do
      fV <- fresh e
      xV <- fresh e
      f' <- collapse e f
      x' <- collapse e x
      inject e (Lam fV (Lam xV (App (Var fV) (Var xV)))) [f', x']

    Nam n -> do
      return $ Nam n

    Dry f x -> do
      fV <- fresh e
      xV <- fresh e
      f' <- collapse e f
      x' <- collapse e x
      inject e (Lam fV (Lam xV (Dry (Var fV) (Var xV)))) [f', x']

    Ctr k xs -> do
      vs <- mapM (\_ -> fresh e) xs
      as <- mapM (collapse e) xs
      inject e (foldr Lam (Ctr k (map Var vs)) vs) as

    Mat k h m -> do
      hV <- fresh e
      mV <- fresh e
      h' <- collapse e h
      m' <- collapse e m
      inject e (Lam hV (Lam mV (Mat k (Var hV) (Var mV)))) [h', m']

    x -> do
      return $ x

inject :: Env -> Term -> [Term] -> IO Term
inject e f [] = return f
inject e f (h:t) = do
  !h <- wnf e h
  case h of
    Sup l a b -> do
      (f0,f1) <- clone e l f
      (t0,t1) <- clone_list e l t
      a' <- inject e f0 (a:t0)
      b' <- inject e f1 (b:t1)
      return $ Sup l a' b'
    _ -> do
      inject e (App f h) t

flatten :: Term -> [Term]
flatten term = bfs [term] [] where
  bfs []     acc = reverse acc
  bfs (t:ts) acc = case t of
    Sup _ a b -> bfs (ts ++ [a, b]) acc
    _         -> bfs ts (t : acc)
