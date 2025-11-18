wnf_dup :: Env -> Stack -> Term -> Name -> Lab -> Term -> IO Term
wnf_dup e s v k l t = do
  when debug $ putStrLn $ "wnf_dup: " ++ show (Dup k l v t)
  case v of
    Era    -> wnf_dup_era e s v k l t
    Sup {} -> wnf_dup_sup e s v k l t
    Set    -> wnf_dup_set e s v k l t
    All {} -> wnf_dup_all e s v k l t
    Lam {} -> wnf_dup_lam e s v k l t
    Nat    -> wnf_dup_nat e s v k l t
    Zer    -> wnf_dup_zer e s v k l t
    Suc {} -> wnf_dup_suc e s v k l t
    Swi {} -> wnf_dup_swi e s v k l t
    Nam {} -> wnf_dup_nam e s v k l t
    Dry {} -> wnf_dup_dry e s v k l t
    Gua {} -> wnf_dup_gua e s v k l t
    Sig {} -> wnf_dup_sig e s v k l t
    Tup {} -> wnf_dup_tup e s v k l t
    Get {} -> wnf_dup_get e s v k l t
    Emp    -> wnf_dup_emp e s v k l t
    Efq    -> wnf_dup_efq e s v k l t
    Uni    -> wnf_dup_uni e s v k l t
    One    -> wnf_dup_one e s v k l t
    Use {} -> wnf_dup_use e s v k l t
    Bol    -> wnf_dup_bol e s v k l t
    Fal    -> wnf_dup_fal e s v k l t
    Tru    -> wnf_dup_tru e s v k l t
    If {}  -> wnf_dup_if  e s v k l t
    Lst {} -> wnf_dup_lst e s v k l t
    Nil    -> wnf_dup_nil e s v k l t
    Con {} -> wnf_dup_con e s v k l t
    Mat {} -> wnf_dup_mat e s v k l t
    _      -> wnf_unwind e s (Dup k l v t)

