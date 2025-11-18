wnf_dup_tup :: WnfDup
wnf_dup_tup e s (Tup a b) k l t = wnf_dup_2 e s k l t a b Tup

