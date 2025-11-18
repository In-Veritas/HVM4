wnf_dup_con :: WnfDup
wnf_dup_con e s (Con h tr) k l t = wnf_dup_2 e s k l t h tr Con

