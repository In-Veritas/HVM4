wnf_dup_sig :: WnfDup
wnf_dup_sig e s (Sig a b) k l t = wnf_dup_2 e s k l t a b Sig

