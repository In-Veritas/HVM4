wnf_dup_if :: WnfDup
wnf_dup_if e s (If f tr) k l t = wnf_dup_2 e s k l t f tr If

