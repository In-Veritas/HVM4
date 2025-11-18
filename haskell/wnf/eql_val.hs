wnf_eql_val :: WnfEql
wnf_eql_val e s a b = case (a, b) of
  (Set, Set)       -> wnf_eql_set_set e s a b
  (All {}, All {}) -> wnf_eql_all_all e s a b
  (Lam {}, Lam {}) -> wnf_eql_lam_lam e s a b
  (Sig {}, Sig {}) -> wnf_eql_sig_sig e s a b
  (Tup {}, Tup {}) -> wnf_eql_tup_tup e s a b
  (Get {}, Get {}) -> wnf_eql_get_get e s a b
  (Emp, Emp)       -> wnf_eql_emp_emp e s a b
  (Efq, Efq)       -> wnf_eql_efq_efq e s a b
  (Uni, Uni)       -> wnf_eql_uni_uni e s a b
  (One, One)       -> wnf_eql_one_one e s a b
  (Use {}, Use {}) -> wnf_eql_use_use e s a b
  (Bol, Bol)       -> wnf_eql_bol_bol e s a b
  (Fal, Fal)       -> wnf_eql_fal_fal e s a b
  (Tru, Tru)       -> wnf_eql_tru_tru e s a b
  (Fal, Tru)       -> wnf_eql_fal_tru e s a b
  (Tru, Fal)       -> wnf_eql_tru_fal e s a b
  (If {}, If {})   -> wnf_eql_if_if   e s a b
  (Nat, Nat)       -> wnf_eql_nat_nat e s a b
  (Zer, Zer)       -> wnf_eql_zer_zer e s a b
  (Suc {}, Suc {}) -> wnf_eql_suc_suc e s a b
  (Swi {}, Swi {}) -> wnf_eql_swi_swi e s a b
  (Lst {}, Lst {}) -> wnf_eql_lst_lst e s a b
  (Nil, Nil)       -> wnf_eql_nil_nil e s a b
  (Con {}, Con {}) -> wnf_eql_con_con e s a b
  (Mat {}, Mat {}) -> wnf_eql_mat_mat e s a b
  (Nam {}, Nam {}) -> wnf_eql_nam_nam e s a b
  (Dry {}, Dry {}) -> wnf_eql_dry_dry e s a b
  _                -> wnf_eql_default e s a b

