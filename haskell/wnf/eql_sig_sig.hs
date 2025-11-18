wnf_eql_sig_sig :: WnfEql
wnf_eql_sig_sig e s (Sig aA aB) (Sig bA bB) = do
  inc_inters e
  wnf_enter e s (And (Eql aA bA) (Eql aB bB))

