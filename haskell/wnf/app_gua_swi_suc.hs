wnf_app_gua_swi_suc :: WnfGuaSwi
wnf_app_gua_swi_suc e s f z sc (Suc n) = do
  inc_inters e
  p <- fresh e
  let fn = (Lam p (App f (Suc (Var p))))
  wnf_enter e s (App (Gua fn sc) n)

