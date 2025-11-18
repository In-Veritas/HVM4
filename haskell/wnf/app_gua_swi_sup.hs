wnf_app_gua_swi_sup :: WnfGuaSwi
wnf_app_gua_swi_sup e s f z sc (Sup l a b) = do
  inc_inters e
  (f0,f1) <- clone e l f
  (z0,z1) <- clone e l z
  (s0,s1) <- clone e l sc
  let app0 = App (Gua f0 (Swi z0 s0)) a
  let app1 = App (Gua f1 (Swi z1 s1)) b
  wnf_enter e s (Sup l app0 app1)

