wnf_app_gua_swi_zer :: WnfGuaSwi
wnf_app_gua_swi_zer e s f z sc Zer = do
  inc_inters e
  wnf_enter e s (Gua (App f Zer) z)

